package com.wavesenterprise.transaction

import org.scalacheck.Gen
import com.google.common.primitives.Shorts
import com.wavesenterprise.CoreTransactionGen
import com.wavesenterprise.account.PublicKeyAccount
import com.wavesenterprise.state._
import com.wavesenterprise.utils.Base64
import com.wavesenterprise.utils.EitherUtils.EitherExt
import org.scalacheck.Arbitrary
import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json

class DataTransactionV2Specification
    extends PropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with CoreTransactionGen
    with WithSenderAndRecipient {

  import DataTransactionEntryOps._

  private def checkSerialization(tx: DataTransactionV2): Assertion = {
    val parsed = DataTransactionV2.parseBytes(tx.bytes()).get

    parsed.sender.address shouldEqual tx.sender.address
    parsed.timestamp shouldEqual tx.timestamp
    parsed.fee shouldEqual tx.fee

    parsed.data.zip(tx.data).foreach {
      case (r, t) =>
        r.key shouldEqual t.key
        r.value shouldEqual t.value
    }

    parsed.bytes() shouldEqual tx.bytes()
  }

  property("serialization roundtrip") {
    forAll(dataTransactionV2Gen)(checkSerialization)
  }

  property("proto serialization roundtrip") {
    forAll(dataTransactionV2Gen) { tx =>
      val recovered = DataTransactionV2.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldEqual tx
    }
  }

  property("serialization from TypedTransaction") {
    forAll(dataTransactionV2Gen) { tx: DataTransactionV2 =>
      val recovered = DataTransactionV2.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("unknown type handing") {
    val badTypeIdGen = Gen.choose[Int](DataEntry.Type.maxId + 1, Byte.MaxValue)
    forAll(dataTransactionV2Gen, badTypeIdGen) {
      case (tx, badTypeId) =>
        val bytes = tx.bytes()
        /*
         * transaction Data Transaction Binary Format in bytes:
         *    1    +     1       +      1       +       32      +     32     +     2         +       2       +     <400        +     1      +   ...
         * Version   Transaction    Transaction      Public key    Public key    Length of         Key 1           Key 1           Value 1
         *  flag       type ID       version           sender        author    the data array      length                           type
         * The maximum size of transaction body bytes is 153,600 bytes.
         * */
        val entryCount = Shorts.fromByteArray(bytes.drop(67))
        if (entryCount > 0) {
          val key1Length = Shorts.fromByteArray(bytes.drop(69))
          val p          = 71 + key1Length
          bytes(p) = badTypeId.toByte
          val parsed = DataTransactionV2.parseBytes(bytes)
          parsed.isFailure shouldBe true
          parsed.failed.get.getMessage shouldBe s"Unknown type $badTypeId"
        }
    }
  }

  property("positive validation cases") {
    import com.wavesenterprise.state._
    import com.wavesenterprise.transaction.validation.DataValidation.MaxEntryCount
    forAll(dataTransactionV2Gen) {
      case (tx) =>
        def check(data: List[DataEntry[_]]): Assertion = {
          val txEi = DataTransactionV2.create(tx.sender, tx.author, data, tx.timestamp, tx.fee, tx.feeAssetId, tx.proofs)
          txEi shouldBe Right(DataTransactionV2(tx.sender, tx.author, data, tx.timestamp, tx.fee, tx.feeAssetId, tx.proofs))
          checkSerialization(txEi.explicitGet())
        }

        check(List.empty)                                                               // no data
        check(List.tabulate(MaxEntryCount)(n => IntegerDataEntry(n.toString, n)))       // maximal data
        check(List.tabulate(30)(n => StringDataEntry(n.toString, "a" * 5107)))          // The maximum size of transaction body bytes is ~153,600 bytes.
        check(List(IntegerDataEntry("a" * (MaxKeySize - 1), 0xa)))                      // max key size
        check(List(BinaryDataEntry("bin", ByteStr.empty)))                              // empty binary
        check(List(BinaryDataEntry("bin", ByteStr(Array.fill(MaxValueSize)(1: Byte))))) // max binary value size
        check(List(StringDataEntry("str", "")))                                         // empty string
        check(List(StringDataEntry("str", "A" * MaxValueSize)))                         // max string size
    }
  }

  property("negative validation cases") {
    val badVersionGen = Arbitrary.arbByte.arbitrary.filter(v => !DataTransactionV2.supportedVersions.contains(v))
    forAll(dataTransactionV2Gen, badVersionGen) {
      case (DataTransactionV2(sender, author, data, fee, timestamp, feeAssetId, proofs), badVersion) =>
        val dataTooBig   = List.tabulate(100)(n => StringDataEntry((100 + n).toString, "a" * 1527))
        val dataTooBigEi = DataTransactionV2.create(sender, author, dataTooBig, timestamp, fee, feeAssetId, proofs)
        dataTooBigEi shouldBe Left(ValidationError.TooBigArray)

        val emptyKey   = List(IntegerDataEntry("", 2))
        val emptyKeyEi = DataTransactionV2.create(sender, author, emptyKey, timestamp, fee, feeAssetId, proofs)
        emptyKeyEi shouldBe Left(ValidationError.GenericError("Empty key found"))

        val keyTooLong   = data :+ BinaryDataEntry("a" * (MaxKeySize + 1), ByteStr(Array(1, 2)))
        val keyTooLongEi = DataTransactionV2.create(sender, author, keyTooLong, timestamp, fee, feeAssetId, proofs)
        keyTooLongEi shouldBe Left(ValidationError.TooBigArray)

        val valueTooLong   = data :+ BinaryDataEntry("key", ByteStr(Array.fill(MaxValueSize + 1)(1: Byte)))
        val valueTooLongEi = DataTransactionV2.create(sender, author, valueTooLong, timestamp, fee, feeAssetId, proofs)
        valueTooLongEi shouldBe Left(ValidationError.TooBigArray)

        val e               = BooleanDataEntry("dupe", true)
        val duplicateKeys   = e +: data.drop(3) :+ e
        val duplicateKeysEi = DataTransactionV2.create(sender, author, duplicateKeys, timestamp, fee, feeAssetId, proofs)
        duplicateKeysEi shouldBe Left(ValidationError.GenericError("Duplicate keys found"))
    }
  }

  property(testName = "JSON format validation") {
    val entry1 = IntegerDataEntry("int", 24)
    val entry2 = BooleanDataEntry("bool", true)
    val entry3 = BinaryDataEntry("blob", ByteStr(Base64.decode("YWxpY2U=").get))
    val tx = DataTransactionV2
      .create(
        PublicKeyAccount(senderAccount.publicKey),
        PublicKeyAccount(senderAccount.publicKey),
        List(entry1, entry2, entry3),
        1526911531530L,
        100000,
        None,
        Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
      )
      .right
      .get

    val js = Json.parse(s"""{
                       "type": 12,
                       "id": "${tx.id()}",
                       "sender": "${senderAccount.address}",
                       "senderPublicKey": "$senderPkBase58",
                       "authorPublicKey": "$senderPkBase58",
                       "author": "${senderAccount.address}",
                       "fee": 100000,
                       "feeAssetId": null,
                       "timestamp": 1526911531530,
                       "proofs": [
                       "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                       ],
                       "version": 2,
                       "data": [
                       {
                       "key": "int",
                       "type": "integer",
                       "value": 24
                       },
                       {
                       "key": "bool",
                       "type": "boolean",
                       "value": true
                       },
                       {
                       "key": "blob",
                       "type": "binary",
                       "value": "base64:YWxpY2U="
                       }
                       ]
                       }
  """)

    js shouldEqual tx.json()
  }

}
