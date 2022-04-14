package com.wavesenterprise.transaction

import com.google.common.primitives.Shorts
import com.wavesenterprise.CoreTransactionGen
import com.wavesenterprise.account.PublicKeyAccount
import com.wavesenterprise.state._
import com.wavesenterprise.utils.Base64
import com.wavesenterprise.utils.EitherUtils.EitherExt
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json

class DataTransactionV1Specification
    extends PropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with CoreTransactionGen
    with WithSenderAndRecipient {

  import DataTransactionEntryOps._

  private def checkSerialization(tx: DataTransactionV1): Assertion = {
    val parsed = DataTransactionV1.parseBytes(tx.bytes()).get
    checkEquals(tx, parsed)
  }

  private def checkEquals(source: DataTransactionV1, parsed: DataTransactionV1): Assertion = {
    parsed.sender.address shouldEqual source.sender.address
    parsed.timestamp shouldEqual source.timestamp
    parsed.fee shouldEqual source.fee

    parsed.data.zip(source.data).foreach {
      case (r, t) =>
        r.key shouldEqual t.key
        r.value shouldEqual t.value
    }

    parsed.bytes() shouldEqual source.bytes()
  }

  property("serialization roundtrip") {
    forAll(dataTransactionV1Gen)(checkSerialization)
  }

  property("proto serialization roundtrip") {
    forAll(dataTransactionV1Gen) { tx =>
      val recovered = DataTransactionV1.fromProto(tx.toInnerProto).explicitGet()
      checkEquals(tx, recovered)
    }
  }

  property("serialization from TypedTransaction") {
    forAll(dataTransactionV1Gen) { tx: DataTransactionV1 =>
      val recovered = DataTransactionV1.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  ignore("unknown type handing") {
    val badTypeIdGen = Gen.choose[Int](DataEntry.Type.maxId + 1, Byte.MaxValue)
    forAll(dataTransactionV1Gen, badTypeIdGen) {
      case (tx, badTypeId) =>
        val bytes      = tx.bytes()
        val entryCount = Shorts.fromByteArray(bytes.drop(67))
        if (entryCount > 0) {
          /*val key1Length = Shorts.fromByteArray(bytes.drop(69))
          val p          = 71 + key1Length
          bytes(p) = badTypeId.toByte*/
          val key1Length = Shorts.fromByteArray(bytes.drop(37))
          val p          = 39 + key1Length
          bytes(p) = badTypeId.toByte
          val parsed = DataTransactionV1.parseBytes(bytes)
          parsed.isFailure shouldBe true
          parsed.failed.get.getMessage shouldBe s"Unknown type $badTypeId"
        }
    }
  }

  //TODO: fix this test case
  ignore("positive validation cases") {
    import com.wavesenterprise.state._
    import com.wavesenterprise.transaction.validation.DataValidation.MaxEntryCount
    forAll(dataTransactionV1Gen, dataEntryGen(500)) {
      case (DataTransactionV1(sender, author, data, fee, timestamp, proofs), entry) =>
        def check(data: List[DataEntry[_]]): Assertion = {
          val txEi = DataTransactionV1.create(sender, author, data, timestamp, fee, proofs)
          txEi shouldBe Right(DataTransactionV1(sender, author, data, fee, timestamp, proofs))
          checkSerialization(txEi.explicitGet())
        }

        check(List.empty)                                                               // no data
        check(List.tabulate(MaxEntryCount)(n => IntegerDataEntry(n.toString, n)))       // maximal data
        check(List.tabulate(30)(n => StringDataEntry(n.toString, "a" * 5109)))          // maximal data
        check(List(IntegerDataEntry("a" * MaxKeySize, 0xa)))                            // max key size
        check(List(BinaryDataEntry("bin", ByteStr.empty)))                              // empty binary
        check(List(BinaryDataEntry("bin", ByteStr(Array.fill(MaxValueSize)(1: Byte))))) // max binary value size
        check(List(StringDataEntry("str", "")))                                         // empty string
        check(List(StringDataEntry("str", "A" * MaxValueSize))) // max string size
    }
  }

  property("negative validation cases") {
    val badVersionGen = Arbitrary.arbByte.arbitrary.filter(v => !DataTransactionV1.supportedVersions.contains(v))
    forAll(dataTransactionV1Gen, badVersionGen) {
      case (DataTransactionV1(sender, author, data, fee, timestamp, proofs), badVersion) =>
        val dataTooBig   = List.tabulate(100)(n => StringDataEntry((100 + n).toString, "a" * 1527))
        val dataTooBigEi = DataTransactionV1.create(sender, author, dataTooBig, timestamp, fee, proofs)
        dataTooBigEi shouldBe Left(ValidationError.TooBigArray)

        val emptyKey   = List(IntegerDataEntry("", 2))
        val emptyKeyEi = DataTransactionV1.create(sender, author, emptyKey, timestamp, fee, proofs)
        emptyKeyEi shouldBe Left(ValidationError.GenericError("Empty key found"))

        val keyTooLong   = data :+ BinaryDataEntry("a" * (MaxKeySize + 1), ByteStr(Array(1, 2)))
        val keyTooLongEi = DataTransactionV1.create(sender, author, keyTooLong, timestamp, fee, proofs)
        keyTooLongEi shouldBe Left(ValidationError.TooBigArray)

        val valueTooLong   = data :+ BinaryDataEntry("key", ByteStr(Array.fill(MaxValueSize + 1)(1: Byte)))
        val valueTooLongEi = DataTransactionV1.create(sender, author, valueTooLong, timestamp, fee, proofs)
        valueTooLongEi shouldBe Left(ValidationError.TooBigArray)

        val e               = BooleanDataEntry("dupe", true)
        val duplicateKeys   = e +: data.drop(3) :+ e
        val duplicateKeysEi = DataTransactionV1.create(sender, author, duplicateKeys, timestamp, fee, proofs)
        duplicateKeysEi shouldBe Left(ValidationError.GenericError("Duplicate keys found"))
    }
  }

  property(testName = "JSON format validation") {
    val entry1 = IntegerDataEntry("int", 24)
    val entry2 = BooleanDataEntry("bool", true)
    val entry3 = BinaryDataEntry("blob", ByteStr(Base64.decode("YWxpY2U=").get))
    val tx = DataTransactionV1
      .create(
        PublicKeyAccount(senderAccount.publicKey),
        PublicKeyAccount(senderAccount.publicKey),
        List(entry1, entry2, entry3),
        1526911531530L,
        100000,
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
                       "timestamp": 1526911531530,
                       "proofs": [
                       "32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94"
                       ],
                       "version": 1,
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
