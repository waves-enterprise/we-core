package com.wavesenterprise.transaction

import com.wavesenterprise.CoreTransactionGen
import com.wavesenterprise.account.{Alias, PrivateKeyAccount, PublicKeyAccount}
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.utils.EitherUtils.EitherExt
import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json

class CreateAliasTransactionV4Specification
    extends PropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with CoreTransactionGen
    with WithSenderAndRecipient {

  property("CreateAliasTransaction serialization roundtrip") {
    forAll(createAliasV4Gen) { tx: CreateAliasTransactionV4 =>
      val recovered = tx.builder.parseBytes(tx.bytes()).get
      recovered shouldEqual tx
    }
  }

  property("CreateAliasTransaction proto serialization roundtrip") {
    forAll(createAliasV4Gen) { tx =>
      val recovered = CreateAliasTransactionV4.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldEqual tx
    }
  }

  property("CreateAliasTransaction serialization from TypedTransaction") {
    forAll(createAliasV4Gen) { tx: CreateAliasTransactionV4 =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered shouldEqual tx
    }
  }

  property("The same aliases from different senders have the same id") {
    forAll(accountGen, accountGen, aliasGen, timestampGen) {
      case (a1: PrivateKeyAccount, a2: PrivateKeyAccount, a: Alias, t: Long) =>
        val tx1 = CreateAliasTransactionV4.selfSigned(a1, a, MinIssueFee, t, None, Some(AtomicBadge(Some(a1.toAddress)))).explicitGet()
        val tx2 = CreateAliasTransactionV4.selfSigned(a2, a, MinIssueFee, t, None, Some(AtomicBadge(Some(a1.toAddress)))).explicitGet()
        tx1.id() shouldBe tx2.id()
    }
  }

  property("JSON format validation for CreateAliasTransactionV4") {
    val tx = CreateAliasTransactionV4
      .create(
        PublicKeyAccount(senderAccount.publicKey),
        Alias.buildWithCurrentChainId("myalias").explicitGet(),
        0L,
        1526910778245L,
        None,
        Some(AtomicBadge(Some(senderAccount.toAddress))),
        Proofs(Seq(ByteStr.decodeBase58("26U7rQTwpdma5GYSZb5bNygVCtSuWL6DKet1Nauf5J57v19mmfnq434YrkKYJqvYt2ydQBUT3P7Xgj5ZVDVAcc5k").get))
      )
      .right
      .get

    val js = Json.parse(s"""{
                       "type": 10,
                       "id": "${tx.id()}",
                       "sender": "${senderAccount.address}",
                       "senderPublicKey": "$senderPkBase58",
                       "fee": 0,
                       "feeAssetId": null,
                       "atomicBadge": { "trustedSender": "${senderAccount.address}"},
                       "timestamp": 1526910778245,
                       "proofs": [
                       "26U7rQTwpdma5GYSZb5bNygVCtSuWL6DKet1Nauf5J57v19mmfnq434YrkKYJqvYt2ydQBUT3P7Xgj5ZVDVAcc5k"
                       ],
                       "version": 4,
                       "alias": "myalias"
                        }
    """)

    js shouldEqual tx.json()
  }

}
