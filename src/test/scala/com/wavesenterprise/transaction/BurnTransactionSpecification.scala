package com.wavesenterprise.transaction

import com.wavesenterprise.CoreTransactionGen
import com.wavesenterprise.account.PublicKeyAccount
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.assets.{BurnTransaction, BurnTransactionV2}
import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json
import com.wavesenterprise.utils.EitherUtils.EitherExt

class BurnTransactionSpecification extends PropSpec with ScalaCheckPropertyChecks with Matchers with CoreTransactionGen with WithSenderAndRecipient {

  property("Burn serialization roundtrip") {
    forAll(burnGen) { issue: BurnTransaction =>
      val recovered = issue.builder.parseBytes(issue.bytes()).get
      recovered.bytes() shouldEqual issue.bytes()
    }
  }

  property("Burn proto serialization roundtrip") {
    forAll(burnGen) { tx =>
      val recovered = BurnTransactionV2.fromProto(tx.toInnerProto).explicitGet()
      recovered shouldEqual tx
    }
  }

  property("Burn serialization from TypedTransaction") {
    forAll(burnGen) { issue: BurnTransaction =>
      val recovered = TransactionParsers.parseBytes(issue.bytes()).get
      recovered.bytes() shouldEqual issue.bytes()
    }
  }

  property("JSON format validation for BurnTransactionV2") {
    val tx = BurnTransactionV2
      .create(
        'T',
        PublicKeyAccount(senderAccount.publicKey),
        ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get,
        10000000000L,
        100000000L,
        1526287561757L,
        Proofs(Seq(ByteStr.decodeBase58("3NcEv6tcVMuXkTJwiqW4J3GMCTe8iSLY7neEfNZonp59eTQEZXYPQWs565CRUctDrvcbtmsRgWvnN7BnFZ1AVZ1H").get))
      )
      .right
      .get

    val js = Json.parse(s"""{
                       "type": 6,
                       "id": "${tx.id()}",
                       "sender": "${senderAccount.address}",
                       "senderPublicKey": "$senderPkBase58",
                       "fee": 100000000,
                       "timestamp": 1526287561757,
                       "proofs": [
                       "3NcEv6tcVMuXkTJwiqW4J3GMCTe8iSLY7neEfNZonp59eTQEZXYPQWs565CRUctDrvcbtmsRgWvnN7BnFZ1AVZ1H"
                       ],
                       "chainId": 84,
                       "version": 2,
                       "assetId": "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz",
                       "amount": 10000000000
                    }
    """)

    js shouldEqual tx.json()
  }

}
