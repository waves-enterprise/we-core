package com.wavesenterprise.transaction

import com.google.common.base.Charsets
import com.wavesenterprise.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesenterprise.transaction.smart.SetScriptTransactionV1
import com.wavesenterprise.utils.EitherUtils.EitherExt
import org.scalacheck.Gen
import play.api.libs.json.{Json, _}

class SetScriptTransactionV1Specification extends GenericTransactionSpecification[SetScriptTransactionV1] {

  override def transactionParser: com.wavesenterprise.transaction.TransactionParserFor[SetScriptTransactionV1] = SetScriptTransactionV1

  override def updateProofs(tx: SetScriptTransactionV1, p: Proofs): SetScriptTransactionV1 = {
    tx.copy(proofs = p)
  }

  override def assertTxs(first: SetScriptTransactionV1, second: SetScriptTransactionV1): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.timestamp shouldEqual second.timestamp
    first.name shouldEqual second.name
    first.description shouldEqual second.description
    first.fee shouldEqual second.fee
    first.version shouldEqual second.version
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
    first.script shouldEqual second.script
  }

  override def generator: Gen[((Seq[com.wavesenterprise.transaction.Transaction], SetScriptTransactionV1))] =
    setScriptTransactionGen.map(t => (Seq(), t))

  override def jsonRepr: Seq[(JsValue, SetScriptTransactionV1)] = {
    val tx =
      SetScriptTransactionV1
        .signed(
          PrivateKeyAccount(senderAccount.privateKey, senderAccount.publicKey),
          'T',
          PublicKeyAccount(senderAccount.publicKey),
          None,
          "script".getBytes(Charsets.UTF_8),
          "description".getBytes(Charsets.UTF_8),
          100000L,
          1526983936610L
        )
        .right
        .get
    val proofs = tx.proofs.base58().head
    Seq(
      (Json.parse(s"""{
                       "type": 13,
                       "id": "${tx.id()}",
                       "sender": "${senderAccount.address}",
                       "senderPublicKey": "$senderPkBase58",
                       "name": "script",
                       "description": "description",
                       "fee": 100000,
                       "timestamp": 1526983936610,
                       "proofs": ["$proofs"],
                       "version": 1,
                       "chainId": 84,
                       "script": null
                       }
    """),
       tx))
  }

  override def transactionName: String = "SetScriptTransaction"

  property("SetScriptTransaction id doesn't depend on proof (spec)") {
    forAll(accountGen, proofsGen, proofsGen, scriptGen) {
      case (acc: PrivateKeyAccount, proofs1, proofs2, script) =>
        val tx1 = SetScriptTransactionV1
          .create(
            'T',
            acc,
            Some(script),
            "script".getBytes(Charsets.UTF_8),
            Some("description").getOrElse("").getBytes(Charsets.UTF_8),
            1,
            1,
            proofs1
          )
          .explicitGet()
        val tx2 = SetScriptTransactionV1
          .create(
            'T',
            acc,
            Some(script),
            "script".getBytes(Charsets.UTF_8),
            Some("description").getOrElse("").getBytes(Charsets.UTF_8),
            1,
            1,
            proofs2
          )
          .explicitGet()

        tx1.id() shouldBe tx2.id()
    }
  }
}
