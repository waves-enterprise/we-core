package com.wavesenterprise.transaction

import com.wavesenterprise.account.{AddressScheme, PublicKeyAccount}
import com.wavesenterprise.state._
import com.wavesenterprise.utils.EitherUtils.EitherExt
import com.wavesenterprise.transaction.assets.SetAssetScriptTransactionV1
import com.wavesenterprise.transaction.smart.script.{Script, ScriptCompiler}
import org.scalacheck.Gen
import play.api.libs.json._

class SetAssetScriptTransactionV1Specification extends GenericTransactionSpecification[SetAssetScriptTransactionV1] {
  def transactionParser: com.wavesenterprise.transaction.TransactionParserFor[SetAssetScriptTransactionV1] = SetAssetScriptTransactionV1
  def updateProofs(tx: SetAssetScriptTransactionV1, p: Proofs): SetAssetScriptTransactionV1 = {
    tx.copy(proofs = p)
  }
  def generator: Gen[((Seq[com.wavesenterprise.transaction.Transaction], SetAssetScriptTransactionV1))] = setAssetScriptTransactionGen
  def assertTxs(first: SetAssetScriptTransactionV1, second: SetAssetScriptTransactionV1): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.version shouldEqual second.version
    first.assetId shouldEqual second.assetId
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
    first.script shouldEqual second.script
  }
  def jsonRepr: Seq[(JsValue, SetAssetScriptTransactionV1)] = {
    val tx = SetAssetScriptTransactionV1
      .create(
        AddressScheme.getAddressSchema.chainId,
        PublicKeyAccount(senderAccount.publicKey),
        ByteStr.decodeBase58("DUyJyszsWcmZG7q2Ctk1hisDeGBPB8dEzyU8Gs5V2j3n").get,
        Some(Script.fromBase64String(script).explicitGet()),
        78311891L,
        1868142423132802425L,
        Proofs(
          Seq("5sRtXKcdDa",
              "9Zfe5aw9D7rRR3nvU3QuAjCNT7pdwRXwvBFxHmdt2WtWwiEwffn",
              "",
              "3C",
              "24jboCkAEFrsBKNh6z8FFyJP8YhejsrBwt7JdHVhiCk7DCc3Zxsc4g6PYG8tsLXmK",
              "").map(ByteStr.decodeBase58(_).get))
      )
      .explicitGet()
    Seq(
      (Json.parse(s"""{"type":15,"id":"${tx
         .id()}","sender":"${senderAccount.address}","senderPublicKey":"$senderPkBase58","fee":78311891,"timestamp":1868142423132802425,"proofs":["5sRtXKcdDa","9Zfe5aw9D7rRR3nvU3QuAjCNT7pdwRXwvBFxHmdt2WtWwiEwffn","","3C","24jboCkAEFrsBKNh6z8FFyJP8YhejsrBwt7JdHVhiCk7DCc3Zxsc4g6PYG8tsLXmK",""],"version":1,"chainId":${AddressScheme.getAddressSchema.chainId},"assetId":"DUyJyszsWcmZG7q2Ctk1hisDeGBPB8dEzyU8Gs5V2j3n","script":"$script"}"""),
       tx))
  }

  property("SetAssetScriptTransaction proto serialization roundtrip") {
    forAll(generator) {
      case (_, tx) =>
        val recovered = SetAssetScriptTransactionV1.fromProto(tx.toInnerProto).explicitGet()
        recovered shouldEqual tx
    }
  }

  def transactionName: String = "SetAssetScriptTransaction"

  private val script = {
    ScriptCompiler(s"""
       |
       | let x = 10
       | 20 == x + x
       |
      """.stripMargin,
                   isAssetScript = true).right.get._1.bytes().base64
  }
}
