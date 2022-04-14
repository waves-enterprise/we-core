package com.wavesenterprise.transaction

import play.api.libs.json._

trait ProvenTransaction extends Transaction with Proven {

  def proofField = Json.obj("proofs" -> proofs.proofs.map(_.base58))

  protected def jsonBase(): JsObject =
    Json.obj(
      "type"            -> builder.typeId,
      "id"              -> id().base58,
      "sender"          -> sender.address,
      "senderPublicKey" -> sender.publicKeyBase58,
      "fee"             -> fee,
      "timestamp"       -> timestamp,
    ) ++ proofField
}
