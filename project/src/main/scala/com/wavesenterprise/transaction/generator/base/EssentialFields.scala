package com.wavesenterprise.transaction.generator.base

import com.wavesenterprise.transaction.generator.base.FieldDefinitionSyntax._
import com.wavesenterprise.transaction.generator.base.FieldGenerationOption.{Essential, ExcludeFromBinaryBody, ProtobufCustomName}
import com.wavesenterprise.transaction.generator.base.FieldType.{LONG, PROOFS, PUBLIC_KEY_ACCOUNT, TRANSACTION_ID}

object EssentialFields {
  val id             = "id"        as TRANSACTION_ID     -> Set(Essential, ExcludeFromBinaryBody)
  val senderField    = "sender"    as PUBLIC_KEY_ACCOUNT -> Set(Essential, ProtobufCustomName("senderPublicKey"))
  val feeField       = "fee"       as LONG               -> Essential
  val timestampField = "timestamp" as LONG               -> Essential
  val proofsField    = "proofs"    as PROOFS             -> Set(Essential, ExcludeFromBinaryBody)
}
