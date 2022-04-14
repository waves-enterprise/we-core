package com.wavesenterprise.privacy

import play.api.libs.json.{Format, Json}

case class PolicyInfo(recipients: List[RecipientInfo], owners: List[OwnerInfo])

object PolicyInfo {
  implicit val recipientFormat: Format[RecipientInfo] = Json.format
  implicit val ownerFormat: Format[OwnerInfo]         = Json.format
  implicit val format: Format[PolicyInfo]             = Json.format
}
case class RecipientInfo(address: String, transactionId: String)
case class OwnerInfo(address: String, transactionId: String)

case class PolicyItemInfo(
    sender: String,
    policy: String,
    info: PolicyItemFileInfo,
    hash: String
)
object PolicyItemInfo {
  implicit val format: Format[PolicyItemInfo] = Json.format
}

case class PolicyItemFileInfo(
    filename: String,
    size: Int,
    timestamp: Long,
    author: String,
    comment: String
)
object PolicyItemFileInfo {
  implicit val format: Format[PolicyItemFileInfo] = Json.format
}
