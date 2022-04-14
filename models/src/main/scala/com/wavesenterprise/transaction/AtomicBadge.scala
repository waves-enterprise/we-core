package com.wavesenterprise.transaction

import com.wavesenterprise.account.Address
import play.api.libs.json.{Format, Json}

case class AtomicBadge(trustedSender: Option[Address] = None)

object AtomicBadge {
  implicit val format: Format[AtomicBadge] = Json.format[AtomicBadge]
}
