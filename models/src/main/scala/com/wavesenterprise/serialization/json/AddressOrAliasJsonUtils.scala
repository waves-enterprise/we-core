package com.wavesenterprise.serialization.json

import com.wavesenterprise.account.AddressOrAlias
import com.wavesenterprise.utils.Base58
import play.api.libs.json._

object AddressOrAliasJsonUtils {
  implicit val jsonFormat: Format[AddressOrAlias] = Format[AddressOrAlias](
    Reads {
      case JsString(str) =>
        Base58
          .decode(str)
          .toEither
          .flatMap(AddressOrAlias.fromBytes(_, 0))
          .map { case (x, _) => JsSuccess(x) }
          .getOrElse(JsError("Can't read PublicKeyAccount"))

      case _ => JsError("Can't read PublicKeyAccount")
    },
    Writes(x => JsString(x.bytes.base58))
  )
}
