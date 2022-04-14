package com.wavesenterprise.transaction.smart.script

import com.wavesenterprise.crypto
import com.wavesenterprise.lang.ScriptVersion
import com.wavesenterprise.lang.v1.Serde
import com.wavesenterprise.transaction.ValidationError.ScriptParseError
import com.wavesenterprise.transaction.smart.script.v1.ScriptV1

object ScriptReader {

  val checksumLength = 4

  def fromBytes(bytes: Array[Byte]): Either[ScriptParseError, Script] = {
    val checkSum         = bytes.takeRight(checksumLength)
    val computedCheckSum = crypto.secureHash(bytes.dropRight(checksumLength)).take(checksumLength)
    val scriptBytes      = bytes.drop(1).dropRight(checksumLength)

    for {
      version <- Either.cond(bytes.nonEmpty, bytes.head, ScriptParseError("Empty script"))
      _       <- Either.cond(checkSum.sameElements(computedCheckSum), (), ScriptParseError("Invalid checksum"))
      sv <- ScriptVersion
        .fromInt(version)
        .fold[Either[ScriptParseError, ScriptVersion]](Left(ScriptParseError(s"Invalid version: $version")))(v => Right(v))
      script <- ScriptV1
        .validateBytes(scriptBytes)
        .flatMap { _ =>
          Serde.deserialize(scriptBytes).flatMap(ScriptV1(sv, _, checkSize = false))
        }
        .left
        .map(ScriptParseError)
    } yield script
  }

  def fromBytesUnsafe(bytes: Array[Byte]): Script = {
    fromBytes(bytes).explicitGet()
  }

}
