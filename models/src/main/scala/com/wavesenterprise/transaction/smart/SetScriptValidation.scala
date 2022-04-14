package com.wavesenterprise.transaction.smart

import com.wavesenterprise.lang.ScriptVersion
import com.wavesenterprise.transaction.ValidationError

import java.nio.charset.StandardCharsets

object SetScriptValidation {
  val MaxNameSize: Int                            = 128 // in symbols
  val MaxDescriptionSizeInBytes: Short            = Short.MaxValue
  val SupportedScriptVersions: Seq[ScriptVersion] = ScriptVersion.Versions.V1 :: ScriptVersion.Versions.V2 :: Nil

  def validateName(name: Array[Byte]): Either[ValidationError, Array[Byte]] =
    Either.cond(
      name.nonEmpty && new String(name, StandardCharsets.UTF_8).length <= MaxNameSize,
      name,
      ValidationError.GenericError(s"name field length must be between 1 and $MaxNameSize")
    )

  def validateDescription(description: Array[Byte]): Either[ValidationError, Array[Byte]] =
    Either.cond(
      description.isEmpty || description.length <= MaxDescriptionSizeInBytes,
      description,
      ValidationError.GenericError(s"description field length in bytes must be less $MaxDescriptionSizeInBytes")
    )
}
