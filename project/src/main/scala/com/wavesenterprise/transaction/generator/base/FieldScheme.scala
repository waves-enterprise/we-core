package com.wavesenterprise.transaction.generator.base

import com.google.common.base.CaseFormat

case class FieldScheme(
    name: String,
    tpe: FieldType,
    inConstructorVersions: Option[Set[Int]] = None,
    inBinaryBody: Boolean = true,
    isEssential: Boolean = false,
    isOverride: Boolean = false,
    fieldToValidation: Option[String => String] = None,
    versionToBodyValue: PartialFunction[Int, String] = PartialFunction.empty,
    explicitVal: Boolean = false,
    fieldToJson: Option[String => String] = None,
    inJson: Boolean = true,
    excludeFormSealedTrait: Boolean = false,
    typeScriptLimit: Option[Int] = None,
    private val inTypeScript: Boolean = true,
    private val specialTypeScriptName: Option[String] = None,
    private val specialTypeScriptType: Option[String] = None,
    private val specialProtoName: Option[String] = None,
    isTransparent: Boolean = false,
    isProofs: Boolean = false
) {
  def typeScriptName: String         = specialTypeScriptName.getOrElse(name)
  def typeScriptType: Option[String] = specialTypeScriptType.orElse(tpe.typeScriptType).filter(_ => inTypeScript)
  def protoName: String              = specialProtoName.getOrElse(name)
  def protoSnakeCaseName: String     = CaseFormat.LOWER_CAMEL.to(CaseFormat.LOWER_UNDERSCORE, protoName)
}
