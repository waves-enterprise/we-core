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
    private val specialProtoName: Option[String] = None
) {
  def protoName: String              = specialProtoName.getOrElse(name)
  def protoSnakeCaseName: String     = CaseFormat.LOWER_CAMEL.to(CaseFormat.LOWER_UNDERSCORE, protoName)
}
