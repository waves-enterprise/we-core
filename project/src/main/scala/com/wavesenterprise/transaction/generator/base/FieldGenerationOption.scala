package com.wavesenterprise.transaction.generator.base

sealed trait FieldGenerationOption extends Product with Serializable

object FieldGenerationOption {

  /**
    * Defines a case class field using the scala constructor notation.
    * @param version – indicates versions of transactions the field will be defined.
    */
  case class InConstructor(version: Int, additionalVersions: Int*) extends FieldGenerationOption {
    def versions: Set[Int] = Set(version) ++ additionalVersions
  }

  /**
    * Defines a case class body field with hardcoded value.
    * @param versionToCode – defines the value of the field depending on the version of the transaction.
    * @param explicitVal – flag indicating whether the field is marked as val.
    */
  case class InBody(versionToCode: PartialFunction[Int, String], explicitVal: Boolean = false, excludeFromSealedTrait: Boolean = false)
      extends FieldGenerationOption

  /**
    * Defines a essential field. These fields have specific code generation.
    */
  case object Essential extends FieldGenerationOption

  /**
    * Defines a field that is excluded from the binary body of the transaction.
    */
  case object ExcludeFromBinaryBody extends FieldGenerationOption

  /**
    * The mark indicating whether the field is marked as overridden.
    */
  case object Override extends FieldGenerationOption

  /**
    * Defines a validated data field. Validation is used when creating the transaction.
    */
  case class Validation(fieldToCode: String => String) extends FieldGenerationOption

  /**
    * Defines a data field with the custom JSON serialization.
    */
  case class Json(fieldToCode: String => String) extends FieldGenerationOption

  /**
    * Defines a data field that is not included in the JSON representation.
    */
  case object NoJson extends FieldGenerationOption

  /**
    * Defines a data field that is not included in the TypeScript representation.
    */
  case object NoTypeScript extends FieldGenerationOption

  /**
    * Defines a limit for TypeScript representation.
    */
  case class TypeScriptLimit(limit: Int) extends FieldGenerationOption

  /**
    * Defines a custom field name for TypeScript representation.
    */
  case class TypeScriptCustomName(name: String) extends FieldGenerationOption

  /**
    * Defines a custom field name for TypeScript representation.
    * Temporal solution. Has to be removed after waves-api refactoring by frond-end.
    */
  case class TypeScriptCustomType(typeName: String) extends FieldGenerationOption

  /**
    * Defines a custom field name for Protobuf representation.
    */
  case class ProtobufCustomName(name: String) extends FieldGenerationOption

  /**
   * Defines a field that is excluded from scala generated classes.
   */
  case object Transparent extends FieldGenerationOption

  /**
   * Defines a data field with proofs serialization.
   */
  case object Proofs extends FieldGenerationOption

}
