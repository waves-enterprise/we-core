package com.wavesenterprise.transaction.generator.base

import com.wavesenterprise.transaction.generator.base.FieldGenerationOption._

object FieldDefinitionSyntax {

  implicit class ExtendedString(private val name: String) extends AnyVal {

    def as(`type`: FieldType): FieldScheme = FieldScheme(name, `type`)

    def as(typeWithOption: (FieldType, FieldGenerationOption)): FieldScheme = {
      val (tpe, option) = typeWithOption
      as(tpe, Set(option))
    }

    def as(typeWithOptions: (FieldType, Set[FieldGenerationOption]))(implicit d: DummyImplicit): FieldScheme = {
      val (tpe, options) = typeWithOptions

      val maybeIsEssential           = options.collectFirst { case Essential                     => () }
      val maybeExcludeFromBody       = options.collectFirst { case ExcludeFromBinaryBody         => () }
      val maybeIsOverride            = options.collectFirst { case Override                      => () }
      val maybeExcludeFromJson       = options.collectFirst { case NoJson                        => () }
      val maybeExcludeFromTypeScript = options.collectFirst { case NoTypeScript                  => () }
      val maybeInConstructor         = options.collectFirst { case inConstructor: InConstructor  => inConstructor.versions }
      val maybeValidation            = options.collectFirst { case validation: Validation        => validation.fieldToCode }
      val maybeInBody                = options.collectFirst { case inBody: InBody                => inBody }
      val maybeCustomJson            = options.collectFirst { case json: Json                    => json.fieldToCode }
      val typeScriptLimit            = options.collectFirst { case TypeScriptLimit(tsLimit)      => tsLimit }
      val specialTypeScriptName      = options.collectFirst { case TypeScriptCustomName(tsName)  => tsName }
      val specialTypeScriptType      = options.collectFirst { case TypeScriptCustomType(tsType)  => tsType }
      val specialProtoName           = options.collectFirst { case ProtobufCustomName(protoName) => protoName }
      val maybeIsTransparent         = options.collectFirst { case Transparent                   => () }
      val maybeIsProofs              = options.collectFirst { case Proofs                        => () }

      FieldScheme(
        name = name,
        tpe = tpe,
        inConstructorVersions = maybeInConstructor,
        inBinaryBody = maybeExcludeFromBody.isEmpty,
        isEssential = maybeIsEssential.isDefined,
        isOverride = maybeIsOverride.isDefined,
        fieldToValidation = maybeValidation,
        versionToBodyValue = maybeInBody.map(_.versionToCode).getOrElse(PartialFunction.empty),
        explicitVal = maybeInBody.fold(false)(_.explicitVal),
        fieldToJson = maybeCustomJson,
        inJson = maybeExcludeFromJson.isEmpty,
        excludeFormSealedTrait = maybeInBody.exists(_.excludeFromSealedTrait),
        typeScriptLimit = typeScriptLimit,
        inTypeScript = maybeExcludeFromTypeScript.isEmpty,
        specialTypeScriptName = specialTypeScriptName,
        specialTypeScriptType = specialTypeScriptType,
        specialProtoName = specialProtoName,
        isTransparent = maybeIsTransparent.isDefined,
        isProofs = maybeIsProofs.isDefined
      )
    }
  }
}
