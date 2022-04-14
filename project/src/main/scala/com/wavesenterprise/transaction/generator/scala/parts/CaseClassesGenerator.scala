package com.wavesenterprise.transaction.generator.parts

import com.wavesenterprise.transaction.TxScheme
import com.wavesenterprise.transaction.generator.base._

trait CaseClassesGenerator extends ScalaGenerator {

  override def imports: Set[String] = super.imports ++ Set(
    "com.wavesenterprise.transaction.TransactionParser",
    "com.wavesenterprise.transaction.TransactionParsers",
    "com.google.common.io.ByteArrayDataOutput",
    "com.google.common.io.ByteStreams.newDataOutput",
    "monix.eval.Coeval",
    "play.api.libs.json._"
  )

  override protected def buildWriter(scheme: TxScheme): CodeWriter = {
    super
      .buildWriter(scheme)
      .fold(scheme.supportedVersions) {
        case (writer, version) =>
          writer.call(buildVersionedTxClass(scheme, version))
      }
  }

  private def buildVersionedTxClass(scheme: TxScheme, version: Int): CodeWriter.EndoFunctor = { writer =>
    val className = versionedClassName(scheme, version)
    val currentVersionDataFields =
      scheme.fields.filter(f => f.inConstructorVersions.forall(_.contains(version)) && !f.versionToBodyValue.isDefinedAt(version))
    val currentVersionSpecificDataFields = currentVersionDataFields.filter(!_.isEssential)
    val currentVersionBodyFields         = scheme.fields.filter(_.versionToBodyValue.isDefinedAt(version))
    val currentVersionDataFieldsForProto = EssentialFields.id +: currentVersionDataFields

    val extensions = scheme.entryName +: scheme.versionExtensions.applyOrElse(version, (_: Int) => Seq.empty)

    writer
      .addLines(s"final case class $className private (")
      .foldWithDelimiterIndented(currentVersionDataFields, ",") {
        case (writer, field) =>
          val overrideModifier = if (field.isOverride) "override val " else ""
          writer.addLines(s"$overrideModifier${field.name}: ${field.tpe.scalaType}")
      }
      .addLines(s") extends ${extensions.mkString(" with ")} {")
      .indent
      .newLine
      .fold(currentVersionBodyFields) {
        case (writer, field) =>
          writer.addLines(
            s"${if (field.isOverride) "override " else ""}${if (field.explicitVal) "val" else "def"} ${field.name}: ${field.tpe.scalaType} = ${field
              .versionToBodyValue(version)}")
      }
      .addLines(s"override def version: Byte = $version")
      .newLine
      .addLines(s"def txType: Long = ${scheme.entryName}.typeId")
      .newLine
      .applyIf(scheme.versionToBlockchainFeatures.isDefinedAt(version)) { writer =>
        writer
          .fold(
            Set(
              "com.wavesenterprise.features.BlockchainFeature",
              "scala.collection.SortedSet"
            )
          ) {
            case (writer, i) =>
              writer.addLines(s"import $i")
          }
          .addLines("override val requiredFeatures: SortedSet[BlockchainFeature] = {")
          .foldWithDelimiterIndented("super.requiredFeatures" +: scheme.versionToBlockchainFeatures(version).map(_.seqCode), " ++") {
            case (writer, featureCode) =>
              writer.addLines(featureCode)
          }
          .addLines("}")
          .newLine
      }
      .call(buildJson(scheme, currentVersionSpecificDataFields))
      .call(buildBodyBytes(scheme, currentVersionDataFields))
      .call(buildBytes(scheme, version))
      .call(buildProofSourceBytes(currentVersionDataFields))
      .call(buildToInnerProto(scheme, currentVersionDataFieldsForProto))
      .call(buildToProto(scheme))
      .addLines(s"override def builder: TransactionParser = $className")
      .outdent
      .addLines("}")
      .newLine
  }

  private def buildJson(scheme: TxScheme, currentVersionSpecificFields: Seq[FieldScheme]): CodeWriter.EndoFunctor = { writer =>
    writer
      .addLines(s"override val json: Coeval[JsObject] = Coeval.eval${if (scheme.cacheSerializable) "Once" else ""}(buildJson)")
      .newLine
      .addLines("private def buildJson: JsObject = {")
      .indent
      .addLines("jsonBase() ++ Json.obj(")
      .indent
      .addLines(""""version" -> version,""")
      .fold(scheme.additionalJsonFields) {
        case (writer, (fieldName, fieldCode)) =>
          writer.addLines(s""""$fieldName" -> $fieldCode,""")
      }
      .foldWithDelimiter(currentVersionSpecificFields.filter(_.inJson), ",") {
        case (writer, field) =>
          writer.addLines(s""""${field.name}" -> ${field.fieldToJson.fold(field.name)(_.apply(field.name))}""")
      }
      .outdent
      .addLines(")")
      .outdent
      .addLines("}")
      .newLine
  }

  private def buildBodyBytes(scheme: TxScheme, currentVersionFields: Seq[FieldScheme]): CodeWriter.EndoFunctor = { writer =>
    val bodyFields = currentVersionFields.filter(_.inBinaryBody)

    writer
      .addLines(s"override val bodyBytes: Coeval[Array[Byte]] = Coeval.eval${if (scheme.cacheSerializable) "Once" else ""} {")
      .addStringMarginIndented {
        """ val output = newDataOutput()
          | writeBodyBytes(output)
          | output.toByteArray"""
      }
      .addLines("}")
      .newLine
      .addLines("protected def writeBodyBytes(output: ByteArrayDataOutput, forProof: Boolean = false): Unit = {")
      .indent
      .addLines("output.writeByte(builder.typeId)")
      .addLines("output.writeByte(version)")
      .fold(bodyFields) {
        case (writer, field) =>
          field.tpe match {
            case tpe: BinarySerializableType if tpe.isCustomProofSource =>
              writer
                .addLines("if (forProof) {")
                .addLinesIndented(tpe.binaryWriter(BinarySerializationContext("output", field.name, forProof = true)))
                .addLines("} else {")
                .addLinesIndented(tpe.binaryWriter(BinarySerializationContext("output", field.name)))
                .addLines("}")
            case tpe: BinarySerializableType =>
              writer.addLines(tpe.binaryWriter(BinarySerializationContext("output", field.name)))
            case _ =>
              writer.addLines(s"{Field ${field.tpe} not implemented BinarySerializableType}")
          }
      }
      .outdent
      .addLines("}")
      .newLine
  }

  private def buildBytes(scheme: TxScheme, version: Int): CodeWriter.EndoFunctor = { writer =>
    writer
      .addLines(s"override val bytes: Coeval[Array[Byte]] = Coeval.eval${if (scheme.cacheSerializable) "Once" else ""} {")
      .addStringMarginIndented {
        s""" val output = newDataOutput()
          | ${scheme.versionToBinaryHeaderType(version) match {
          case BinaryHeaderType.Legacy => ""
          case BinaryHeaderType.Modern => "output.writeByte(TransactionParsers.ModernTxFlag)"
        }}
          | writeBodyBytes(output)
          | proofs.writeBytes(output)
          | output.toByteArray"""
      }
      .addLines("}")
      .newLine
  }

  private def buildProofSourceBytes(currentVersionFields: Seq[FieldScheme]): CodeWriter.EndoFunctor = { writer =>
    writer.applyIf(currentVersionFields.exists(_.tpe.isCustomProofSource)) {
      _.addLines("override def proofSourceBytes: Array[Byte] = {")
        .addStringMarginIndented {
          """val output = newDataOutput()
          |writeBodyBytes(output, forProof = true)
          |output.toByteArray"""
        }
        .addLines("}")
        .newLine
    }
  }

  private val senderAddressFieldScheme = FieldScheme(name = "sender.toAddress", specialProtoName = Some("senderAddress"), tpe = FieldType.ADDRESS)

  private def buildToInnerProto(scheme: TxScheme, currentVersionFields: Seq[FieldScheme]): CodeWriter.EndoFunctor = { writer =>
    writer
      .addLines(s"override def toInnerProto: Pb${scheme.entryName} = {")
      .indent
      .addLines(s"Pb${scheme.entryName}(")
      .foldWithDelimiterIndented(currentVersionFields :+ senderAddressFieldScheme, ",") {
        case (writer, field) =>
          field.tpe match {
            case tpe: ProtoCompatibleType =>
              writer.addLines {
                tpe.vanillaToProtoAdapter.fold(s"${field.protoName} = ${field.name}") { adapter =>
                  if (tpe.isMessageProtoType) {
                    s"""${field.name} = Some {
                       |  ${adapter(ProtoAdaptationContext(field.protoName))}
                       |}""".stripMargin
                  } else {
                    s"${field.protoName} = ${adapter(ProtoAdaptationContext(field.name))}"
                  }
                }
              }
            case _ =>
              writer.addLines(s"{Field ${field.tpe} not implemented ProtoCompatibleType}")
          }
      }
      .addLines(")")
      .outdent
      .addLines("}")
      .newLine
  }

  private def buildToProto(scheme: TxScheme): CodeWriter.EndoFunctor = { writer =>
    writer
      .addLines(s"override def toProto: PbTransaction = {")
      .indent
      .addLines(s"val innerTx = PbTransaction.Transaction.${scheme.entryName}(toInnerProto)")
      .addLines(s"PbTransaction(version, innerTx)")
      .outdent
      .addLines("}")
      .newLine
  }

  private def versionedClassName(scheme: TxScheme, version: Int) = {
    s"${scheme.entryName}V$version"
  }
}
