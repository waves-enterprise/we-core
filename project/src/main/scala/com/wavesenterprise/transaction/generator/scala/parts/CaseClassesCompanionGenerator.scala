package com.wavesenterprise.transaction.generator.parts

import com.wavesenterprise.transaction.TxScheme
import com.wavesenterprise.transaction.generator.base.EssentialFields._
import com.wavesenterprise.transaction.generator.base._

trait CaseClassesCompanionGenerator extends ScalaGenerator {

  override def imports: Set[String] = super.imports ++ Set(
    "com.wavesenterprise.transaction.TransactionParserFor",
    "com.wavesenterprise.transaction.ValidationError",
    "com.wavesenterprise.account.PrivateKeyAccount",
    "com.wavesenterprise.crypto",
    "com.wavesenterprise.state.ByteStr",
    "scala.util.Try"
  )

  override protected def buildWriter(scheme: TxScheme): CodeWriter = {
    super
      .buildWriter(scheme)
      .fold(scheme.supportedVersions) {
        case (writer, version) =>
          writer.call(buildVersionedTxClassCompanion(scheme, version))
      }
  }

  private def buildVersionedTxClassCompanion(scheme: TxScheme, version: Int): CodeWriter.EndoFunctor = { writer =>
    val className = versionedClassName(scheme, version)
    val currentVersionDataFields = scheme.fields.filter { field =>
      field.inConstructorVersions.forall(_.contains(version)) && // Include field if it belongs to at least one version
      !field.versionToBodyValue.isDefinedAt(version) && // Exclude hardcoded fields
      !field.isTransparent
    }

    val extensions = Seq(
      s"TransactionParserFor[$className]",
      scheme.versionToBinaryHeaderType(version).scalaTrait
    ) ++ scheme.caseClassCompanionExtensions

    writer
      .addLines(s"object $className extends ${extensions.mkString(" with ")} {")
      .indent
      .newLine
      .addLines(s"override val typeId: Byte = ${scheme.entryName}.typeId")
      .applyIf(scheme.versionToBinaryHeaderType(version) == BinaryHeaderType.Legacy) {
        _.addLines(s"override val version: Byte = $version")
      }
      .newLine
      .addLines(s"override def supportedVersions: Set[Byte] = Set($version)")
      .newLine
      .call(buildParseTail(className, currentVersionDataFields))
      .call(buildCreate(scheme, className, currentVersionDataFields))
      .call(buildSigned(scheme, className, currentVersionDataFields))
      .call(buildSelfSigned(className, currentVersionDataFields))
      .call(buildFromProto(scheme, className, currentVersionDataFields))
      .outdent
      .addLines("}")
      .newLine
  }

  private def buildParseTail(className: String, currentVersionFields: Seq[FieldScheme]): CodeWriter.EndoFunctor = { writer =>
    writer
      .addLines(s"override def parseTail(version: Byte, bytes: Array[Byte], offset: Int): Try[$className] = {")
      .indent
      .addLines("Try {")
      .indent
      .foldWithAccumulator(currentVersionFields, "offset") {
        case ((writer, offsetField), field) =>
          field.tpe match {
            case tpe: BinarySerializableType =>
              writer.addLines(tpe.binaryReader(BinaryDeserializationContext("bytes", offsetField, field.name))) -> (field.name + "End")
            case _ =>
              writer.addLines(s"{Field type ${field.tpe} not implemented BinarySerializableType}") -> (field.name + "End")
          }
      }
      .addLines(s"$className(${currentVersionFields.map(_.name).mkString(", ")})")
      .outdent
      .addLines("}")
      .outdent
      .addLines("}")
      .newLine
  }

  private def buildCreate(scheme: TxScheme, className: String, currentVersionFields: Seq[FieldScheme]): CodeWriter.EndoFunctor = { writer =>
    val fieldsWithValidation = currentVersionFields.filter(_.fieldToValidation.nonEmpty)

    writer
      .addLines(s"def create(")
      .foldWithDelimiterIndented(currentVersionFields, ",") {
        case (writer, field) =>
          writer.addLines(s"${field.name}: ${field.tpe}")
      }
      .addLines(s"): Either[ValidationError, $className] = {")
      .indent
      .applyIf(fieldsWithValidation.isEmpty && scheme.ensures.isEmpty) { writer =>
        writer.addLines(s"Right($className(${currentVersionFields.map(_.name).mkString(", ")}))")
      }
      .applyIf(fieldsWithValidation.nonEmpty || scheme.ensures.nonEmpty) { writer =>
        writer
          .addLines("for {")
          .indent
          .fold(fieldsWithValidation) {
            case (writer, field) =>
              writer.addLines(s"_ <- ${field.fieldToValidation.map(_.apply(field.name)).getOrElse("")}")
          }
          .addLines(s"resultTx = $className(${currentVersionFields.map(_.name).mkString(", ")})")
          .fold(scheme.ensures) {
            case (writer, ensure) =>
              writer.addLines(s"_ <- $ensure(resultTx)")
          }
          .outdent
          .addLines(s"} yield resultTx")
      }
      .outdent
      .addLines("}")
      .newLine
  }

  private def buildSigned(scheme: TxScheme, className: String, currentVersionFields: Seq[FieldScheme]): CodeWriter.EndoFunctor = { writer =>
    writer
      .addLines(s"def signed(")
      .indent
      .addLines("signer: PrivateKeyAccount,")
      .foldWithDelimiter(currentVersionFields.filter(_.name != proofsField.name), ",") {
        case (writer, field) =>
          writer.addLines(s"${field.name}: ${field.tpe}")
      }
      .outdent
      .addLines(s"): Either[ValidationError, $className] = {")
      .indent
      .addLines("for {")
      .indent
      .addLines(s"unsigned <- create(${currentVersionFields.map(f => if (f.name == proofsField.name) "Proofs.empty" else f.name).mkString(", ")})")
      .addLines(s"proofs   <- Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.proofSourceBytes))))")
      .addLines("resultTx = unsigned.copy(proofs = proofs)")
      .fold(scheme.ensures) {
        case (writer, ensure) =>
          writer.addLines(s"_ <- $ensure(resultTx)")
      }
      .outdent
      .addLines(s"} yield resultTx")
      .outdent
      .addLines("}")
      .newLine
  }

  private def buildSelfSigned(className: String, currentVersionFields: Seq[FieldScheme]): CodeWriter.EndoFunctor = { writer =>
    writer
      .addLines(s"def selfSigned(")
      .foldWithDelimiterIndented(currentVersionFields.filter(_.name != proofsField.name), ",") {
        case (writer, field) =>
          val tpe = if (field.name == senderField.name) "PrivateKeyAccount" else field.tpe
          writer.addLines(s"${field.name}: $tpe")
      }
      .addLines(s"): Either[ValidationError, $className] = {")
      .addLinesIndented(s"signed(sender, ${currentVersionFields.map(_.name).filter(_ != "proofs").mkString(", ")})")
      .addLines("}")
      .newLine
  }

  private def buildFromProto(scheme: TxScheme, className: String, currentVersionFields: Seq[FieldScheme]): CodeWriter.EndoFunctor = { writer =>
    writer
      .addLines(s"def fromProto(protoTx: Pb${scheme.entryName}): Either[ValidationError, $className] = {")
      .indent
      .addLines("for {")
      .indent
      .fold(currentVersionFields) {
        case (writer, field) =>
          field.tpe match {
            case tpe: ProtoCompatibleType =>
              writer
                .addLines {
                  tpe.protoToVanillaAdapter.fold(s"${field.name} = protoTx.${field.protoName}") { adapter =>
                    if (tpe.isMessageProtoType) {
                      val nonEmptyFieldName = s"some${field.name.capitalize}"
                      s"""$nonEmptyFieldName <- protoTx.${field.protoName}.toRight(ValidationError.GenericError("Field '${field.name}' cannot be empty"))
                         |${field.name} <- ${adapter(ProtoAdaptationContext(nonEmptyFieldName))}""".stripMargin
                    } else {
                      field.name + " <- " + adapter(ProtoAdaptationContext(s"protoTx.${field.protoName}"))
                    }
                  }
                }
            case _ =>
              writer.addLines(s"{Field type ${field.tpe} not implemented ProtoCompatibleType}")
          }

      }
      .addLines(s"result <- create(${currentVersionFields.map(_.name).mkString(", ")})")
      .outdent
      .addLines("} yield result")
      .outdent
      .addLines("}")
      .newLine
  }

  private def versionedClassName(scheme: TxScheme, version: Int): String = {
    s"${scheme.entryName}V$version"
  }
}
