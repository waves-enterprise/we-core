package com.wavesenterprise.transaction.generator.scala

import com.wavesenterprise.transaction.TxScheme
import com.wavesenterprise.transaction.generator.base.CodeWriter
import src.main.scala.com.wavesenterprise.transaction.generator.base.Adapter

/**
  * Creates a proto adapter object that transforms transactions inside the container.
  */
object AtomicInnerTxAdapterGenerator extends Adapter {

  val packageName = "com.wavesenterprise.serialization"
  val objectName = "AtomicInnerTxAdapter"

  def buildWriter(schemes: Seq[TxScheme]): CodeWriter = {
    val filteredSchemes = schemes.filter(scheme => !scheme.isContainer)

    val imports = Seq(
      "com.wavesenterprise.transaction.AtomicInnerTransaction",
      "com.wavesenterprise.transaction.ValidationError",
      "com.wavesenterprise.transaction.protobuf.{AtomicInnerTransaction => PbInnerTransaction}"
    ) ++ filteredSchemes
      .map(scheme => scheme.packageName + s".${scheme.entryName}")

    CodeWriter()
      .addLines(s"package $packageName")
      .newLine
      .fold(imports) {
        case (writer, imp) => writer.addLines("import " + imp)
      }
      .newLine
      .addLines(s"object $objectName {")
      .indent
      .newLine
      .call(buildToProto(filteredSchemes))
      .call(buildFromProto(filteredSchemes))
      .outdent
      .addLines("}")
      .newLine
  }

  def buildToProto(schemes: Seq[TxScheme]): CodeWriter.EndoFunctor = { writer =>
    writer
      .addLines("def toProto(tx: AtomicInnerTransaction): PbInnerTransaction = {")
      .indent
      .addLines("val pbTransaction = tx match {")
      .foldIndented(schemes) {
        case (writer, scheme) =>
          import scheme.{entryName, lowerCamelCaseEntryName}

          writer.addLines(s"case $lowerCamelCaseEntryName: $entryName => PbInnerTransaction.Transaction.$entryName($lowerCamelCaseEntryName.toInnerProto)")
      }
      .addLines("}")
      .newLine
      .addLines("PbInnerTransaction(tx.version, pbTransaction)")
      .outdent
      .addLines("}")
      .newLine
  }

  def buildFromProto(schemes: Seq[TxScheme]): CodeWriter.EndoFunctor = { writer =>
    writer
      .addLines(s"def fromProto(protoTx: PbInnerTransaction): Either[ValidationError, AtomicInnerTransaction] = {")
      .indent
      .addLines("val versionedTx = protoTx.transaction match {")
      .indent
      .fold(schemes) {
        case (writer, scheme) =>
          writer.addLines(s"case PbInnerTransaction.Transaction.${scheme.entryName}(value) => ${scheme.entryName}.fromProto(protoTx.version, value)")
      }
      .addLines("""case PbInnerTransaction.Transaction.Empty => Left(ValidationError.GenericError(s"Empty inner transaction"))""")
      .addLines("""case transaction => Left(ValidationError.GenericError(s"Impossible inner transaction type: ${transaction.getClass()}"))""")
      .outdent
      .addLines("}")
      .newLine
      .addLines("versionedTx.flatMap {")
      .indent
      .addLines("case tx: AtomicInnerTransaction => Right(tx)")
      .addLines("""case _ => Left(ValidationError.GenericError(s"Internal transaction does not support atomic container"))""")
      .outdent
      .addLines("}")
      .outdent
      .addLines("}")
      .newLine
  }
}
