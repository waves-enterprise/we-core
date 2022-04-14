package src.main.scala.com.wavesenterprise.transaction.generator.scala

import com.wavesenterprise.transaction.TxScheme
import com.wavesenterprise.transaction.generator.base.CodeWriter
import src.main.scala.com.wavesenterprise.transaction.generator.base.Adapter

/**
  * Creates a proto adapter object that transforms transactions from proto to scala models.
  */
object TxAdapterGenerator extends Adapter {

  val packageName = "com.wavesenterprise.serialization"
  val objectName = "TxAdapter"

  def buildWriter(schemes: Seq[TxScheme]): CodeWriter = {
    val imports = Seq(
      "com.wavesenterprise.transaction.ProtoSerializableTransaction",
      "com.wavesenterprise.transaction.ValidationError",
      "com.wavesenterprise.transaction.protobuf.{Transaction => PbTransaction}"
    ) ++ schemes.map(scheme => scheme.packageName + s".${scheme.entryName}")

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
      .call(buildFromProto(schemes))
      .outdent
      .addLines("}")
      .newLine
  }

  def buildFromProto(schemes: Seq[TxScheme]): CodeWriter.EndoFunctor = { writer =>
    writer
      .addLines(s"def fromProto(protoTx: PbTransaction): Either[ValidationError, ProtoSerializableTransaction] = {")
      .indent
      .addLines("protoTx.transaction match {")
      .indent
      .fold(schemes) {
        case (writer, scheme) =>
          writer.addLines(s"case PbTransaction.Transaction.${scheme.entryName}(value) => ${scheme.entryName}.fromProto(protoTx.version, value)")
      }
      .addLines("""case PbTransaction.Transaction.Empty => Left(ValidationError.GenericError(s"Empty inner transaction"))""")
      .addLines("""case _ => Left(ValidationError.GenericError(s"Proto-unsupported transaction"))""")
      .outdent
      .addLines("}")
      .outdent
      .addLines("}")
      .newLine
  }
}
