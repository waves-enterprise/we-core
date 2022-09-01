package src.main.scala.com.wavesenterprise.transaction.generator.typescript

import com.wavesenterprise.transaction.TxScheme
import com.wavesenterprise.transaction.generator.base.{CodeWriter, TypeScriptGenerator}

object ConstantsTypeScriptGenerator extends TypeScriptGenerator {
  val FILE_NAME = "constants"

  override protected def buildWriter(schemes: Seq[TxScheme]): CodeWriter = {
    val txVersions = schemes.flatMap(_.supportedTypeScriptVersions).distinct.sorted
    val txNameTypeId = schemes
      .map { scheme =>
        scheme.typeScriptEntryName -> scheme.typeId
      }
      .sortBy(_._2)

    CodeWriter()
      .addLines(s"export const $txTypesTsClassName = {")
      .foldWithDelimiterIndented(txNameTypeId, ",") {
        case (writer, (typeName, typeId)) => writer.addLines(s"$typeName: $typeId")
      }
      .addLines("}")
      .newLine
      .addLines(s"export const $txVersionsTxClassName = {")
      .foldWithDelimiterIndented(txVersions, ",") {
        case (writer, v) => writer.addLines(s"V$v: $v")
      }
      .addLines("}")
  }
}
