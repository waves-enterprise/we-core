package com.wavesenterprise.transaction.generator.typescript

import com.wavesenterprise.transaction.TxScheme
import com.wavesenterprise.transaction.generator.base.FieldType.{OPTION_BASE, SHORT_LIST}
import com.wavesenterprise.transaction.generator.base.{CodeWriter, TypeScriptGenerator}
import src.main.scala.com.wavesenterprise.transaction.generator.typescript.ConstantsTypeScriptGenerator

object TxTypeScriptGenerator extends TypeScriptGenerator {
  val FILE_NAME   = "Transactions"
  val EXPORT_NAME = "TRANSACTIONS"

  override protected def imports: Set[String] = super.imports ++ Set("TxType", "TxVersion")

  override protected def buildWriter(schemes: Seq[TxScheme]): CodeWriter = {
    val imports      = importsWriter(schemes)
    val transactions = transactionsWriter(schemes)
    val exports      = exportsWriter(schemes)

    imports.newLine.newLine.combine(transactions).newLine.combine(exports)
  }

  private def importsWriter(schemes: Seq[TxScheme]): CodeWriter = {
    val schemeImports = schemes
      .flatMap { scheme =>
        scheme.fields
          .filter { field =>
            field.inConstructorVersions.forall(_.nonEmpty) && // Include field if it belongs to at least one version
            !scheme.supportedVersions.forall(field.versionToBodyValue.isDefinedAt) // Exclude hardcoded fields
          }
      }
      .flatMap { scheme =>
        scheme.tpe match {
          case s @ SHORT_LIST(underlay) =>
            underlay.typeScriptType.fold(List.empty[String]) { underlayType =>
              List(s.typeScriptType.get, underlayType)
            }
          case other => other.typeScriptType.fold(List.empty[String])(List(_))
        }
      }
      .toSet

    val schemeAdditionalImports = (for {
      scheme         <- schemes
      version        <- scheme.supportedTypeScriptVersions
      versionImports <- scheme.versionToAdditionalTypeScriptImports(version)
    } yield versionImports).toSet

    val allImports = (schemeImports ++ schemeAdditionalImports ++ imports).toList.sorted

    CodeWriter()
      .addLines("import {")
      .foldWithDelimiterIndented(allImports, ",") {
        case (w, imp) => w.addLines(s"$imp")
      }
      .addLines("} from '@wavesenterprise/signature-generator'")
      .addLines(s"import { $txTypesTsClassName, $txVersionsTxClassName } from './${ConstantsTypeScriptGenerator.FILE_NAME}'")
      .addLines("import { createTransactionsFactory, Processor } from './TransactionsFactory'")
  }

  private def transactionsWriter(schemes: Seq[TxScheme]): CodeWriter =
    CodeWriter()
      .foldWithDelimiter(schemes, "\n") {
        case (schemeWriter, scheme) =>
          schemeWriter.foldWithDelimiter(scheme.supportedTypeScriptVersions, "\n") {
            case (versionedWriter, version) =>
              val versionSuffix   = if (version > 1) s"V$version" else ""
              val versionedTxName = scheme.typeScriptEntryName + versionSuffix
              val typescriptFields = scheme.fields
                .filter { field =>
                  field.inConstructorVersions.forall(_.contains(version)) &&
                  !field.versionToBodyValue.isDefinedAt(version) && field.typeScriptType.isDefined &&
                  (field.tpe match {
                    case SHORT_LIST(underlying) => underlying.typeScriptType.isDefined
                    case _                      => true
                  })
                }

              versionedWriter
                .addLines(s"const $versionedTxName = {")
                .indent
                .addLines(s"tx_type: new TxType(true, $txTypesTsClassName.${scheme.typeScriptEntryName}),")
                .addLines(s"version: new TxVersion(true, $txVersionsTxClassName.V$version),")
                .foldWithDelimiter(typescriptFields, ",") {
                  case (fieldWriter, field) =>
                    field.tpe match {
                      case SHORT_LIST(underlying) =>
                        fieldWriter.addLines(s"${field.typeScriptName}: new ${field.typeScriptType.get}(${underlying.typeScriptType.get})")
                      case _ =>
                        val isRequired = field.tpe match {
                          case _: OPTION_BASE => "false"
                          case _              => "true"
                        }
                        val fieldLimit = field.typeScriptLimit.map(l => s", $l").getOrElse("")
                        fieldWriter.addLines(s"${field.typeScriptName}: new ${field.typeScriptType.get}($isRequired$fieldLimit)")
                    }
                }
                .applyIf(scheme.versionToAdditionalTypeScriptFields(version).nonEmpty)(_.append(","))
                .foldWithDelimiter(scheme.versionToAdditionalTypeScriptFields(version), ",") {
                  case (fieldWriter, (fieldName, fieldType)) => fieldWriter.addLines(s"$fieldName: $fieldType")
                }
                .outdent
                .addLines("}")
          }
      }

  def exportsWriter(schemes: Seq[TxScheme]): CodeWriter =
    CodeWriter()
      .addLines(s"export const $EXPORT_NAME = {")
      .foldWithDelimiterIndented(schemes, ",") {
        case (writer, scheme) =>
          writer
            .addLines(s"${scheme.typeScriptEntryName}: {")
            .foldWithDelimiter(scheme.supportedTypeScriptVersions, ",") {
              case (innerWriter, version) =>
                val versionSuffix = if (version > 1) s"V$version" else ""
                val txName        = scheme.typeScriptEntryName + versionSuffix
                innerWriter.addLinesIndented(s"V$version: createTransactionsFactory($txName)")
            }
            .addLines("}")
      }
      .addLines("}")
}
