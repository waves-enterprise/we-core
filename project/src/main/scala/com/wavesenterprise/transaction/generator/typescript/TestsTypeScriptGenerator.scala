package com.wavesenterprise.transaction.generator.typescript

import com.wavesenterprise.transaction.TxScheme
import com.wavesenterprise.transaction.generator.base.{CodeWriter, TypeScriptGenerator}

object TestsTypeScriptGenerator extends TypeScriptGenerator {
  val FILE_NAME = "Tests"

  private val fieldTestValue: Map[String, String] =
    Map(
      "amount"              -> """"100000000"""",
      "assetId"             -> """"WAVES"""",
      "attachment"          -> """"base64:3rbFDtbPwAvSp2vBvqGfGR9PxYf34SocMRkRKFgzTtXXnnv7upRHXJzZrLSQo8tUW6yMtEiZ"""",
      "fee"                 -> "1000000",
      "feeAssetId"          -> """"WAVES"""",
      "recipient"           -> """"3NiVPB1t32jC3SJpLomY3Zv6kwvfaJpRkqS"""",
      "senderPublicKey"     -> """"34qsNWsKKQaysTzpsf4aTyRS6Q1BoUuBntgGVj6SHZg3"""",
      "targetPubKey"        -> """"FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z"""",
      "timestamp"           -> "1598008066632",
      "duplicate_timestamp" -> "1598008066632",
      "target"              -> """"3NiVPB1t32jC3SJpLomY3Zv6kwvfaJpRkqS"""",
      "nodeName"            -> """"node-0"""",
      "opType"              -> """"add"""",
      "role"                -> """"miner"""",
      "dueTimestamp"        -> "1572600785208",
      "proofs"              -> """["5wMeGz4xzrs1AYJQR7DQU8jK8KJZ4g7HGxiGiZ1H8rfUHJKyKxTcZWFWHhojWuJMjst6kbDYL4EkcV2GyXKffyPU"]""",
      "alias"               -> """"John"""",
      "chainId"             -> "1",
      "name"                -> """"D56Gk8tvSAhNesghXgjAw67rSYDf4F2vo7HmsFTuGweC"""",
      "description"         -> """"Some script"""",
      "quantity"            -> "10000000",
      "decimals"            -> "2",
      "reissuable"          -> "true",
      "script"              -> """"base64:3rbFDtbPwAvSp2vBvqGfGR9nRS1nBVnfuSCN3HxSZ7fVRpt3tuFG5JSmyTmvHPxYf34SocMRkRKFgzTtXXnnv7upRHXJzZrLSQo8tUW6yMtE"""",
      "leaseId"             -> """"E9yZC4cVhCDfbjFJCc9CqkAtkoFy5KaCe64iaxHM2adG"""",
      "isEnabled"           -> "true",
      "authorPublicKey"     -> """"34qsNWsKKQaysTzpsf4aTyRS6Q1BoUuBntgGVj6SHZg3"""",
      "data"                -> """[{"type":"integer", "key": "height", "value": 100}]""",
      "transfers"           -> """[
                              |  {"recipient":"3NgSJRdMYu4ZbNpSbyRNZLJDX926W7e1EKQ","amount":"1000000000"},
                              |  {"recipient":"3NotQaBygbSvYZW4ftJ2ZwLXex4rTHY1Qzn","amount":"1000000000"},
                              |  {"recipient":"3NpkC1FSW9xNfmAMuhRSRArLgnfyGyEry7w","amount":"1000000000"},
                              |  {"recipient":"3NkZd8Xd4KsuPiNVsuphRNCZE3SqJycqv8d","amount":"1000000000"}
                              |]""".stripMargin,
      "policyName"          -> """"SomeName"""",
      "recipients"          -> """["3NotQaBygbSvYZW4ftJ2ZwLXex4rTHY1Qzn", "3votNaBcgb25FdsdgsdSvYZW4ftJ2ZwLXex"]""",
      "owners"              -> """["3NotQaBygbSvYZW4ftJ2ZwLXex4rTHY1Qzn", "3votNaBcgb25FdsdgsdSvYZW4ftJ2ZwLXex"]""",
      "policyId"            -> """"DP5MggKC8GJuLZshCVNSYwBtE6WTRtMM1YPPdcmwbuNg"""",
      "image"               -> """"localhost:5000/smart-kv"""",
      "imageHash"           -> """"b48d1de58c39d2160a4b8a5a9cae90818da1212742ec1f11fba1209bed0a212c"""",
      "dataHash"            -> """"34qsNWsKKQaysTzpsf4aTyRS6Q1BoUuBntgGVj6SHZg3"""",
      "contractName"        -> """"SomeName"""",
      "params"              -> """[{"type":"integer", "key": "height", "value": 100}]""",
      "contractId"          -> """"DP5MggKC8GJuLZshCVNSYwBtE6WTRtMM1YPPdcmwbuNg"""",
      "contractVersion"     -> "2",
      "validationPolicy"    -> "0",
      "apiVersion"          -> """"1.1""""
    )

  override protected def buildWriter(schemes: Seq[TxScheme]): CodeWriter =
    CodeWriter()
      .addLines(s"""import { ${TxTypeScriptGenerator.EXPORT_NAME} } from '../src/';""")
      .addLines(s"""import { config } from "@wavesenterprise/signature-generator";""")
      .addLines(s"""import * as expect from 'expect';""")
      .newLine
      .addLines(s"""const decoder = new TextDecoder('utf-8');""")
      .newLine
      .combine(testsWriter(schemes))

  private def testsWriter(schemes: Seq[TxScheme]): CodeWriter =
    CodeWriter()
      .addLines("""describe('', () => {""")
      .indent
      .addLines("beforeEach(() => {")
      .addLinesIndented("config.set({networkByte: 84, crypto: 'waves'})")
      .addLines("});")
      .newLine
      .foldWithDelimiter(schemes, "\n") {
        case (schemeWriter, scheme) =>
          schemeWriter.foldWithDelimiter(scheme.supportedTypeScriptVersions, "\n") {
            case (testWriter, version) =>
              val versionSuffix   = if (version > 1) s"V$version" else ""
              val versionedTxName = scheme.typeScriptEntryName + versionSuffix
              val fieldNames = scheme.fields
                .filter { field =>
                  field.inConstructorVersions.forall(_.contains(version)) &&
                  !field.versionToBodyValue.isDefinedAt(version)
                }
                .flatMap { field =>
                  field.typeScriptType.map(_ => field.typeScriptName)
                }

              testWriter
                .addLines(s"it('$versionedTxName', async () => {")
                .indent
                .addLines("const transaction = {")
                .foldWithDelimiterIndented(fieldNames, ",") {
                  case (fieldWriter, fieldName) =>
                    fieldWriter.addLines(s"$fieldName: ${fieldTestValue.getOrElse(fieldName, """""""")}")
                }
                .applyIf(scheme.versionToAdditionalTypeScriptFields(version).nonEmpty)(_.append(","))
                .foldWithDelimiterIndented(scheme.versionToAdditionalTypeScriptFields(version), ",") {
                  case (fieldWriter, (fieldName, _)) =>
                    fieldWriter.addLines(s"$fieldName: ${fieldTestValue.getOrElse(fieldName, """""""")}")
                }
                .addLines("};")
                .addLines(s"const signatureGenerator = TRANSACTIONS.${scheme.typeScriptEntryName}.V$version(transaction);")
                .addLines("const bytes = await signatureGenerator.getBytes();")
                .newLine
                .addLines("expect(decoder.decode(bytes))")
                .indent
                .addLines(".toEqual(")
                .indent
                .addLines("decoder.decode(Int8Array.from(")
                // TODO 08.12.2020 izhavoronkov: fix when client will be ready
//                .addLinesIndented(s"[${txBytesByType(scheme.typeId)}]")
                .addLinesIndented(s"[0]")
                .outdent
                .addLines(")).toString()")
                .outdent
                .addLines(")")
                .outdent
                .addLines("})")
          }
      }
      .outdent
      .addLines("});")
}
