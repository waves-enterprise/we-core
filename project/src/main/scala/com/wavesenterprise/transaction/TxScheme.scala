package com.wavesenterprise.transaction

import com.google.common.base.CaseFormat
import com.wavesenterprise.transaction.generator.base.EssentialFields._
import com.wavesenterprise.transaction.generator.base.FieldDefinitionSyntax._
import com.wavesenterprise.transaction.generator.base.FieldGenerationOption.{Validation, _}
import com.wavesenterprise.transaction.generator.base.FieldType._
import com.wavesenterprise.transaction.generator.base._
import enumeratum._

import scala.collection.immutable
import scala.collection.immutable.Set

/*
 * FIELD ORDER CHANGE BREAKS SERIALIZATION, CAN ONLY APPEND NEW FIELDS FOR NEW VERSIONS
 * */
sealed abstract class TxScheme(
    val packageName: String = TxScheme.BasePackage,
    val typeId: Byte,
    val supportedVersions: Set[Int],
    val fields: Seq[FieldScheme],
    val versionToBlockchainFeatures: PartialFunction[Int, Seq[BlockchainFeature]] = PartialFunction.empty,
    val additionalImports: Set[String] = Set.empty,
    val additionalJsonFields: Seq[(String, String)] = Seq.empty,
    val versionToAdditionalTypeScriptFields: Int => Seq[(String, String)] = _ => Seq.empty,
    val versionToAdditionalTypeScriptImports: Int => Set[String] = _ => Set.empty,
    val sealedTraitExtensions: Seq[String] = Seq.empty,
    val caseClassCompanionExtensions: Seq[String] = Seq.empty,
    val versionExtensions: PartialFunction[Int, Seq[String]] = PartialFunction.empty,
    val versionToBinaryHeaderType: Int => BinaryHeaderType = _ => BinaryHeaderType.Modern,
    /** Flag indicating whether to memoize json and binary representation */
    val cacheSerializable: Boolean = true,
    /** Specifies a post-condition methods for transaction creating */
    val ensures: Seq[String] = Seq.empty,
    val isContainer: Boolean = false,
    private val unsupportedTypeScriptVersions: Set[Int] = Set.empty,
    val unusedImports: Set[String] = Set.empty
) extends EnumEntry {

  val supportedTypeScriptVersions: Set[Int] = supportedVersions -- unsupportedTypeScriptVersions

  def protobufPackageName: String = {
    val protoPackage = packageName.replace(".transaction", ".transaction.protobuf")

    if (packageName == protoPackage)
      throw new IllegalArgumentException("The package name must start with 'com.wavesenterprise.transaction'")
    else
      protoPackage
  }

  def snakeCaseEntryName: String = {
    CaseFormat.UPPER_CAMEL.to(CaseFormat.LOWER_UNDERSCORE, entryName)
  }

  def lowerCamelCaseEntryName: String = {
    CaseFormat.UPPER_CAMEL.to(CaseFormat.LOWER_CAMEL, entryName)
  }

  def typeScriptEntryName: String = entryName.replaceFirst("Transaction", "")
}

/**
  * Warning: it is not allowed to change the order of transaction definitions in this object. It is important for proto
  * generation.
  */
object TxScheme extends Enum[TxScheme] {

  val BasePackage      = "com.wavesenterprise.transaction"
  val BaseProtoPackage = "com.wavesenterprise.transaction.protobuf"

  case object RegisterNodeTransaction
      extends TxScheme(
        typeId = 111,
        supportedVersions = Set(1, 2),
        fields = Seq(
          senderField,
          "target" as PUBLIC_KEY_ACCOUNT -> Set(Json(f => s"$f.address"), TypeScriptCustomName("targetPubKey")),
          "nodeName" as NODE_NAME        -> Validation(f => s"validateNodeName($f)"),
          "opType" as PERMISSION_OP_TYPE -> Validation(f => s"validateOpType($f, nodeName)"),
          timestampField,
          feeField,
          "atomicBadge" as OPTION(ATOMIC_BADGE) -> Set(Override, InConstructor(2)),
          proofsField,
          "sender_address" as SENDER_ADDRESS -> Transparent
        ),
        additionalJsonFields = Seq("targetPubKey" -> "target.publicKeyBase58"),
        additionalImports =
          Set("com.wavesenterprise.transaction.validation.RegisterNodeValidation._", "com.wavesenterprise.transaction.AtomicInnerTransaction"),
        versionToBlockchainFeatures = {
          case 2 => Seq(BlockchainFeature.OtherTxTypesAtomicSupport)
        },
        versionExtensions = {
          case 2 => Seq("AtomicInnerTransaction")
        },
      )

  case object CreateAliasTransaction
      extends TxScheme(
        typeId = 10,
        supportedVersions = Set(2, 3, 4),
        fields = Seq(
          "id" as CUSTOM_TYPE(scalaName = "Coeval[ByteStr]", protoName = "bytes") -> Set(
            Override,
            InBody(
              { case _ => "Coeval.evalOnce(ByteStr(crypto.fastHash(builder.typeId +: alias.bytes.arr)))" },
              explicitVal = true
            )
          ),
          senderField,
          "alias" as ALIAS -> Json(f => s"$f.name"),
          feeField,
          timestampField,
          "feeAssetId" as ASSET_ID.?            -> Set(Override, InConstructor(3, 4)),
          "atomicBadge" as OPTION(ATOMIC_BADGE) -> Set(Override, InConstructor(4)),
          proofsField,
          "sender_address" as SENDER_ADDRESS -> Transparent
        ),
        additionalImports = Set("com.wavesenterprise.crypto", "com.wavesenterprise.transaction.AtomicInnerTransaction"),
        versionToBlockchainFeatures = {
          case 3 => Seq(BlockchainFeature.SponsoredFeesSupport)
          case 4 => Seq(BlockchainFeature.SponsoredFeesSupport, BlockchainFeature.OtherTxTypesAtomicSupport)
        },
        versionExtensions = {
          case 4 => Seq("AtomicInnerTransaction")
        }
      )

  // region Assets

  private val assetsTxPackage = "com.wavesenterprise.transaction.assets"

  case object IssueTransaction
      extends TxScheme(
        packageName = assetsTxPackage,
        typeId = 3,
        supportedVersions = Set(2, 3),
        fields = Seq(
          "chainId" as BYTE -> Set(Validation(f => s"validateChainId($f)"), Override),
          senderField,
          "name" as SHORT_BYTE_ARRAY -> Set(Validation(f => s"validateName($f)"), Json(f => s"new String($f, StandardCharsets.UTF_8)")),
          "description" as SHORT_BYTE_ARRAY -> Set(Validation(f => s"validateDescription($f)"),
                                                   Json(f => s"new String($f, StandardCharsets.UTF_8)"),
                                                   TypeScriptLimit(1000)),
          "quantity" as LONG -> Validation(f => s"""validatePositiveAmount($f, "assets")"""),
          "decimals" as BYTE -> Validation(f => s"validateDecimals($f)"),
          "reissuable" as BOOLEAN,
          feeField,
          timestampField,
          "atomicBadge" as OPTION(ATOMIC_BADGE) -> Set(Override, InConstructor(3)),
          "script" as SCRIPT.?                  -> Json(f => s"$f.map(_.bytes().base64)"),
          proofsField,
          "assetId" as CUSTOM_TYPE(scalaName = "Coeval[ByteStr]", protoName = "bytes") -> InBody({ case _ => "id" }, explicitVal = true),
          "sender_address" as SENDER_ADDRESS                                           -> Transparent
        ),
        additionalJsonFields = Seq("assetId" -> "assetId().base58"),
        additionalImports = Set(
          "com.wavesenterprise.transaction.validation._",
          "com.wavesenterprise.transaction._",
          "java.nio.charset.StandardCharsets",
          "com.wavesenterprise.state.ByteStr",
          "com.wavesenterprise.transaction.AtomicInnerTransaction"
        ),
        versionExtensions = {
          case 3 => Seq("AtomicInnerTransaction")
        },
        sealedTraitExtensions = Seq("ChainSpecific"),
        versionToBlockchainFeatures = {
          case 2 =>
            Seq(
              BlockchainFeature.SmartAccounts,
              BlockchainFeature.EvalFeature("script.map(_ => Seq(BlockchainFeature.SmartAssets)).getOrElse(Seq.empty)")
            )
          case 3 =>
            Seq(
              BlockchainFeature.SmartAccounts,
              BlockchainFeature.EvalFeature("script.map(_ => Seq(BlockchainFeature.SmartAssets)).getOrElse(Seq.empty)"),
              BlockchainFeature.OtherTxTypesAtomicSupport
            )
        }
      )

  case object ReissueTransaction
      extends TxScheme(
        packageName = assetsTxPackage,
        typeId = 5,
        supportedVersions = Set(2, 3),
        fields = Seq(
          "chainId" as BYTE -> Set(Validation(f => s"validateChainId($f)"), Override),
          senderField,
          "assetId" as ASSET_ID,
          "quantity" as LONG -> Validation(f => s"""validatePositiveAmount($f, "assets")"""),
          "reissuable" as BOOLEAN,
          feeField,
          timestampField,
          "atomicBadge" as OPTION(ATOMIC_BADGE) -> Set(Override, InConstructor(3)),
          proofsField,
          "checkedAssets" as CUSTOM_TYPE(scalaName = "List[ByteStr]", protoName = "repeated bytes") -> Set(
            Override,
            InBody({ case _ => "List(assetId)" })
          ),
          "sender_address" as SENDER_ADDRESS -> Transparent
        ),
        additionalImports = Set("com.wavesenterprise.transaction.validation._",
                                "com.wavesenterprise.transaction._",
                                "com.wavesenterprise.transaction.AtomicInnerTransaction"),
        versionExtensions = {
          case 3 => Seq("AtomicInnerTransaction")
        },
        sealedTraitExtensions = Seq("ChainSpecific"),
        versionToBlockchainFeatures = {
          case 2 => Seq(BlockchainFeature.SmartAccounts)
          case 3 => Seq(BlockchainFeature.SmartAccounts, BlockchainFeature.OtherTxTypesAtomicSupport)
        }
      )

  case object BurnTransaction
      extends TxScheme(
        packageName = assetsTxPackage,
        typeId = 6,
        supportedVersions = Set(2, 3),
        fields = Seq(
          "chainId" as BYTE -> Override,
          senderField,
          "assetId" as ASSET_ID,
          "amount" as LONG -> Validation(f => s"""validateNonNegativeAmount($f, "assets")"""),
          feeField,
          timestampField,
          "atomicBadge" as OPTION(ATOMIC_BADGE) -> Set(Override, InConstructor(3)),
          proofsField,
          "checkedAssets" as CUSTOM_TYPE(scalaName = "List[ByteStr]", protoName = "repeated bytes") -> Set(
            Override,
            InBody({ case _ => "List(assetId)" })
          ),
          "sender_address" as SENDER_ADDRESS -> Transparent
        ),
        additionalImports = Set("com.wavesenterprise.transaction.validation._",
                                "com.wavesenterprise.transaction._",
                                "com.wavesenterprise.transaction.AtomicInnerTransaction"),
        versionExtensions = {
          case 3 => Seq("AtomicInnerTransaction")
        },
        sealedTraitExtensions = Seq("ChainSpecific"),
        versionToBlockchainFeatures = {
          case 2 => Seq(BlockchainFeature.SmartAccounts)
          case 3 => Seq(BlockchainFeature.SmartAccounts, BlockchainFeature.OtherTxTypesAtomicSupport)
        }
      )

  private val leaseTxPackage = "com.wavesenterprise.transaction.lease"

  case object LeaseTransaction
      extends TxScheme(
        packageName = leaseTxPackage,
        typeId = 8,
        supportedVersions = Set(2, 3),
        fields = Seq(
          "assetId" as ASSET_ID.? -> NoJson,
          /*_*/
          "assetIdChecker" as CUSTOM_TYPE(scalaName = "Unit", protoName = "unit") -> InBody(
            { case _ => """require(assetId.isEmpty, "Leasing assets is not supported yet")""" },
            explicitVal = true,
            excludeFromSealedTrait = true
          ),
          /*_*/
          senderField,
          "recipient" as ADDRESS_OR_ALIAS -> Json(f => s"$f.stringRepr"),
          "amount" as LONG                -> Validation(f => s"validateLeaseParams($f, fee)"),
          feeField,
          timestampField,
          "atomicBadge" as OPTION(ATOMIC_BADGE) -> Set(Override, InConstructor(3)),
          proofsField,
          "sender_address" as SENDER_ADDRESS -> Transparent
        ),
        additionalImports =
          Set("com.wavesenterprise.transaction.validation.LeaseValidation._", "com.wavesenterprise.transaction.AtomicInnerTransaction"),
        versionExtensions = {
          case 3 => Seq("AtomicInnerTransaction")
        },
        versionToBlockchainFeatures = {
          case 2 => Seq(BlockchainFeature.SmartAccounts)
          case 3 => Seq(BlockchainFeature.SmartAccounts, BlockchainFeature.OtherTxTypesAtomicSupport)
        }
      )

  case object LeaseCancelTransaction
      extends TxScheme(
        packageName = leaseTxPackage,
        typeId = 9,
        supportedVersions = Set(2, 3),
        fields = Seq(
          "chainId" as BYTE -> Set(Validation(f => s"validateChainId($f)"), Override),
          senderField,
          feeField,
          timestampField,
          "leaseId" as ASSET_ID                 -> Validation(f => s"validateLeaseId($f)"),
          "atomicBadge" as OPTION(ATOMIC_BADGE) -> Set(Override, InConstructor(3)),
          proofsField,
          "sender_address" as SENDER_ADDRESS -> Transparent
        ),
        sealedTraitExtensions = Seq("ChainSpecific"),
        additionalImports = Set(
          "com.wavesenterprise.transaction.validation.LeaseValidation._",
          "com.wavesenterprise.transaction._",
          "com.wavesenterprise.transaction.AtomicInnerTransaction"
        ),
        versionExtensions = {
          case 3 => Seq("AtomicInnerTransaction")
        },
        versionToBlockchainFeatures = {
          case 2 => Seq(BlockchainFeature.SmartAccounts)
          case 3 => Seq(BlockchainFeature.SmartAccounts, BlockchainFeature.OtherTxTypesAtomicSupport)
        }
      )

  case object SponsorFeeTransaction
      extends TxScheme(
        packageName = assetsTxPackage,
        typeId = 14,
        supportedVersions = Set(1, 2),
        fields = Seq(
          senderField,
          "assetId" as ASSET_ID,
          "isEnabled" as BOOLEAN,
          feeField,
          timestampField,
          "atomicBadge" as OPTION(ATOMIC_BADGE) -> Set(Override, InConstructor(2)),
          proofsField,
          "checkedAssets" as CUSTOM_TYPE(scalaName = "List[ByteStr]", protoName = "repeated bytes") -> Set(
            Override,
            InBody({ case _ => "List(assetId)" })
          ),
          "sender_address" as SENDER_ADDRESS -> Transparent
        ),
        additionalImports = Set("com.wavesenterprise.transaction._", "com.wavesenterprise.transaction.AtomicInnerTransaction"),
        versionExtensions = {
          case 2 => Seq("AtomicInnerTransaction")
        },
        versionToBlockchainFeatures = {
          case 1 => Seq(BlockchainFeature.SponsoredFeesSupport)
          case 2 => Seq(BlockchainFeature.SponsoredFeesSupport, BlockchainFeature.OtherTxTypesAtomicSupport)
        }
      )

  case object SetAssetScriptTransaction
      extends TxScheme(
        packageName = assetsTxPackage,
        typeId = 15,
        supportedVersions = Set(1),
        fields = Seq(
          "chainId" as BYTE -> Set(Validation(f => s"validateChainId($f)"), Override),
          senderField,
          "assetId" as ASSET_ID,
          "script" as SCRIPT.? -> Json(f => s"$f.map(_.bytes().base64)"),
          feeField,
          timestampField,
          proofsField,
          "checkedAssets" as BIG_LIST(SHORT_BYTE_STR) -> Set(Override, InBody({ case _ => "List(assetId)" })),
          "sender_address" as SENDER_ADDRESS          -> Transparent
        ),
        additionalImports = Set("com.wavesenterprise.transaction.validation._", "com.wavesenterprise.transaction._"),
        sealedTraitExtensions = Seq("ChainSpecific"),
        versionToBlockchainFeatures = { case 1 => Seq(BlockchainFeature.SmartAssets) }
      )

  // endregion Assets

  // region Data

  case object DataTransaction
      extends TxScheme(
        typeId = 12,
        supportedVersions = Set(1, 2, 3),
        fields = Seq(
          senderField,
          "author" as PUBLIC_KEY_ACCOUNT -> Set(Json(f => s"$f.address"),
                                                ProtobufCustomName("authorPublicKey"),
                                                TypeScriptCustomName("authorPublicKey")),
          "data" as SHORT_LIST(DATA_ENTRY) -> Validation(f => s"validateData($f)"),
          timestampField,
          feeField,
          "feeAssetId" as ASSET_ID.?            -> Set(Override, InConstructor(2, 3)),
          "atomicBadge" as OPTION(ATOMIC_BADGE) -> Set(Override, InConstructor(3)),
          proofsField,
          "sender_address" as SENDER_ADDRESS -> Transparent
        ),
        additionalImports =
          Set("com.wavesenterprise.transaction.validation.DataValidation._", "com.wavesenterprise.transaction.AtomicInnerTransaction"),
        versionExtensions = {
          case 3 => Seq("AtomicInnerTransaction")
        },
        additionalJsonFields = Seq("authorPublicKey" -> "author"),
        ensures = Seq("validateSize"),
        versionToBlockchainFeatures = {
          case 1 => Seq(BlockchainFeature.DataTransaction)
          case 2 => Seq(BlockchainFeature.DataTransaction, BlockchainFeature.SponsoredFeesSupport)
          case 3 => Seq(BlockchainFeature.DataTransaction, BlockchainFeature.SponsoredFeesSupport, BlockchainFeature.OtherTxTypesAtomicSupport)

        },
        cacheSerializable = false
      )

  // endregion Data

  // region Transfer

  private val transferTxPackage = "com.wavesenterprise.transaction.transfer"

  case object TransferTransaction
      extends TxScheme(
        packageName = transferTxPackage,
        typeId = 4,
        supportedVersions = Set(2, 3),
        fields = Seq(
          senderField,
          "assetId" as ASSET_ID.?,
          "feeAssetId" as ASSET_ID.? -> Override,
          timestampField,
          "amount" as LONG -> Validation(f => s"""validatePositiveAmount($f, assetId.map(_.base58).getOrElse("WEST"))"""),
          feeField.copy(fieldToValidation = Some(f => s"validateFee($f)")),
          "recipient" as ADDRESS_OR_ALIAS -> Json(f => s"$f.stringRepr"),
          "attachment" as SHORT_BYTE_ARRAY -> Set(Validation(f => s"""validateAttachment($f)"""),
                                                  Json(f => s"Base58.encode($f)"),
                                                  TypeScriptCustomType("Base58WithLength"),
                                                  TypeScriptLimit(192)),
          "atomicBadge" as OPTION(ATOMIC_BADGE) -> Set(Override, InConstructor(3)),
          proofsField,
          "checkedAssets" as BIG_LIST(SHORT_BYTE_STR) -> Set(Override, InBody({ case _ => "assetId.toList" })),
          "sender_address" as SENDER_ADDRESS          -> Transparent
        ),
        versionToBlockchainFeatures = {
          case 2 =>
            Seq(
              BlockchainFeature.SmartAccounts,
              BlockchainFeature.EvalFeature("feeAssetId.fold(Seq.empty[BlockchainFeature])(_ => Seq(BlockchainFeature.SponsoredFeesSupport))")
            )
          case 3 =>
            Seq(
              BlockchainFeature.SmartAccounts,
              BlockchainFeature.SponsoredFeesSupport,
              BlockchainFeature.AtomicTransactionSupport
            )
        },
        additionalImports = Set(
          "com.wavesenterprise.transaction.validation._",
          "com.wavesenterprise.utils.Base58",
          "com.wavesenterprise.transaction.AtomicInnerTransaction"
        ),
        versionExtensions = { case 3 => Seq("AtomicInnerTransaction") }
      )

  case object MassTransferTransaction
      extends TxScheme(
        packageName = transferTxPackage,
        typeId = 11,
        supportedVersions = Set(1, 2, 3),
        fields = Seq(
          senderField,
          "assetId" as ASSET_ID.?,
          "transfers" as TRANSFER_BATCH -> Set(Validation(f => s"validateMassTransfers($f, ${feeField.name})"), Json(f => s"$f.map(_.toDescriptor)")),
          timestampField,
          feeField,
          "attachment" as SHORT_BYTE_ARRAY -> Set(Validation(f => s"""validateAttachment($f)"""),
                                                  Json(f => s"Base58.encode($f)"),
                                                  TypeScriptCustomType("Base58WithLength"),
                                                  TypeScriptLimit(192)),
          "feeAssetId" as ASSET_ID.?            -> Set(Override, InConstructor(2, 3)),
          "atomicBadge" as OPTION(ATOMIC_BADGE) -> Set(Override, InConstructor(3)),
          proofsField,
          "checkedAssets" as BIG_LIST(SHORT_BYTE_STR) -> Set(Override, InBody({ case _ => "assetId.toList" })),
          "sender_address" as SENDER_ADDRESS          -> Transparent
        ),
        additionalJsonFields = Seq(
          "transferCount" -> "transfers.size",
          "totalAmount"   -> "transfers.map(_.amount).sum"
        ),
        versionToBlockchainFeatures = {
          case 1 => Seq(BlockchainFeature.MassTransfer)
          case 2 => Seq(BlockchainFeature.MassTransfer, BlockchainFeature.SponsoredFeesSupport)
          case 3 => Seq(BlockchainFeature.MassTransfer, BlockchainFeature.SponsoredFeesSupport, BlockchainFeature.OtherTxTypesAtomicSupport)

        },
        additionalImports = Set("com.wavesenterprise.transaction.validation._",
                                "com.wavesenterprise.utils.Base58",
                                "com.wavesenterprise.transaction.AtomicInnerTransaction"),
        versionExtensions = {
          case 3 => Seq("AtomicInnerTransaction")
        },
        versionToBinaryHeaderType = {
          case 1 => BinaryHeaderType.Legacy
          case _ => BinaryHeaderType.Modern
        }
      )

  // endregion Transfer

  // region Acl

  case object PermitTransaction
      extends TxScheme(
        packageName = "com.wavesenterprise.transaction.acl",
        typeId = 102,
        supportedVersions = Set(1, 2),
        fields = Seq(
          senderField,
          "target" as ADDRESS_OR_ALIAS -> Json(f => s"$f.stringRepr"),
          timestampField,
          feeField,
          "permissionOp" as PERMISSION_OP       -> Set(Validation(f => s"validatePermissionOp(timestamp, $f)"), NoJson),
          "atomicBadge" as OPTION(ATOMIC_BADGE) -> Set(Override, InConstructor(2), NoTypeScript),
          proofsField,
          "sender_address" as SENDER_ADDRESS -> Transparent
        ),
        additionalJsonFields = Seq(
          "opType"       -> "permissionOp.opType.str",
          "role"         -> "permissionOp.role.prefixS",
          "dueTimestamp" -> "permissionOp.dueTimestampOpt"
        ),
        additionalImports = Set(
          "com.wavesenterprise.transaction.validation.PermitValidation._",
          "com.wavesenterprise.transaction.AtomicInnerTransaction"
        ),
        versionExtensions = { case 2 => Seq("AtomicInnerTransaction") },
        versionToAdditionalTypeScriptFields = v => {
          val v1Imports = Seq(
            "opType"              -> "new PermissionOpType(true)",
            "role"                -> "new PermissionRole(true)",
            "duplicate_timestamp" -> "new Long(true)",
            "dueTimestamp"        -> "new PermissionDueTimestamp(false)"
          )
          v match {
            case 1 => v1Imports
            case 2 => v1Imports :+ ("atomicBadge" -> "new AtomicBadge(false)")
            case _ => Seq.empty
          }
        },
        versionToAdditionalTypeScriptImports = {
          case 1 => Set("PermissionOpType", "PermissionRole", "PermissionDueTimestamp")
          case 2 => Set("PermissionOpType", "PermissionRole", "PermissionDueTimestamp", "AtomicBadge")
          case _ => Set.empty
        },
        versionToBlockchainFeatures = { case 2 => Seq(BlockchainFeature.AtomicTransactionSupport) }
      )

  // endregion Acl

  // region Policy

  case object CreatePolicyTransaction
      extends TxScheme(
        typeId = 112,
        supportedVersions = Set(1, 2, 3),
        fields = Seq(
          senderField,
          "policyName" as SHORT_STRING,
          "description" as SHORT_STRING,
          "recipients" as BIG_LIST(ADDRESS),
          "owners" as BIG_LIST(ADDRESS),
          timestampField,
          feeField,
          "feeAssetId" as ASSET_ID.?            -> Set(Override, InConstructor(2, 3)),
          "atomicBadge" as OPTION(ATOMIC_BADGE) -> Set(Override, InConstructor(3)),
          proofsField,
          "sender_address" as SENDER_ADDRESS -> Transparent
        ),
        versionToBlockchainFeatures = {
          case 2 => Seq(BlockchainFeature.SponsoredFeesSupport)
          case 3 => Seq(BlockchainFeature.SponsoredFeesSupport, BlockchainFeature.AtomicTransactionSupport)
        },
        additionalImports = Set("com.wavesenterprise.transaction.AtomicInnerTransaction"),
        versionExtensions = { case 3 => Seq("AtomicInnerTransaction") }
      )

  case object UpdatePolicyTransaction
      extends TxScheme(
        typeId = 113,
        supportedVersions = Set(1, 2, 3),
        fields = Seq(
          senderField,
          "policyId" as SHORT_BYTE_STR,
          "recipients" as BIG_LIST(ADDRESS),
          "owners" as BIG_LIST(ADDRESS),
          "opType" as PERMISSION_OP_TYPE,
          timestampField,
          feeField,
          "feeAssetId" as ASSET_ID.?            -> Set(Override, InConstructor(2, 3)),
          "atomicBadge" as OPTION(ATOMIC_BADGE) -> Set(Override, InConstructor(3)),
          proofsField,
          "sender_address" as SENDER_ADDRESS -> Transparent
        ),
        versionToBlockchainFeatures = {
          case 2 => Seq(BlockchainFeature.SponsoredFeesSupport)
          case 3 => Seq(BlockchainFeature.SponsoredFeesSupport, BlockchainFeature.AtomicTransactionSupport)
        },
        additionalImports = Set("com.wavesenterprise.transaction.AtomicInnerTransaction"),
        versionExtensions = { case 3 => Seq("AtomicInnerTransaction") }
      )

  case object PolicyDataHashTransaction
      extends TxScheme(
        typeId = 114,
        supportedVersions = Set(1, 2, 3),
        fields = Seq(
          senderField,
          "dataHash" as POLICY_DATA_HASH,
          "policyId" as SHORT_BYTE_STR,
          timestampField,
          feeField,
          "feeAssetId" as ASSET_ID.?            -> Set(Override, InConstructor(2, 3)),
          "atomicBadge" as OPTION(ATOMIC_BADGE) -> Set(Override, InConstructor(3)),
          proofsField,
          "sender_address" as SENDER_ADDRESS -> Transparent
        ),
        versionToBlockchainFeatures = {
          case 2 => Seq(BlockchainFeature.SponsoredFeesSupport)
          case 3 => Seq(BlockchainFeature.SponsoredFeesSupport, BlockchainFeature.AtomicTransactionSupport)
        },
        additionalImports = Set("com.wavesenterprise.transaction.AtomicInnerTransaction"),
        versionExtensions = { case 3 => Seq("AtomicInnerTransaction") },
        unsupportedTypeScriptVersions = Set(1, 2)
      )

  // endregion Policy

  // region Docker

  private val dockerTxPackage = "com.wavesenterprise.transaction.docker"

  case object CreateContractTransaction
      extends TxScheme(
        packageName = dockerTxPackage,
        typeId = 103,
        supportedVersions = Set(1, 2, 3, 4, 5, 6, 7),
        fields = Seq(
          senderField,
          "image" as SHORT_STRING                     -> Set(InConstructor(1, 2, 3, 4, 5, 6), Validation(f => s"validateImage($f)")),
          "imageHash" as SHORT_STRING                 -> Set(InConstructor(1, 2, 3, 4, 5, 6), Validation(f => s"validateHash($f)")),
          "contractName" as SHORT_STRING              -> Validation(f => s"validateContractName($f)"),
          "contractId" as SHORT_BYTE_STR              -> Set(Override, InBody({ case _ => "id()" })),
          "params" as SHORT_LIST(CONTRACT_DATA_ENTRY) -> Set(Override, Validation(f => s"validateParams($f)")),
          feeField,
          timestampField,
          "feeAssetId" as ASSET_ID.?                     -> Set(Override, InConstructor(2, 3, 4, 5, 6, 7)),
          "atomicBadge" as OPTION(ATOMIC_BADGE)          -> Set(Override, InConstructor(3, 4, 5, 6, 7)),
          "validationPolicy" as VALIDATION_POLICY        -> Set(InConstructor(4, 5, 6, 7), Validation(f => s"validateValidationPolicy($f)")),
          "apiVersion" as CONTRACT_API_VERSION           -> InConstructor(4, 5, 6),
          "payments" as SHORT_LIST(CONTRACT_TRANSFER_IN) -> Set(Override, InConstructor(5, 6, 7)),
          proofsField,
          "sender_address" as SENDER_ADDRESS        -> Transparent,
          "isConfidential" as BOOLEAN               -> Set(Override, InConstructor(6, 7)),
          "groupParticipants" as SHORT_SET(ADDRESS) -> Set(Override, InConstructor(6, 7)),
          "groupOwners" as SHORT_SET(ADDRESS)       -> Set(Override, InConstructor(6, 7)),
          "storedContract" as STORED_CONTRACT       -> Set(Override, InConstructor(7), Validation(f => s"validateHash($f)")),
        ),
        ensures = Seq("validateSize"),
        versionToBlockchainFeatures = {
          case 2 =>
            Seq(
              BlockchainFeature.ContractsGrpcSupport,
              BlockchainFeature.EvalFeature("feeAssetId.fold(Seq.empty[BlockchainFeature])(_ => Seq(BlockchainFeature.SponsoredFeesSupport))")
            )
          case 3 =>
            Seq(
              BlockchainFeature.ContractsGrpcSupport,
              BlockchainFeature.AtomicTransactionSupport,
              BlockchainFeature.SponsoredFeesSupport
            )
          case 4 =>
            Seq(
              BlockchainFeature.ContractsGrpcSupport,
              BlockchainFeature.AtomicTransactionSupport,
              BlockchainFeature.SponsoredFeesSupport,
              BlockchainFeature.ContractValidationsSupport
            )
          case 5 =>
            Seq(
              BlockchainFeature.ContractsGrpcSupport,
              BlockchainFeature.AtomicTransactionSupport,
              BlockchainFeature.SponsoredFeesSupport,
              BlockchainFeature.ContractValidationsSupport,
              BlockchainFeature.ContractNativeTokenSupportAndPkiV1Support
            )
          case 6 =>
            Seq(
              BlockchainFeature.ContractsGrpcSupport,
              BlockchainFeature.AtomicTransactionSupport,
              BlockchainFeature.SponsoredFeesSupport,
              BlockchainFeature.ContractValidationsSupport,
              BlockchainFeature.ContractNativeTokenSupportAndPkiV1Support,
              BlockchainFeature.ConfidentialDataInContractsSupport
            )
          case 7 =>
            Seq(
              BlockchainFeature.ContractsGrpcSupport,
              BlockchainFeature.AtomicTransactionSupport,
              BlockchainFeature.SponsoredFeesSupport,
              BlockchainFeature.ContractValidationsSupport,
              BlockchainFeature.ContractNativeTokenSupportAndPkiV1Support,
              BlockchainFeature.ConfidentialDataInContractsSupport,
              BlockchainFeature.WasmContractsSupport,
            )
        },
        sealedTraitExtensions = Seq("ExecutableTransaction"),
        caseClassCompanionExtensions = Seq("ContractTransactionValidation"),
        additionalImports = Set(
          "com.wavesenterprise.transaction.AtomicInnerTransaction",
          "com.wavesenterprise.transaction.PaymentsV1ToContract",
          "com.wavesenterprise.transaction.ValidationPolicySupport",
          "com.wavesenterprise.transaction.ApiVersionSupport",
          "com.wavesenterprise.transaction.docker.ConfidentialDataInContractSupported",
          "com.wavesenterprise.transaction.StoredContractSupported",
          "com.wavesenterprise.transaction.docker.DockerContractTransaction",
          "com.wavesenterprise.docker.StoredContract",
          "com.wavesenterprise.docker.StoredContractOps"
        ),
        versionExtensions = {
          case 1 => Seq("DockerContractTransaction")
          case 2 => Seq("DockerContractTransaction")
          case 3 => Seq("AtomicInnerTransaction", "DockerContractTransaction")
          case 4 => Seq("AtomicInnerTransaction", "ValidationPolicySupport", "ApiVersionSupport", "DockerContractTransaction")
          case 5 => Seq("AtomicInnerTransaction", "ValidationPolicySupport", "ApiVersionSupport", "PaymentsV1ToContract", "DockerContractTransaction")
          case 6 => Seq(
              "AtomicInnerTransaction",
              "ValidationPolicySupport",
              "ApiVersionSupport",
              "PaymentsV1ToContract",
              "ConfidentialDataInCreateContractSupported",
              "DockerContractTransaction"
            )
          case 7 => Seq("AtomicInnerTransaction",
                        "ValidationPolicySupport",
                        "PaymentsV1ToContract",
                        "ConfidentialDataInCreateContractSupported",
                        "StoredContractSupported")
        }
      )

  case object CallContractTransaction
      extends TxScheme(
        packageName = dockerTxPackage,
        typeId = 104,
        supportedVersions = Set(1, 2, 3, 4, 5, 6, 7),
        fields = Seq(
          senderField,
          "contractId" as SHORT_BYTE_STR,
          "params" as SHORT_LIST(CONTRACT_DATA_ENTRY) -> Set(Override, Validation(f => s"validateParams($f)")),
          feeField,
          timestampField,
          "contractVersion" as INT                       -> Set(Override, InBody({ case 1 => "1" }), InConstructor(2, 3, 4, 5, 6, 7)),
          "feeAssetId" as ASSET_ID.?                     -> Set(Override, InConstructor(3, 4, 5, 6, 7)),
          "atomicBadge" as OPTION(ATOMIC_BADGE)          -> Set(Override, InConstructor(4, 5, 6, 7)),
          "payments" as SHORT_LIST(CONTRACT_TRANSFER_IN) -> Set(Override, InConstructor(5, 6, 7)),
          proofsField,
          "sender_address" as SENDER_ADDRESS -> Transparent,
          "inputCommitment" as COMMITMENT    -> Set(Override, InConstructor(6), Validation(f => s"validateCommitment($f)")),
          "inputCommitmentOpt" as OPTION(COMMITMENT) -> Set(Override,
                                                            InConstructor(7),
                                                            Validation(f => s"$f.fold(Either.right[ValidationError, Unit](()))(validateCommitment)")),
          "contractEngine" as SHORT_STRING   -> Set(Override, InConstructor(7), Validation(f => s"validateContractEngine($f)")),
          "callFunc" as OPTION(SHORT_STRING) -> Set(Override, InConstructor(7)),
        ),
        ensures = Seq("validateSize"),
        versionToBlockchainFeatures = {
          case 3 =>
            Seq(BlockchainFeature.SponsoredFeesSupport)
          case 4 =>
            Seq(BlockchainFeature.SponsoredFeesSupport, BlockchainFeature.AtomicTransactionSupport)
          case 5 =>
            Seq(BlockchainFeature.SponsoredFeesSupport,
                BlockchainFeature.AtomicTransactionSupport,
                BlockchainFeature.ContractNativeTokenSupportAndPkiV1Support)
          case 6 =>
            Seq(
              BlockchainFeature.SponsoredFeesSupport,
              BlockchainFeature.AtomicTransactionSupport,
              BlockchainFeature.ContractNativeTokenSupportAndPkiV1Support,
              BlockchainFeature.ConfidentialDataInContractsSupport
            )
          case 7 =>
            Seq(
              BlockchainFeature.SponsoredFeesSupport,
              BlockchainFeature.AtomicTransactionSupport,
              BlockchainFeature.ContractNativeTokenSupportAndPkiV1Support,
              BlockchainFeature.ConfidentialDataInContractsSupport,
              BlockchainFeature.WasmContractsSupport
            )
        },
        sealedTraitExtensions = Seq("ExecutableTransaction"),
        caseClassCompanionExtensions = Seq("ContractTransactionValidation"),
        additionalImports = Set(
          "com.wavesenterprise.transaction.AtomicInnerTransaction",
          "com.wavesenterprise.transaction.PaymentsV1ToContract",
          "com.wavesenterprise.transaction.docker.ConfidentialDataInCallContractSupported",
          "com.wavesenterprise.transaction.docker.CommitmentValidations._",
          "com.wavesenterprise.transaction.wasm.WasmContractSupported",
          "com.wavesenterprise.transaction.wasm.WasmContractSupported._"
        ),
        versionExtensions = {
          case 4 => Seq("AtomicInnerTransaction")
          case 5 => Seq("AtomicInnerTransaction", "PaymentsV1ToContract")
          case 6 => Seq("AtomicInnerTransaction", "PaymentsV1ToContract", "ConfidentialDataInCallContractSupported")
          case 7 => Seq("AtomicInnerTransaction", "PaymentsV1ToContract", "OptionalConfidentialDataInCallContractSupported", "WasmContractSupported")
        }
      )

  case object ExecutedContractTransaction
      extends TxScheme(
        packageName = dockerTxPackage,
        typeId = 105,
        supportedVersions = Set(1, 2, 3, 4, 5),
        fields = Seq(
          senderField,
          "tx" as EXECUTABLE_TRANSACTION,
          "results" as SHORT_LIST(CONTRACT_DATA_ENTRY) -> Set(Override,
                                                              InBody({ case 5 => "resultsMap.mapping.values.flatten.toList" }),
                                                              Validation(f => s"validateResults($f)")),
          "resultsMap" as DATA_ENTRY_MAP                     -> Set(InConstructor(5), Validation(f => s"validateResultsMap($f)")),
          "resultsHash" as SHORT_BYTE_STR                    -> InConstructor(2, 3, 4, 5),
          "validationProofs" as SHORT_LIST(VALIDATION_PROOF) -> InConstructor(2, 3, 4, 5),
          timestampField,
          feeField.copy(versionToBodyValue = { case _ => "0" }),
          "atomicBadge" as OPTION(ATOMIC_BADGE) -> Set(Override, InBody({ case _ => "None" })),
          "assetOperations" as SHORT_LIST(CONTRACT_ASSET_OPERATION) -> Set(InConstructor(3, 4),
                                                                           InBody { case 5 => "assetOperationsMap.mapping.values.flatten.toList" }),
          proofsField,
          "sender_address" as SENDER_ADDRESS        -> Transparent,
          "readings" as SHORT_LIST(READ_DESCRIPTOR) -> InConstructor(4, 5),
          "readingsHash" as OPTION(READINGS_HASH)   -> Set(InConstructor(4, 5), Validation(f => s"validateReadingsHash($f)")),
          "outputCommitment" as COMMITMENT          -> Set(InConstructor(4), Validation(f => s"validateCommitment($f)")),
          "outputCommitmentOpt" as OPTION(COMMITMENT) -> Set(
            InConstructor(5),
            Validation(f => s"$f.fold(Either.right[ValidationError, Unit](()))(validateCommitment)")),
          "assetOperationsMap" as ASSET_OPERATIONS_MAP -> InConstructor(5),
          "statusCode" as INT                          -> InConstructor(5),
          "errorMessage" as OPTION(SHORT_STRING)       -> InConstructor(5),
        ),
        ensures = Seq("validateSize"),
        versionToBlockchainFeatures = {
          case 2 => Seq(BlockchainFeature.ContractValidationsSupport)
          case 3 => Seq(BlockchainFeature.ContractValidationsSupport, BlockchainFeature.ContractNativeTokenSupportAndPkiV1Support)
          case 4 => Seq(
              BlockchainFeature.ContractValidationsSupport,
              BlockchainFeature.ContractNativeTokenSupportAndPkiV1Support,
              BlockchainFeature.ConfidentialDataInContractsSupport
            )
          case 5 => Seq(
              BlockchainFeature.ContractValidationsSupport,
              BlockchainFeature.ContractNativeTokenSupportAndPkiV1Support,
              BlockchainFeature.ConfidentialDataInContractsSupport,
              BlockchainFeature.WasmContractsSupport
            )
        },
        caseClassCompanionExtensions = Seq("ContractTransactionValidation"),
        cacheSerializable = false,
        additionalImports = Set(
          "com.wavesenterprise.transaction.AtomicInnerTransaction",
          "com.wavesenterprise.transaction.ValidatorProvable",
          "com.wavesenterprise.transaction.docker.ConfidentialCallOutputSupport",
          "com.wavesenterprise.transaction.docker.ConfidentialCallOutputSupport._",
          "com.wavesenterprise.transaction.docker.CommitmentValidations._",
          "com.wavesenterprise.transaction.docker.assets.ContractAssetOperation.ContractAssetOperationMap",
          "com.wavesenterprise.transaction.docker.assets.AssetOperationsSupport",
          "com.wavesenterprise.transaction.docker.ContractTransactionEntryOps.DataEntryMap"
        ),
        sealedTraitExtensions = Seq("AtomicInnerTransaction"),
        unsupportedTypeScriptVersions = Set(1, 2, 3, 4),
        versionExtensions = {
          case 2 => Seq("ValidatorProvable")
          case 3 => Seq("ValidatorProvable", "AssetOperationsSupport")
          case 4 => Seq("ValidatorProvable", "AssetOperationsSupport", "ConfidentialCallOutputSupport")
          case 5 => Seq("ValidatorProvable", "AssetOperationsSupport", "OptionalConfidentialCallOutputSupport")
        },
        unusedImports = Set("com.wavesenterprise.serialization.ModelsBinarySerializer")
      )

  case object DisableContractTransaction
      extends TxScheme(
        packageName = dockerTxPackage,
        typeId = 106,
        supportedVersions = Set(1, 2, 3),
        fields = Seq(
          senderField,
          "contractId" as SHORT_BYTE_STR,
          feeField,
          timestampField,
          "feeAssetId" as ASSET_ID.?            -> Set(Override, InConstructor(2, 3)),
          "atomicBadge" as OPTION(ATOMIC_BADGE) -> Set(Override, InConstructor(3)),
          proofsField,
          "sender_address" as SENDER_ADDRESS -> Transparent
        ),
        versionToBlockchainFeatures = {
          case 2 => Seq(BlockchainFeature.SponsoredFeesSupport)
          case 3 => Seq(BlockchainFeature.SponsoredFeesSupport, BlockchainFeature.AtomicTransactionSupport)
        },
        additionalImports = Set("com.wavesenterprise.transaction.AtomicInnerTransaction"),
        versionExtensions = { case 3 => Seq("AtomicInnerTransaction") }
      )

  case object UpdateContractTransaction
      extends TxScheme(
        packageName = dockerTxPackage,
        typeId = 107,
        supportedVersions = Set(1, 2, 3, 4, 5, 6),
        fields = Seq(
          senderField,
          "contractId" as SHORT_BYTE_STR,
          "image" as SHORT_STRING     -> Set(InConstructor(1, 2, 3, 4, 5), Validation(f => s"validateImage($f)")),
          "imageHash" as SHORT_STRING -> Set(InConstructor(1, 2, 3, 4, 5), Validation(f => s"validateHash($f)")),
          feeField,
          timestampField,
          "feeAssetId" as ASSET_ID.?              -> Set(Override, InConstructor(2, 3, 4, 5, 6)),
          "atomicBadge" as OPTION(ATOMIC_BADGE)   -> Set(Override, InConstructor(3, 4, 5, 6)),
          "validationPolicy" as VALIDATION_POLICY -> Set(InConstructor(4, 5, 6), Validation(f => s"validateValidationPolicy($f)")),
          "apiVersion" as CONTRACT_API_VERSION    -> InConstructor(4, 5),
          proofsField,
          "sender_address" as SENDER_ADDRESS        -> Transparent,
          "groupParticipants" as SHORT_SET(ADDRESS) -> Set(Override, InConstructor(5, 6)),
          "groupOwners" as SHORT_SET(ADDRESS)       -> Set(Override, InConstructor(5, 6)),
          "storedContract" as STORED_CONTRACT       -> Set(Override, InConstructor(6), Validation(f => s"validateHash($f)")),
        ),
        versionToBlockchainFeatures = {
          case 2 => Seq(BlockchainFeature.SponsoredFeesSupport)
          case 3 => Seq(BlockchainFeature.SponsoredFeesSupport, BlockchainFeature.AtomicTransactionSupport)
          case 4 =>
            Seq(
              BlockchainFeature.SponsoredFeesSupport,
              BlockchainFeature.AtomicTransactionSupport,
              BlockchainFeature.ContractValidationsSupport
            )
          case 5 =>
            Seq(
              BlockchainFeature.SponsoredFeesSupport,
              BlockchainFeature.AtomicTransactionSupport,
              BlockchainFeature.ContractValidationsSupport,
              BlockchainFeature.ConfidentialDataInContractsSupport
            )
          case 6 =>
            Seq(
              BlockchainFeature.SponsoredFeesSupport,
              BlockchainFeature.AtomicTransactionSupport,
              BlockchainFeature.ContractValidationsSupport,
              BlockchainFeature.ConfidentialDataInContractsSupport,
              BlockchainFeature.WasmContractsSupport
            )
        },
        sealedTraitExtensions = Seq("ExecutableTransaction"),
        caseClassCompanionExtensions = Seq("ContractTransactionValidation"),
        additionalImports = Set(
          "com.wavesenterprise.transaction.AtomicInnerTransaction",
          "com.wavesenterprise.transaction.ValidationPolicySupport",
          "com.wavesenterprise.transaction.ApiVersionSupport",
          "com.wavesenterprise.transaction.docker.ConfidentialDataInUpdateContractSupported",
          "com.wavesenterprise.transaction.docker.DockerContractTransaction",
          "com.wavesenterprise.docker.StoredContract",
          "com.wavesenterprise.docker.StoredContractOps",
          "com.wavesenterprise.transaction.StoredContractSupported",
        ),
        versionExtensions = {
          case 1 => Seq("DockerContractTransaction")
          case 2 => Seq("DockerContractTransaction")
          case 3 => Seq("AtomicInnerTransaction", "DockerContractTransaction")
          case 4 => Seq("AtomicInnerTransaction", "ValidationPolicySupport", "ApiVersionSupport", "DockerContractTransaction")
          case 5 => Seq("AtomicInnerTransaction",
                        "ValidationPolicySupport",
                        "ApiVersionSupport",
                        "ConfidentialDataInUpdateContractSupported",
                        "DockerContractTransaction")
          case 6 => Seq("AtomicInnerTransaction", "ValidationPolicySupport", "ConfidentialDataInUpdateContractSupported", "StoredContractSupported")
        }
      )

  // endregion Docker

  // region Smart

  private val smartTxPackage = "com.wavesenterprise.transaction.smart"

  case object SetScriptTransaction
      extends TxScheme(
        packageName = smartTxPackage,
        typeId = 13,
        supportedVersions = Set(1),
        fields = Seq(
          "chainId" as BYTE -> Set(Validation(f => s"validateChainId($f)"), Override),
          "chainIdChecker" as CUSTOM_TYPE(scalaName = "Unit", protoName = "unit") -> InBody(
            { case _ => """require(chainId == AddressScheme.getAddressSchema.chainId, s"Wrong chainId ${chainId.toInt}")""" },
            explicitVal = true,
            excludeFromSealedTrait = true
          ),
          senderField,
          "script" as SCRIPT.? -> Json(f => s"$f.map(_.bytes().base64)"),
          "name" as SHORT_BYTE_ARRAY -> Set(Validation(f => s"validateName($f)"),
                                            Json(f => s"new String($f, StandardCharsets.UTF_8)"),
                                            TypeScriptLimit(128)),
          "description" as SHORT_BYTE_ARRAY -> Set(Validation(f => s"validateDescription($f)"),
                                                   Json(f => s"new String($f, StandardCharsets.UTF_8)"),
                                                   TypeScriptLimit(Short.MaxValue)),
          feeField,
          timestampField,
          proofsField,
          "sender_address" as SENDER_ADDRESS -> Transparent
        ),
        additionalImports = Set(
          "com.wavesenterprise.transaction.validation.validateChainId",
          "java.nio.charset.StandardCharsets",
          "com.wavesenterprise.state.ByteStr",
          "com.wavesenterprise.transaction.smart.SetScriptValidation._",
          "com.wavesenterprise.account.AddressScheme"
        ),
        versionToBlockchainFeatures = {
          case 1 =>
            Seq(
              BlockchainFeature.EvalFeature("""Seq((script.map(_.version.value) match {
                                            |  case Some(1) | None => BlockchainFeature.SmartAccounts
                                            |  case _              => BlockchainFeature.SmartAccountTrading
                                            |}))""".stripMargin)
            )
        }
      )

  // endregion Smart

  case object AtomicTransaction
      extends TxScheme(
        typeId = 120,
        supportedVersions = Set(1),
        fields = Seq(
          senderField,
          "miner" as new OPTION_BASE(PUBLIC_KEY_ACCOUNT) with SkipForProof -> NoTypeScript,
          "transactions" as SHORT_LIST(ATOMIC_INNER_TRANSACTION) -> Set(
            Json(f => s"$f.map(_.json())"),
            Validation(f => s"validateInnerTransactions($f, miner, sender.toAddress)")
          ),
          feeField.copy(versionToBodyValue = { case _ => "0" }),
          timestampField,
          proofsField,
          "sender_address" as SENDER_ADDRESS -> Transparent
        ),
        additionalImports = Set("com.wavesenterprise.transaction.validation.AtomicValidation._"),
        isContainer = true
      )

  override def values: immutable.IndexedSeq[TxScheme] = findValues
}
