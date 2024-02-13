package com.wavesenterprise.transaction.docker.assets

import cats.implicits._
import com.google.common.io.ByteArrayDataOutput
import com.google.common.io.ByteStreams.newDataOutput
import com.wavesenterprise.account.AddressOrAlias
import com.wavesenterprise.crypto
import com.wavesenterprise.serialization.BinarySerializer
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.state.ByteStr.ByteStrFormat
import com.wavesenterprise.transaction.ValidationError.GenericError
import com.wavesenterprise.transaction.{AssetId, AssetIdLength}
import enumeratum.values.{ByteEnum, ByteEnumEntry}
import monix.eval.Coeval
import play.api.libs.json._

import scala.collection.immutable

trait Versioned {
  val version: Byte
}

/**
  * Defines the operation that a contract performs on the asset
  */
sealed trait ContractAssetOperation extends Versioned {
  def operationType: ContractAssetOperation.OperationType

  protected final def writeBytes(output: ByteArrayDataOutput): Unit = {
    writePrefixBytes(output)
    writeContractAssetOperationBytes(output)
  }

  protected final def writePrefixBytes(output: ByteArrayDataOutput): Unit = {
    output.writeByte(operationType.value)
    output.writeByte(version)
  }

  def writeContractAssetOperationBytes(output: ByteArrayDataOutput): Unit

  def bytes(): Coeval[Array[Byte]] = Coeval.eval {
    val output = newDataOutput()
    ContractAssetOperation.writeBytes(this, output)
    output.toByteArray
  }
}

object ContractAssetOperation {
  import com.wavesenterprise.serialization.AssetIdUtils._

  private val OperationTypeField = "operationType"

  type OperationType = ContractAssetOperationType

  sealed abstract class ContractAssetOperationType(val value: Byte, val name: String) extends ByteEnumEntry

  object ContractAssetOperationTypes extends ByteEnum[ContractAssetOperationType] {
    case object ContractTransferOutType extends ContractAssetOperationType(0, "transfer")
    case object ContractIssueType       extends ContractAssetOperationType(1, "issue")
    case object ContractReissueType     extends ContractAssetOperationType(2, "reissue")
    case object ContractBurnType        extends ContractAssetOperationType(3, "burn")
    case object ContractLeaseType       extends ContractAssetOperationType(4, "lease")
    case object ContractCancelLeaseType extends ContractAssetOperationType(5, "cancel-lease")
    case object ContractPaymentType     extends ContractAssetOperationType(6, "contract-payment")

    override def values: immutable.IndexedSeq[ContractAssetOperationType] = findValues

    def fromString(str: String): Either[GenericError, ContractAssetOperationType] = {
      values
        .find(_.name == str)
        .toRight(GenericError(s"ContractAssetOperationType type '$str' does not exist"))
    }
  }

  implicit val contractAssetOperationTypeFormat: Format[ContractAssetOperationType] = Format(
    {
      case JsString(str) =>
        ContractAssetOperationTypes
          .fromString(str)
          .fold(
            error => JsError(s"Error parsing '$OperationTypeField': ${error.err}"),
            parsed => JsSuccess(parsed)
          )
      case otherType =>
        JsError(s"Error parsing '$OperationTypeField': expected a string, but got '${otherType}'")
    },
    { case opType: ContractAssetOperationType => JsString(opType.name) }
  )

  private final def readContractAssetOperationPrefixFromBytes(bytes: Array[Byte], offset: Int): ((ContractAssetOperation.OperationType, Byte), Int) =
    ((ContractAssetOperation.ContractAssetOperationTypes.valuesToEntriesMap(bytes(offset)), bytes(offset + 1)), offset + 2)

  final case class ContractTransferOutV1 private (
      operationType: OperationType,
      version: Byte,
      assetId: Option[AssetId],
      recipient: AddressOrAlias,
      amount: Long
  ) extends ContractAssetOperation {
    override def writeContractAssetOperationBytes(output: ByteArrayDataOutput): Unit = {
      BinarySerializer.writeByteIterable(assetId, assetIdWriter, output)
      output.write(recipient.bytes.arr)
      output.writeLong(amount)
    }
  }

  object ContractTransferOutV1 {
    import com.wavesenterprise.serialization.json.AddressOrAliasJsonUtils._

    val operationType: OperationType = ContractAssetOperationTypes.ContractTransferOutType
    val version: Byte                = 1

    val format: Format[ContractTransferOutV1] = Json.format

    def apply(assetId: Option[AssetId], recipient: AddressOrAlias, amount: Long): ContractTransferOutV1 =
      ContractTransferOutV1(
        operationType = ContractTransferOutV1.operationType,
        version = 1,
        assetId = assetId,
        recipient = recipient,
        amount = amount
      )

    def fromBytes(bytes: Array[Byte], offset: Int): (ContractTransferOutV1, Int) = {
      val ((operationType, version), versionEnd) = readContractAssetOperationPrefixFromBytes(bytes, offset)
      val (assetId, assetIdEnd)                  = BinarySerializer.parseOption(bytes, assetIdReader, versionEnd)
      val (recipient, recipientEnd)              = AddressOrAlias.fromBytesUnsafe(bytes, assetIdEnd)
      val (amount, end)                          = BinarySerializer.parseLong(bytes, recipientEnd)

      (ContractTransferOutV1(operationType, version, assetId, recipient, amount), end)
    }
  }

  final case class ContractIssueV1 private (operationType: OperationType,
                                            version: Byte,
                                            assetId: ByteStr,
                                            name: String,
                                            description: String,
                                            quantity: Long,
                                            decimals: Byte,
                                            isReissuable: Boolean,
                                            nonce: Byte)
      extends ContractAssetOperation {
    override def writeContractAssetOperationBytes(output: ByteArrayDataOutput): Unit = {
      output.write(assetId.arr)
      BinarySerializer.writeShortString(name, output)
      BinarySerializer.writeShortString(description, output)
      output.writeLong(quantity)
      output.writeByte(decimals)
      output.writeByte(if (isReissuable) 1 else 0)
      output.writeByte(nonce)
    }
  }
  object ContractIssueV1 {
    val operationType: OperationType = ContractAssetOperationTypes.ContractIssueType
    val version: Byte                = 1

    val format: Format[ContractIssueV1] = Json.format

    def apply(assetId: ByteStr,
              name: String,
              description: String,
              quantity: Long,
              decimals: Byte,
              isReissuable: Boolean,
              nonce: Byte): ContractIssueV1 =
      ContractIssueV1(
        operationType = ContractIssueV1.operationType,
        version = 1,
        assetId = assetId,
        name = name,
        description = description,
        quantity = quantity,
        decimals = decimals,
        isReissuable = isReissuable,
        nonce = nonce
      )

    def fromBytes(bytes: Array[Byte], offset: Int): (ContractIssueV1, Int) = {
      val ((operationType, version), versionEnd) = readContractAssetOperationPrefixFromBytes(bytes, offset)
      val (assetId, assetIdEnd)                  = bytes.slice(versionEnd, versionEnd + AssetIdLength) -> (versionEnd + AssetIdLength)
      val (name, nameEnd)                        = BinarySerializer.parseShortString(bytes, assetIdEnd)
      val (description, descriptionEnd)          = BinarySerializer.parseShortString(bytes, nameEnd)
      val (quantity, quantityEnd)                = BinarySerializer.parseLong(bytes, descriptionEnd)
      val (decimals, decimalsEnd)                = bytes(quantityEnd)                                  -> (quantityEnd + 1)
      val (isReissuable, isReissuableEnd)        = (bytes(decimalsEnd) == 1)                           -> (decimalsEnd + 1)
      val (nonce, nonceEnd)                      = bytes(isReissuableEnd)                              -> (isReissuableEnd + 1)

      (ContractIssueV1(operationType, version, ByteStr(assetId), name, description, quantity, decimals, isReissuable, nonce), nonceEnd)
    }
  }

  final case class ContractReissueV1 private (operationType: OperationType, version: Byte, assetId: AssetId, quantity: Long, isReissuable: Boolean)
      extends ContractAssetOperation {
    override def writeContractAssetOperationBytes(output: ByteArrayDataOutput): Unit = {
      output.write(assetId.arr)
      output.writeLong(quantity)
      output.writeByte(if (isReissuable) 1 else 0)
    }
  }
  object ContractReissueV1 {
    val operationType: OperationType = ContractAssetOperationTypes.ContractReissueType
    val version: Byte                = 1

    val format: Format[ContractReissueV1] = Json.format

    def apply(assetId: AssetId, quantity: Long, isReissuable: Boolean): ContractReissueV1 =
      ContractReissueV1(operationType = ContractReissueV1.operationType,
                        version = 1,
                        assetId = assetId,
                        quantity = quantity,
                        isReissuable = isReissuable)

    def fromBytes(bytes: Array[Byte], offset: Int): (ContractReissueV1, Int) = {
      val ((operationType, version), versionEnd) = readContractAssetOperationPrefixFromBytes(bytes, offset)
      val (assetId, assetIdEnd)                  = ByteStr(bytes.slice(versionEnd, versionEnd + AssetIdLength)) -> (versionEnd + AssetIdLength)
      val (quantity, quantityEnd)                = BinarySerializer.parseLong(bytes, assetIdEnd)
      val (isReissuable, end)                    = (bytes(quantityEnd) == 1)                                    -> (quantityEnd + 1)

      (ContractReissueV1(operationType, version, assetId, quantity, isReissuable), end)
    }
  }

  final case class ContractBurnV1 private (operationType: OperationType, version: Byte, assetId: Option[AssetId], amount: Long)
      extends ContractAssetOperation {
    override def writeContractAssetOperationBytes(output: ByteArrayDataOutput): Unit = {
      BinarySerializer.writeByteIterable(assetId, assetIdWriter, output)
      output.writeLong(amount)
    }
  }
  object ContractBurnV1 {
    val operationType: OperationType = ContractAssetOperationTypes.ContractBurnType
    val version: Byte                = 1

    val format: Format[ContractBurnV1] = Json.format

    def apply(assetId: Option[AssetId], amount: Long): ContractBurnV1 =
      ContractBurnV1(operationType = ContractBurnV1.operationType, version = 1, assetId = assetId, amount = amount)

    def fromBytes(bytes: Array[Byte], offset: Int): (ContractBurnV1, Int) = {
      val ((operationType, version), versionEnd) = readContractAssetOperationPrefixFromBytes(bytes, offset)
      val (assetId, assetIdEnd)                  = BinarySerializer.parseOption(bytes, assetIdReader, versionEnd)
      val (amount, end)                          = BinarySerializer.parseLong(bytes, assetIdEnd)

      (ContractBurnV1(operationType, version, assetId, amount), end)
    }
  }

  final case class ContractLeaseV1 private (
      operationType: OperationType,
      version: Byte,
      leaseId: ByteStr,
      nonce: Byte,
      recipient: AddressOrAlias,
      amount: Long,
  ) extends ContractAssetOperation {
    override def writeContractAssetOperationBytes(output: ByteArrayDataOutput): Unit = {
      output.write(leaseId.arr)
      output.writeByte(nonce)
      output.write(recipient.bytes.arr)
      output.writeLong(amount)
    }
  }

  object ContractLeaseV1 {
    import com.wavesenterprise.serialization.json.AddressOrAliasJsonUtils._

    val operationType: OperationType = ContractAssetOperationTypes.ContractLeaseType
    val version: Byte                = 1

    val format: Format[ContractLeaseV1] = Json.format

    def apply(leaseId: ByteStr, nonce: Byte, recipient: AddressOrAlias, amount: Long): ContractLeaseV1 =
      ContractLeaseV1(operationType = ContractLeaseV1.operationType, version = 1, leaseId, nonce, recipient, amount)

    def fromBytes(bytes: Array[Byte], offset: Int): (ContractLeaseV1, Int) = {
      val ((operationType, version), versionEnd) = readContractAssetOperationPrefixFromBytes(bytes, offset)
      val (leaseId, leaseIdEnd)                  = bytes.slice(versionEnd, versionEnd + crypto.DigestSize) -> (versionEnd + crypto.DigestSize)
      val (nonce, nonceEnd)                      = bytes(leaseIdEnd)                                       -> (leaseIdEnd + 1)
      val (recipient, recipientEnd)              = AddressOrAlias.fromBytesUnsafe(bytes, nonceEnd)
      val (amount, end)                          = BinarySerializer.parseLong(bytes, recipientEnd)

      (ContractLeaseV1(operationType, version, ByteStr(leaseId), nonce, recipient, amount), end)
    }
  }

  final case class ContractCancelLeaseV1 private (
      operationType: OperationType,
      version: Byte,
      leaseId: ByteStr,
  ) extends ContractAssetOperation {
    override def writeContractAssetOperationBytes(output: ByteArrayDataOutput): Unit = {
      output.write(leaseId.arr)
    }
  }

  object ContractCancelLeaseV1 {
    val operationType: OperationType = ContractAssetOperationTypes.ContractCancelLeaseType
    val version: Byte                = 1

    val format: Format[ContractCancelLeaseV1] = Json.format

    def apply(leaseId: ByteStr): ContractCancelLeaseV1 =
      ContractCancelLeaseV1(operationType, version = 1, leaseId)

    def fromBytes(bytes: Array[Byte], offset: Int): (ContractCancelLeaseV1, Int) = {
      val ((operationType, version), versionEnd) = readContractAssetOperationPrefixFromBytes(bytes, offset)
      val (leaseId, end)                         = bytes.slice(versionEnd, versionEnd + crypto.DigestSize) -> (versionEnd + crypto.DigestSize)

      (ContractCancelLeaseV1(operationType, version, ByteStr(leaseId)), end)
    }
  }

  implicit val format: Format[ContractAssetOperation] = {
    Format(
      {
        case obj @ JsObject(fields) =>
          fields.get(OperationTypeField) match {
            case None =>
              JsError(s"Expected '$OperationTypeField' field")

            case Some(opTypeField) =>
              opTypeField
                .validate[ContractAssetOperationType]
                .flatMap {
                  case ContractAssetOperationTypes.ContractTransferOutType => ContractTransferOutV1.format.reads(obj)
                  case ContractAssetOperationTypes.ContractIssueType       => ContractIssueV1.format.reads(obj)
                  case ContractAssetOperationTypes.ContractReissueType     => ContractReissueV1.format.reads(obj)
                  case ContractAssetOperationTypes.ContractBurnType        => ContractBurnV1.format.reads(obj)
                  case ContractAssetOperationTypes.ContractLeaseType       => ContractLeaseV1.format.reads(obj)
                  case ContractAssetOperationTypes.ContractCancelLeaseType => ContractCancelLeaseV1.format.reads(obj)
                  case ContractAssetOperationTypes.ContractPaymentType     => ContractPaymentV1.format.reads(obj)
                }
          }

        case _ => JsError(s"Expected contract asset operation json object")
      },
      {
        case obj: ContractTransferOutV1 => ContractTransferOutV1.format.writes(obj)
        case obj: ContractIssueV1       => ContractIssueV1.format.writes(obj)
        case obj: ContractReissueV1     => ContractReissueV1.format.writes(obj)
        case obj: ContractBurnV1        => ContractBurnV1.format.writes(obj)
        case obj: ContractLeaseV1       => ContractLeaseV1.format.writes(obj)
        case obj: ContractCancelLeaseV1 => ContractCancelLeaseV1.format.writes(obj)
        case obj: ContractPaymentV1     => ContractPaymentV1.format.writes(obj)
      }
    )
  }

  def writeBytes(value: ContractAssetOperation, output: ByteArrayDataOutput): Unit = value.writeBytes(output)

  def fromBytes(bytes: Array[Byte], offset: Int): (ContractAssetOperation, Int) = readContractAssetOperationPrefixFromBytes(bytes, offset) match {
    case ((ContractAssetOperationTypes.ContractTransferOutType, 1), _) => ContractTransferOutV1.fromBytes(bytes, offset)
    case ((ContractAssetOperationTypes.ContractIssueType, 1), _)       => ContractIssueV1.fromBytes(bytes, offset)
    case ((ContractAssetOperationTypes.ContractReissueType, 1), _)     => ContractReissueV1.fromBytes(bytes, offset)
    case ((ContractAssetOperationTypes.ContractBurnType, 1), _)        => ContractBurnV1.fromBytes(bytes, offset)
    case ((ContractAssetOperationTypes.ContractLeaseType, 1), _)       => ContractLeaseV1.fromBytes(bytes, offset)
    case ((ContractAssetOperationTypes.ContractCancelLeaseType, 1), _) => ContractCancelLeaseV1.fromBytes(bytes, offset)
    case ((ContractAssetOperationTypes.ContractPaymentType, 1), _)     => ContractPaymentV1.fromBytes(bytes, offset)
  }

  final case class ContractPaymentV1 private (
      operationType: OperationType,
      version: Byte,
      assetId: Option[AssetId],
      recipient: ByteStr,
      amount: Long
  ) extends ContractAssetOperation {
    override def writeContractAssetOperationBytes(output: ByteArrayDataOutput): Unit = {
      BinarySerializer.writeByteIterable(assetId, assetIdWriter, output)
      BinarySerializer.writeShortByteStr(recipient, output)
      output.writeLong(amount)
    }
  }

  object ContractPaymentV1 {

    val operationType: OperationType = ContractAssetOperationTypes.ContractPaymentType
    val version: Byte                = 1

    val format: Format[ContractPaymentV1] = Json.format

    def apply(assetId: Option[AssetId], recipient: ByteStr, amount: Long): ContractPaymentV1 =
      ContractPaymentV1(
        operationType = ContractPaymentV1.operationType,
        version = 1,
        assetId = assetId,
        recipient = recipient,
        amount = amount
      )

    def fromBytes(bytes: Array[Byte], offset: Int): (ContractPaymentV1, Int) = {
      val ((operationType, version), versionEnd) = readContractAssetOperationPrefixFromBytes(bytes, offset)
      val (assetId, assetIdEnd)                  = BinarySerializer.parseOption(bytes, assetIdReader, versionEnd)
      val (recipient, recipientEnd)              = BinarySerializer.parseShortByteStr(bytes, assetIdEnd)
      val (amount, end)                          = BinarySerializer.parseLong(bytes, recipientEnd)

      (ContractPaymentV1(operationType, version, assetId, recipient, amount), end)
    }
  }

  type ContractAssetOperationList = List[ContractAssetOperation]

  object ContractAssetOperationList {
    def fromBytes(bytes: Array[Byte], offset: Int): (ContractAssetOperationList, Int) = {
      BinarySerializer.parseBigList(bytes, ContractAssetOperation.fromBytes, offset)
    }

    def writeBytes(operations: ContractAssetOperationList, output: ByteArrayDataOutput): Unit = {
      BinarySerializer.writeBigIterable(operations, ContractAssetOperation.writeBytes, output)
    }

    implicit val format: Format[ContractAssetOperationList] = Format(Reads.list, Writes.list)
  }

  case class ContractAssetOperationMap(
      mapping: Map[ByteStr, List[ContractAssetOperation]]
  )
  object ContractAssetOperationMap {
    def fromBytes(bytes: Array[Byte], offset: Int): (ContractAssetOperationMap, Int) = {
      val (contractCount, countOffset) = BinarySerializer.parseInt(bytes, offset)
      var readOffset                   = countOffset
      var readCount                    = 0
      val result                       = scala.collection.mutable.Buffer[(ByteStr, List[ContractAssetOperation])]()
      while (readCount < contractCount) {
        val (contractId, listOffset)        = BinarySerializer.parseShortByteStr(bytes, readOffset)
        val (assetOperationList, newOffset) = ContractAssetOperationList.fromBytes(bytes, listOffset)
        result += (contractId -> assetOperationList)
        readCount += 1
        readOffset = newOffset
      }
      ContractAssetOperationMap(result.toMap) -> readOffset
    }

    def writeBytes(operationMap: ContractAssetOperationMap, output: ByteArrayDataOutput): Unit = {
      BinarySerializer.writeInt(operationMap.mapping.size, output)
      operationMap.mapping.foreach {
        case (contractId, operations) =>
          BinarySerializer.writeShortByteStr(contractId, output)
          ContractAssetOperationList.writeBytes(operations, output)
      }
    }

    implicit val format: Format[ContractAssetOperationMap] = {
      Format(
        {
          case _ @JsObject(fields) =>
            fields.toList.map {
              case (stringKey, jsonValues) =>
                for {
                  contractId <- ByteStrFormat.reads(JsString(stringKey))
                  jsonValues <- ContractAssetOperationList.format.reads(jsonValues)
                } yield {
                  contractId -> jsonValues
                }
            }.traverse(_.asEither)
              .left.map(JsError.apply)
              .right.map(s => JsSuccess(ContractAssetOperationMap(s.toMap)))
              .fold(identity, identity)

          case _ => JsError("expected base58 -> list[ContractAssetOperation] object")
        },
        {
          map =>
            JsObject(map.mapping.toList.map(kv => kv._1.base58 -> ContractAssetOperationList.format.writes(kv._2)))
        }
      )
    }

  }
}
