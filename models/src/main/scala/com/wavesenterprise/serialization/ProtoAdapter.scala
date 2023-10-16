package com.wavesenterprise.serialization

import cats.implicits._
import com.google.common.io.ByteStreams.newDataOutput
import com.google.protobuf.ByteString
import com.google.protobuf.duration.{Duration => PbDuration}
import com.wavesenterprise.account.{Address, AddressOrAlias, Alias, PublicKeyAccount}
import com.wavesenterprise.acl.{OpType, PermissionOp, Role}
import com.wavesenterprise.crypto.internals.HashBytes
import com.wavesenterprise.crypto.internals.confidentialcontracts.Commitment
import com.wavesenterprise.docker.ContractApiVersion
import com.wavesenterprise.docker.validator.ValidationPolicy
import com.wavesenterprise.privacy.PolicyItemInfo
import com.wavesenterprise.protobuf.service.privacy.{PolicyItemInfoResponse, PolicyItemFileInfo => PbPolicyItemFileInfo}
import com.wavesenterprise.state._
import com.wavesenterprise.transaction.ValidationError.GenericError
import com.wavesenterprise.transaction._
import com.wavesenterprise.transaction.docker._
import com.wavesenterprise.transaction.docker.assets.ContractAssetOperation.{
  ContractBurnV1,
  ContractCancelLeaseV1,
  ContractIssueV1,
  ContractLeaseV1,
  ContractReissueV1,
  ContractTransferOutV1
}
import com.wavesenterprise.transaction.docker.assets.{ContractAssetOperation, ContractTransferInV1}
import com.wavesenterprise.transaction.protobuf.ValidationPolicy.Type
import com.wavesenterprise.transaction.protobuf.docker.{
  ContractBurn,
  ContractCancelLease,
  ContractIssue,
  ContractLease,
  ContractReissue,
  ContractTransferOut,
  ContractTransferIn => PbContractTransferIn
}
import com.wavesenterprise.transaction.protobuf.{
  AtomicBadge => PbAtomicBadge,
  ContractApiVersion => PbContractApiVersion,
  ContractAssetOperation => PbContractAssetOperation,
  DataEntry => PbDataEntry,
  ExecutableTransaction => PbExecutableTransaction,
  OpType => PbOpType,
  PermissionOp => PbPeprmissionOp,
  Role => PbRole,
  Transfer => PbTransfer,
  ValidationPolicy => PbValidationPolicy,
  ValidationProof => PbValidationProof
}
import com.wavesenterprise.transaction.transfer.ParsedTransfer

import scala.concurrent.duration.Duration

object ProtoAdapter {
  implicit def scalaDurationToProto(duration: Duration): PbDuration =
    PbDuration(duration.toMillis / 1000, (duration.toMillis % 1000 * 1000).toInt)

  def toProto(dataEntry: DataEntry[_]): PbDataEntry = {
    import PbDataEntry.Value._
    dataEntry match {
      case IntegerDataEntry(key, value) => PbDataEntry(key, IntValue(value))
      case StringDataEntry(key, value)  => PbDataEntry(key, StringValue(value))
      case BooleanDataEntry(key, value) => PbDataEntry(key, BoolValue(value))
      case BinaryDataEntry(key, value)  => PbDataEntry(key, BinaryValue(ByteString.copyFrom(value.arr)))
    }
  }

  def fromProto(protoDataEntry: PbDataEntry): Either[ValidationError, DataEntry[_]] = {
    import PbDataEntry.Value._
    protoDataEntry match {
      case PbDataEntry(key, IntValue(value), _)    => Right(IntegerDataEntry(key, value))
      case PbDataEntry(key, StringValue(value), _) => Right(StringDataEntry(key, value))
      case PbDataEntry(key, BoolValue(value), _)   => Right(BooleanDataEntry(key, value))
      case PbDataEntry(key, BinaryValue(value), _) => Right(BinaryDataEntry(key, ByteStr(value.toByteArray)))
      case PbDataEntry(key, Empty, _)              => Left(GenericError(s"Empty data entry value with key '$key'"))
    }
  }

  def toProto(tx: ExecutableTransaction): PbExecutableTransaction = {
    val pbTransaction = tx match {
      case callTx: CallContractTransaction     => PbExecutableTransaction.Transaction.CallContractTransaction(callTx.toInnerProto)
      case createTx: CreateContractTransaction => PbExecutableTransaction.Transaction.CreateContractTransaction(createTx.toInnerProto)
      case updateTx: UpdateContractTransaction => PbExecutableTransaction.Transaction.UpdateContractTransaction(updateTx.toInnerProto)
    }

    PbExecutableTransaction(tx.version, pbTransaction)
  }

  def fromProto(protoTx: PbExecutableTransaction): Either[ValidationError, ExecutableTransaction] = {
    protoTx.transaction match {
      case PbExecutableTransaction.Transaction.CreateContractTransaction(value) => CreateContractTransaction.fromProto(protoTx.version, value)
      case PbExecutableTransaction.Transaction.CallContractTransaction(value)   => CallContractTransaction.fromProto(protoTx.version, value)
      case PbExecutableTransaction.Transaction.UpdateContractTransaction(value) => UpdateContractTransaction.fromProto(protoTx.version, value)
      case PbExecutableTransaction.Transaction.Empty                            => Left(GenericError("Empty executable transaction"))
    }
  }

  def toProto(contractAssetOperation: ContractAssetOperation): PbContractAssetOperation = {
    contractAssetOperation match {
      case o: ContractAssetOperation.ContractIssueV1 =>
        val innerOperation = ContractIssue(
          o.assetId.base58,
          o.name,
          o.description,
          o.quantity,
          o.decimals,
          o.isReissuable,
          o.nonce
        )
        PbContractAssetOperation(PbContractAssetOperation.Operation.ContractIssue(innerOperation))
      case o: ContractAssetOperation.ContractReissueV1 =>
        val innerOperation = ContractReissue(
          o.assetId.base58,
          o.quantity,
          o.isReissuable
        )
        PbContractAssetOperation(PbContractAssetOperation.Operation.ContractReissue(innerOperation))
      case o: ContractAssetOperation.ContractBurnV1 =>
        val innerOperation = ContractBurn(
          o.assetId.map(_.base58),
          o.amount
        )
        PbContractAssetOperation(PbContractAssetOperation.Operation.ContractBurn(innerOperation))
      case o: ContractAssetOperation.ContractTransferOutV1 =>
        val innerOperation = ContractTransferOut(
          o.assetId.map(_.base58),
          o.recipient.stringRepr,
          o.amount
        )
        PbContractAssetOperation(PbContractAssetOperation.Operation.ContractTransferOut(innerOperation))

      case o: ContractAssetOperation.ContractLeaseV1 =>
        val innerOperation = ContractLease(
          o.leaseId.base58,
          o.nonce,
          o.recipient.stringRepr,
          o.amount
        )
        PbContractAssetOperation(PbContractAssetOperation.Operation.ContractLease(innerOperation))

      case o: ContractAssetOperation.ContractCancelLeaseV1 =>
        val innerOperation = ContractCancelLease(
          o.leaseId.base58,
        )
        PbContractAssetOperation(PbContractAssetOperation.Operation.ContractCancelLease(innerOperation))
    }

  }

  def fromProto(protoContractAssetOperation: PbContractAssetOperation): Either[ValidationError, ContractAssetOperation] = {
    protoContractAssetOperation.operation match {
      case PbContractAssetOperation.Operation.Empty =>
        Left(GenericError(s"Empty contract asset operation value"))
      case inner: PbContractAssetOperation.Operation.ContractIssue =>
        val value = inner.value
        ByteStr
          .decodeBase58(value.assetId)
          .toEither
          .leftMap(err => GenericError(s"Error decoding 'assetId' of ContractIssue asset operation: $err"))
          .map { decodedAssetId =>
            ContractIssueV1(
              decodedAssetId,
              value.name,
              value.description,
              value.quantity,
              value.decimals.toByte,
              value.isReissuable,
              value.nonce.toByte
            )
          }

      case inner: PbContractAssetOperation.Operation.ContractReissue =>
        val value = inner.value
        ByteStr
          .decodeBase58(value.assetId)
          .toEither
          .leftMap(err => GenericError(s"Error decoding 'assetId' of ContractReissue asset operation: $err"))
          .map { decodedAssetId =>
            ContractReissueV1(decodedAssetId, value.quantity, value.isReissuable)
          }

      case inner: PbContractAssetOperation.Operation.ContractBurn =>
        val value = inner.value
        value.assetId
          .traverse(str => ByteStr.decodeBase58(str).toEither)
          .leftMap(err => GenericError(s"Error decoding 'assetId' of ContractBurn asset operation: $err"))
          .map { assetIdOpt =>
            ContractBurnV1(assetIdOpt, value.amount)
          }

      case inner: PbContractAssetOperation.Operation.ContractTransferOut =>
        val value = inner.value
        for {
          assetIdOpt <- value.assetId
            .traverse(str => ByteStr.decodeBase58(str).toEither)
            .leftMap(err => GenericError(s"Error decoding 'assetId' of ContractTransferOut asset operation: $err"))
          recipient <- AddressOrAlias
            .fromString(value.recipient)
            .leftMap(ValidationError.fromCryptoError)
        } yield ContractTransferOutV1(assetIdOpt, recipient, value.amount)

      case inner: PbContractAssetOperation.Operation.ContractLease =>
        val value = inner.value

        for {
          leaseId <- ByteStr
            .decodeBase58(value.leaseId)
            .toEither
            .leftMap(err => GenericError(s"Error decoding 'leaseId' of ContractLease asset operation: $err"))

          recipient <- AddressOrAlias
            .fromString(value.recipient)
            .leftMap(ValidationError.fromCryptoError)
        } yield ContractLeaseV1(leaseId, value.nonce.toByte, recipient, value.amount)

      case inner: PbContractAssetOperation.Operation.ContractCancelLease =>
        val value = inner.value

        for {
          leaseId <- ByteStr
            .decodeBase58(value.leaseId)
            .toEither
            .leftMap(err => GenericError(s"Error decoding 'leaseId' of ContractCancelLease asset operation: $err"))
        } yield ContractCancelLeaseV1(leaseId)
    }
  }

  def toProto(contractTransferIn: ContractTransferInV1): PbContractTransferIn =
    PbContractTransferIn(
      contractTransferIn.assetId.map(_.base58),
      contractTransferIn.amount
    )

  def fromProto(protoContractTransferIn: PbContractTransferIn): Either[ValidationError, ContractTransferInV1] = {
    protoContractTransferIn.assetId
      .traverse(str => ByteStr.decodeBase58(str).toEither)
      .leftMap(err => GenericError(s"Error decoding 'assetId' of ContractTransferIn asset operation: $err"))
      .map(assetId => ContractTransferInV1(assetId, protoContractTransferIn.amount))
  }

  def toProto(validationProof: ValidationProof): PbValidationProof = {
    PbValidationProof(
      byteArrayToByteString(validationProof.validatorPublicKey.publicKey.getEncoded),
      byteArrayToByteString(validationProof.signature.arr)
    )
  }

  def fromProto(protoValidationProof: PbValidationProof): Either[ValidationError, ValidationProof] = {
    for {
      publicKey <- byteArrayFromProto(protoValidationProof.validatorPublicKey).map(PublicKeyAccount.apply)
      signature <- byteStrFromProto(protoValidationProof.signature)
    } yield ValidationProof(publicKey, signature)
  }

  def toProto(opType: OpType): PbOpType = {
    opType match {
      case OpType.Add    => PbOpType.ADD
      case OpType.Remove => PbOpType.REMOVE
    }
  }

  def fromProto(opType: PbOpType): Either[ValidationError, OpType] = {
    opType match {
      case PbOpType.ADD                 => Right(OpType.Add)
      case PbOpType.REMOVE              => Right(OpType.Remove)
      case PbOpType.UNKNOWN_OP_TYPE     => Left(GenericError(s"Unknown OpType"))
      case PbOpType.Unrecognized(value) => Left(GenericError(s"Unrecognized OpType '$value'"))
    }
  }

  def toProto(permissionOp: PermissionOp): PbPeprmissionOp = {
    PbPeprmissionOp(
      toProto(permissionOp.opType),
      Some(toProto(permissionOp.role)),
      permissionOp.timestamp,
      permissionOp.dueTimestampOpt
    )
  }

  def fromProto(protoPermissionOp: PbPeprmissionOp): Either[ValidationError, PermissionOp] = {
    for {
      opType <- fromProto(protoPermissionOp.opType)
      role   <- protoPermissionOp.role.fold(GenericError(s"Role cannot be empty").asLeft[Role])(fromProto)
    } yield PermissionOp(opType, role, protoPermissionOp.timestamp, protoPermissionOp.dueTimestamp)
  }

  def toProto(role: Role): PbRole = {
    PbRole(role.byte)
  }

  def fromProto(protoRole: PbRole): Either[GenericError, Role] = {
    Role.fromByte(protoRole.id.toByte)
  }

  def toProto(transfer: ParsedTransfer): PbTransfer = {
    PbTransfer(byteArrayToByteString(transfer.recipient.bytes.arr), transfer.amount)
  }

  def fromProto(protoTransfer: PbTransfer): Either[ValidationError, ParsedTransfer] = {
    addressOrAliasFromProto(protoTransfer.recipient)
      .map(ParsedTransfer(_, protoTransfer.amount))
  }

  def proofsToProto(value: Proofs): Seq[ByteString] = {
    value.proofs.map(str => byteArrayToByteString(str.arr))
  }

  def proofsFromProto(value: Seq[ByteString]): Either[ValidationError, Proofs] = {
    Right(Proofs(value.map(bytes => ByteStr(bytes.toByteArray))))
  }

  def toProto(badge: AtomicBadge): PbAtomicBadge = {
    PbAtomicBadge(badge.trustedSender.map(sender => ProtoAdapter.byteArrayToByteString(sender.bytes.arr)))
  }

  def fromProto(protoBadge: PbAtomicBadge): Either[ValidationError, AtomicBadge] =
    protoBadge.trustedSender.fold(Either.right[ValidationError, AtomicBadge](AtomicBadge(None))) { trustedSenderPb =>
      for {
        trustedSender <- ProtoAdapter.byteArrayFromProto(trustedSenderPb)
        address       <- Address.fromBytes(trustedSender).left.map(ValidationError.fromCryptoError)
      } yield AtomicBadge(Some(address))
    }

  def addressOrAliasFromProto(value: ByteString): Either[ValidationError, AddressOrAlias] =
    for {
      bytes             <- byteArrayFromProto(value)
      aliasWithPosition <- AddressOrAlias.fromBytes(bytes, 0).left.map(ValidationError.fromCryptoError)
      (alias, _) = aliasWithPosition
    } yield alias

  def addressFromProto(value: ByteString): Either[ValidationError, Address] =
    byteArrayFromProto(value).flatMap(Address.fromBytes(_).left.map(ValidationError.fromCryptoError))

  def aliasFromProto(value: ByteString): Either[ValidationError, Alias] =
    byteArrayFromProto(value).flatMap(Alias.fromBytes(_).left.map(ValidationError.fromCryptoError))

  def byteFromProto(value: Int): Either[GenericError, Byte] = {
    Either.cond(value.isValidByte, value.toByte, GenericError("Invalid byte value"))
  }

  def byteStrFromProto(value: ByteString): Either[ValidationError, ByteStr] = {
    Right(ByteStr(value.toByteArray))
  }

  def byteArrayFromProto(value: ByteString): Either[ValidationError, Array[Byte]] = {
    Right(value.toByteArray)
  }

  def byteArrayToByteString(value: Array[Byte]): ByteString = {
    ByteString.copyFrom(value)
  }

  def toProto(value: ValidationPolicy): PbValidationPolicy = {
    val `type` = value match {
      case ValidationPolicy.Any =>
        PbValidationPolicy.Type.Any(PbValidationPolicy.Any())
      case ValidationPolicy.Majority =>
        PbValidationPolicy.Type.Majority(PbValidationPolicy.Majority())
      case ValidationPolicy.MajorityWithOneOf(addresses) =>
        val addressesBytes = addresses.map(address => byteArrayToByteString(address.bytes.arr))
        val pbAddresses    = PbValidationPolicy.MajorityWithOneOf(addressesBytes)
        PbValidationPolicy.Type.MajorityWithOneOf(pbAddresses)
    }

    PbValidationPolicy(`type`)
  }

  def fromProto(value: PbValidationPolicy): Either[ValidationError, ValidationPolicy] =
    value.`type` match {
      case Type.Empty =>
        Left(GenericError(s"Empty validation policy value"))
      case Type.Any(_) =>
        Right(ValidationPolicy.Any)
      case Type.Majority(_) =>
        Right(ValidationPolicy.Majority)
      case Type.MajorityWithOneOf(addressesValue) =>
        addressesValue.addresses.toList
          .map(addressFromProto)
          .sequence
          .map(ValidationPolicy.MajorityWithOneOf)
    }

  def toProto(value: ContractApiVersion): PbContractApiVersion =
    PbContractApiVersion(value.majorVersion, value.minorVersion)

  def fromProto(value: PbContractApiVersion): Either[ValidationError, ContractApiVersion] =
    (Either.cond(value.majorVersion.isValidShort && value.majorVersion >= 0, value.majorVersion.toShort, GenericError(s"Invalid major version")),
     Either.cond(value.minorVersion.isValidShort && value.minorVersion >= 0, value.minorVersion.toShort, GenericError(s"Invalid minor version")))
      .mapN(ContractApiVersion(_, _))

  def toProto(info: PolicyItemInfo): PolicyItemInfoResponse = {
    val pbInfo = PbPolicyItemFileInfo(
      filename = info.info.filename,
      size = info.info.size,
      timestamp = info.info.timestamp,
      author = info.info.author,
      comment = info.info.comment
    )
    PolicyItemInfoResponse(
      senderAddress = info.sender,
      policyId = info.policy,
      info = Some(pbInfo),
      dataHash = info.hash
    )
  }

  def commitmentToProto(commitment: Commitment): ByteString = {
    ByteString.copyFrom(commitment.hash.arr)
  }

  def commitmentFromProto(byteString: ByteString): Either[ValidationError, Commitment] = Right {
    Commitment.fromHash(HashBytes(ByteStr(byteString.toByteArray)))
  }

  def toProto(readingsHash: ReadingsHash): ByteString = {
    ByteString.copyFrom(readingsHash.hash.arr)
  }

  def readingsHashFromProto(byteString: ByteString): Either[ValidationError, ReadingsHash] = Right {
    ReadingsHash(ByteStr(byteString.toByteArray))
  }

  def toProto(readDescriptor: ReadDescriptor): ByteString = ByteString.copyFrom {
    val output = newDataOutput()
    ReadDescriptor.writeBytes(readDescriptor, output)
    output.toByteArray
  }

  def readDescriptorFromProto(byteString: ByteString): Either[ValidationError, ReadDescriptor] = Right {
    ReadDescriptor.fromBytes(byteString.toByteArray, 0)._1
  }

}
