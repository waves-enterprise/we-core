package com.wavesenterprise.docker

import com.google.common.io.ByteStreams.newDataOutput
import com.google.common.primitives.Ints
import com.wavesenterprise.account.{Address, PublicKeyAccount}
import com.wavesenterprise.crypto
import com.wavesenterprise.docker.StoredContract.{DockerContract, storedContractReader}
import com.wavesenterprise.docker.validator.ValidationPolicy
import com.wavesenterprise.serialization.{BinarySerializer, ModelsBinarySerializer}
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.docker._
import com.wavesenterprise.transaction.{ApiVersionSupport, ValidationPolicySupport}
import com.wavesenterprise.transaction.docker.{
  ConfidentialDataInCreateContractSupported,
  ConfidentialDataInUpdateContractSupported,
  CreateContractTransaction,
  UpdateContractTransaction
}
import com.wavesenterprise.utils.DatabaseUtils.ByteArrayDataOutputExt
import monix.eval.Coeval

import scala.util.hashing.MurmurHash3

case class ContractInfo(creator: Coeval[PublicKeyAccount],
                        contractId: ByteStr,
                        storedContract: StoredContract,
                        version: Int,
                        active: Boolean,
                        validationPolicy: ValidationPolicy = ValidationPolicy.Default,
                        apiVersion: ContractApiVersion = ContractApiVersion.Initial,
                        isConfidential: Boolean = false,
                        groupParticipants: Set[Address] = Set(),
                        groupOwners: Set[Address] = Set()) {

  def isTheSameImage(other: ContractInfo): Boolean = {
    storedContract == other.storedContract
  }

  override def equals(that: Any): Boolean =
    that match {
      case that: ContractInfo =>
        eq(that) ||
          (creator() == that.creator() &&
            contractId == that.contractId &&
            storedContract.equals(that.storedContract) &&
            version == that.version &&
            active == that.active &&
            validationPolicy == that.validationPolicy &&
            apiVersion == that.apiVersion &&
            isConfidential == that.isConfidential &&
            groupParticipants == that.groupParticipants &&
            groupOwners == groupOwners)
      case _ => false
    }

  override def hashCode(): Int = MurmurHash3.orderedHash(Seq(creator(),
                                                             contractId,
                                                             storedContract,
                                                             version,
                                                             active,
                                                             validationPolicy,
                                                             apiVersion,
                                                             isConfidential,
                                                             groupParticipants,
                                                             groupOwners))
}

//noinspection UnstableApiUsage
object ContractInfo {

  val FirstVersion: Int = 1

  def apply(tx: CreateContractTransaction): ContractInfo = {
    val validationPolicy = tx match {
      case tx: ValidationPolicySupport => tx.validationPolicy
      case _                           => ValidationPolicy.Default
    }

    val apiVersion = tx match {
      case tx: ApiVersionSupport => tx.apiVersion
      case _                     => ContractApiVersion.Initial
    }

    val (isConfidential, groupParticipants, groupOwners): (Boolean, Set[Address], Set[Address]) = {
      tx match {
        case confidentialTx: ConfidentialDataInCreateContractSupported =>
          (confidentialTx.isConfidential, confidentialTx.groupParticipants, confidentialTx.groupOwners)
        case _ =>
          (false, Set(), Set())
      }
    }

    tx match {
      case createTxWithImage: DockerContractTransaction => ContractInfo(
          Coeval.pure(tx.sender),
          createTxWithImage.contractId,
          DockerContract(createTxWithImage.image, createTxWithImage.imageHash),
          FirstVersion,
          active = true,
          validationPolicy = validationPolicy,
          apiVersion = apiVersion,
          isConfidential = isConfidential,
          groupParticipants = groupParticipants,
          groupOwners = groupOwners
        )
      case createContractTransactionV7: CreateContractTransactionV7 =>
        ContractInfo(
          Coeval.pure(tx.sender),
          createContractTransactionV7.contractId,
          createContractTransactionV7.storedContract,
          FirstVersion,
          active = true,
          validationPolicy = validationPolicy,
          apiVersion = apiVersion,
          isConfidential = isConfidential,
          groupParticipants = groupParticipants,
          groupOwners = groupOwners
        )
    }
  }

  def apply(tx: UpdateContractTransaction, contractInfo: ContractInfo): ContractInfo = {
    val validationPolicy = tx match {
      case tx: ValidationPolicySupport => tx.validationPolicy
      case _                           => contractInfo.validationPolicy
    }

    val apiVersion = tx match {
      case tx: ApiVersionSupport => tx.apiVersion
      case _                     => ContractApiVersion.Initial
    }
    val (groupParticipants, groupOwners): (Set[Address], Set[Address]) = {
      tx match {
        case confidentialTx: ConfidentialDataInUpdateContractSupported =>
          (confidentialTx.groupParticipants, confidentialTx.groupOwners)
        case _ =>
          (contractInfo.groupParticipants, contractInfo.groupOwners)
      }
    }

    tx match {
      case updateTxWithImage: DockerContractTransaction => contractInfo.copy(
          Coeval.pure(tx.sender),
          contractId = tx.contractId,
          DockerContract(updateTxWithImage.image, updateTxWithImage.imageHash),
          version = contractInfo.version + 1,
          groupParticipants = groupParticipants,
          groupOwners = groupOwners,
          validationPolicy = validationPolicy,
          apiVersion = apiVersion
        )
      case updateTx: UpdateContractTransactionV6 => contractInfo.copy(
          Coeval.pure(tx.sender),
          contractId = tx.contractId,
          storedContract = updateTx.storedContract,
          version = contractInfo.version + 1,
          groupParticipants = groupParticipants,
          groupOwners = groupOwners,
          validationPolicy = validationPolicy
        )
    }
  }

  def toBytes(contractInfo: ContractInfo): Array[Byte] = {

    import contractInfo._
    val ndo = newDataOutput()
    ndo.writePublicKey(creator())
    ndo.writeBytes(contractId.arr)
    ndo.writeStoredContract(storedContract)
    ndo.writeInt(version)
    ndo.writeBoolean(active)
    ndo.write(contractInfo.validationPolicy.bytes)
    ndo.write(contractInfo.apiVersion.bytes)
    ndo.writeBoolean(contractInfo.isConfidential)
    ModelsBinarySerializer.writeAddresses(contractInfo.groupParticipants, ndo)
    ModelsBinarySerializer.writeAddresses(contractInfo.groupOwners, ndo)

    ndo.toByteArray
  }

  def fromBytes(bytes: Array[Byte]): ContractInfo = {

    val (creatorBytes, creatorEnd)                = bytes.take(crypto.KeyLength)                                           -> crypto.KeyLength
    val creator                                   = Coeval.evalOnce(PublicKeyAccount(creatorBytes))
    val (contractId, contractIdEnd)               = BinarySerializer.parseShortByteStr(bytes, creatorEnd)
    val (contract, contractEnd)                   = storedContractReader(bytes, contractIdEnd)
    val (version, versionEnd)                     = Ints.fromByteArray(bytes.slice(contractEnd, contractEnd + Ints.BYTES)) -> (contractEnd + Ints.BYTES)
    val (active, activeEnd)                       = (bytes(versionEnd) == 1)                                               -> (versionEnd + 1)
    val (validationPolicy, validationPolicyEnd)   = ValidationPolicy.fromBytesUnsafe(bytes, activeEnd)
    val (apiVersion, apiVersionEnd)               = ContractApiVersion.fromBytesUnsafe(bytes, validationPolicyEnd)
    val (isConfidential, isConfidentialEnd)       = (bytes(apiVersionEnd) == 1)                                            -> (apiVersionEnd + 1)
    val (groupParticipants, groupParticipantsEnd) = ModelsBinarySerializer.parseAddressesSet(bytes, isConfidentialEnd)
    val (groupOwners, _)                          = ModelsBinarySerializer.parseAddressesSet(bytes, groupParticipantsEnd)

    ContractInfo(creator, contractId, contract, version, active, validationPolicy, apiVersion, isConfidential, groupParticipants, groupOwners)
  }

}
