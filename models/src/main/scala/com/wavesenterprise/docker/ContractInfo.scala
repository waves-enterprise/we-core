package com.wavesenterprise.docker

import com.google.common.io.ByteStreams.newDataOutput
import com.google.common.primitives.Ints
import com.wavesenterprise.account.{Address, PublicKeyAccount}
import com.wavesenterprise.crypto
import com.wavesenterprise.docker.validator.ValidationPolicy
import com.wavesenterprise.serialization.{BinarySerializer, ModelsBinarySerializer}
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.ValidationPolicyAndApiVersionSupport
import com.wavesenterprise.transaction.docker.{
  ConfidentialDataInCreateContractSupported,
  ConfidentialDataInUpdateContractSupported,
  CreateContractTransaction,
  UpdateContractTransaction
}
import com.wavesenterprise.utils.DatabaseUtils.ByteArrayDataOutputExt
import monix.eval.Coeval
import play.api.libs.json._

import scala.util.hashing.MurmurHash3

case class ContractInfo(creator: Coeval[PublicKeyAccount],
                        contractId: ByteStr,
                        image: String,
                        imageHash: String,
                        version: Int,
                        active: Boolean,
                        validationPolicy: ValidationPolicy = ValidationPolicy.Default,
                        apiVersion: ContractApiVersion = ContractApiVersion.Initial,
                        isConfidential: Boolean = false,
                        groupParticipants: Set[Address] = Set(),
                        groupOwners: Set[Address] = Set()) {

  def isTheSameImage(other: ContractInfo): Boolean = {
    image == other.image && imageHash == other.imageHash
  }

  override def equals(that: Any): Boolean =
    that match {
      case that: ContractInfo =>
        eq(that) ||
          (creator() == that.creator() &&
            contractId == that.contractId &&
            image == that.image &&
            imageHash == that.imageHash &&
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
                                                             image,
                                                             imageHash,
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

  implicit val PublicKeyReads: Reads[PublicKeyAccount] = {
    case JsString(s) => PublicKeyAccount.fromBase58String(s).fold(e => JsError(e.message), JsSuccess(_))
    case _           => JsError("Expected string value for public key value")
  }

  implicit val PublicKeyFormat: Format[PublicKeyAccount] = Format(PublicKeyReads, PublicKeyAccount.Writes)
  implicit val LazyPublicKeyFormat: Format[Coeval[PublicKeyAccount]] =
    Format.invariantFunctorFormat.inmap(PublicKeyFormat, Coeval.pure[PublicKeyAccount], _.apply())
  implicit val ContractInfoFormat: OFormat[ContractInfo] = Json.format

  def apply(tx: CreateContractTransaction): ContractInfo = {
    val validationPolicy = tx match {
      case tx: ValidationPolicyAndApiVersionSupport => tx.validationPolicy
      case _                                        => ValidationPolicy.Default
    }

    val apiVersion = tx match {
      case tx: ValidationPolicyAndApiVersionSupport => tx.apiVersion
      case _                                        => ContractApiVersion.Initial
    }

    val (isConfidential, groupParticipants, groupOwners): (Boolean, Set[Address], Set[Address]) = {
      tx match {
        case confidentialTx: ConfidentialDataInCreateContractSupported =>
          (confidentialTx.isConfidential, confidentialTx.groupParticipants, confidentialTx.groupOwners)
        case _ =>
          (false, Set(), Set())
      }
    }

    ContractInfo(
      Coeval.pure(tx.sender),
      tx.contractId,
      tx.image,
      tx.imageHash,
      FirstVersion,
      active = true,
      validationPolicy = validationPolicy,
      apiVersion = apiVersion,
      isConfidential,
      groupParticipants,
      groupOwners
    )
  }

  def apply(tx: UpdateContractTransaction, contractInfo: ContractInfo): ContractInfo = {
    val validationPolicy = tx match {
      case tx: ValidationPolicyAndApiVersionSupport => tx.validationPolicy
      case _                                        => contractInfo.validationPolicy
    }

    val apiVersion = tx match {
      case tx: ValidationPolicyAndApiVersionSupport => tx.apiVersion
      case _                                        => ContractApiVersion.Initial
    }
    val (groupParticipants, groupOwners): (Set[Address], Set[Address]) = {
      tx match {
        case confidentialTx: ConfidentialDataInUpdateContractSupported =>
          (confidentialTx.groupParticipants, confidentialTx.groupOwners)
        case _ =>
          (contractInfo.groupParticipants, contractInfo.groupOwners)
      }
    }

    contractInfo.copy(
      image = tx.image,
      imageHash = tx.imageHash,
      version = contractInfo.version + 1,
      validationPolicy = validationPolicy,
      apiVersion = apiVersion,
      groupParticipants = groupParticipants,
      groupOwners = groupOwners
    )
  }

  def toBytes(contractInfo: ContractInfo): Array[Byte] = {

    import contractInfo._
    val ndo = newDataOutput()
    ndo.writePublicKey(creator())
    ndo.writeBytes(contractId.arr)
    ndo.writeString(image)
    ndo.writeString(imageHash)
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

    val (creatorBytes, creatorEnd)                = bytes.take(crypto.KeyLength)                                             -> crypto.KeyLength
    val creator                                   = Coeval.evalOnce(PublicKeyAccount(creatorBytes))
    val (contractId, contractIdEnd)               = BinarySerializer.parseShortByteStr(bytes, creatorEnd)
    val (image, imageEnd)                         = BinarySerializer.parseShortString(bytes, contractIdEnd)
    val (imageHash, imageHashEnd)                 = BinarySerializer.parseShortString(bytes, imageEnd)
    val (version, versionEnd)                     = Ints.fromByteArray(bytes.slice(imageHashEnd, imageHashEnd + Ints.BYTES)) -> (imageHashEnd + Ints.BYTES)
    val (active, activeEnd)                       = (bytes(versionEnd) == 1)                                                 -> (versionEnd + 1)
    val (validationPolicy, validationPolicyEnd)   = ValidationPolicy.fromBytesUnsafe(bytes, activeEnd)
    val (apiVersion, apiVersionEnd)               = ContractApiVersion.fromBytesUnsafe(bytes, validationPolicyEnd)
    val (isConfidential, isConfidentialEnd)       = (bytes(apiVersionEnd) == 1)                                              -> (apiVersionEnd + 1)
    val (groupParticipants, groupParticipantsEnd) = ModelsBinarySerializer.parseAddressesSet(bytes, isConfidentialEnd)
    val (groupOwners, _)                          = ModelsBinarySerializer.parseAddressesSet(bytes, groupParticipantsEnd)

    ContractInfo(creator, contractId, image, imageHash, version, active, validationPolicy, apiVersion, isConfidential, groupParticipants, groupOwners)
  }
}
