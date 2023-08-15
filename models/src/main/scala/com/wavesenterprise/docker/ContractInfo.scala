package com.wavesenterprise.docker

import com.google.common.io.ByteStreams.newDataOutput
import com.google.common.primitives.Ints
import com.wavesenterprise.account.{Address, PublicKeyAccount}
import com.wavesenterprise.crypto
import com.wavesenterprise.docker.ContractInfo.DockerContract.DockerContractFormat
import com.wavesenterprise.docker.ContractInfo.StoredContract
import com.wavesenterprise.docker.ContractInfo.WasmContract._
import com.wavesenterprise.docker.validator.ValidationPolicy
import com.wavesenterprise.serialization.BinarySerializer.{Offset, parseBigByteArray, parseShortByteArray, parseShortString}
import com.wavesenterprise.serialization.{BinarySerializer, ModelsBinarySerializer}
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.docker._
import com.wavesenterprise.transaction.{ApiVersionSupport, ValidationPolicySupport}
import com.wavesenterprise.utils.Base64
import com.wavesenterprise.utils.DatabaseUtils.{ByteArrayDataOutputExt, DOCKER_CONTRACT_TYPE}
import monix.eval.Coeval
import play.api.libs.json._

import java.nio.charset.StandardCharsets.UTF_8
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

  trait StoredContract

  case class DockerContract(
      image: String,
      imageHash: String
  ) extends StoredContract {
    override def equals(other: Any): Boolean = {
      other match {
        case that: DockerContract =>
          eq(that) || (image == that.image && imageHash == that.imageHash)
        case _ =>
          false
      }
    }

    override def hashCode(): Int = MurmurHash3.orderedHash(Seq(image, imageHash))
  }

  object DockerContract {
    implicit val DockerContractFormat: Format[DockerContract] = Json.format
  }

  case class WasmContract(
      bytecode: Array[Byte],
      bytecodeHash: String
  ) extends StoredContract {

    override def equals(other: Any): Boolean = {
      other match {
        case that: WasmContract =>
          eq(that) || ((bytecode sameElements that.bytecode) && bytecodeHash == that.bytecodeHash)
        case _ =>
          false
      }
    }

    override def hashCode(): Int = MurmurHash3.orderedHash(Seq(bytecode, bytecodeHash))
  }

  object WasmContract {
    val BYTECODE      = "bytecode"
    val BYTECODE_HASH = "bytecodeHash"

    implicit val WasmContractReads: Reads[WasmContract] = {
      case jsVal: JsValue if jsVal.asOpt[JsObject].isDefined =>
        val obj      = jsVal.as[JsObject]
        val bytecode = obj.value.get(BYTECODE)
        val hashcode = obj.value.get(BYTECODE_HASH)
        if (bytecode.isEmpty || hashcode.isEmpty) {
          JsError("Expected jsobject value for wasm bytecode")
        } else {
          Base64.decode(bytecode.get.as[String])
            .map(WasmContract(_, hashcode.get.as[String]))
            .fold(e => JsError(e.getMessage), JsSuccess(_))
        }
      case _ => JsError("Expected jsobject value for wasm bytecode")
    }

    implicit val WasmContractWrites: Writes[WasmContract] = { contract =>
      JsObject(Seq(
        BYTECODE      -> JsString(Base64.encode(contract.bytecode)),
        BYTECODE_HASH -> JsString(contract.bytecodeHash)
      ))
    }

    implicit val WasmContractFormat: Format[WasmContract] = Format(WasmContractReads, WasmContractWrites)
  }

  val FirstVersion: Int = 1

  implicit val LazyPublicKeyFormat: Format[Coeval[PublicKeyAccount]] =
    Format.invariantFunctorFormat.inmap(PublicKeyAccount.PublicKeyAccountFormat, Coeval.pure[PublicKeyAccount], _.apply())

  implicit val StoredContractReads: Reads[StoredContract] = { jsValue =>
    val wasm   = WasmContractReads.reads(jsValue)
    val docker = DockerContractFormat.reads(jsValue)
    wasm.fold(
      wasmErr =>
        docker.fold(
          dockerErr => JsError(JsError.merge(wasmErr, dockerErr)),
          _ => docker
        ),
      _ => wasm
    )
  }

  implicit val StoredContractWrites: Writes[StoredContract] = {
    case c: WasmContract   => WasmContractWrites.writes(c)
    case c: DockerContract => DockerContractFormat.writes(c)
  }

  implicit val StoredContractFormat: Format[StoredContract] = Format(StoredContractReads, StoredContractWrites)
  implicit val ContractInfoFormat: OFormat[ContractInfo]    = Json.format

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
          WasmContract(createContractTransactionV7.bytecode, createContractTransactionV7.bytecodeHash),
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
          (Set(), Set())
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
      case updateTxWasm: UpdateContractTransactionV6 => contractInfo.copy(
          Coeval.pure(tx.sender),
          contractId = tx.contractId,
          WasmContract(updateTxWasm.bytecode, updateTxWasm.bytecodeHash),
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
    val (contract, contractEnd)                   = contractImageReader(bytes, contractIdEnd)
    val (version, versionEnd)                     = Ints.fromByteArray(bytes.slice(contractEnd, contractEnd + Ints.BYTES)) -> (contractEnd + Ints.BYTES)
    val (active, activeEnd)                       = (bytes(versionEnd) == 1)                                               -> (versionEnd + 1)
    val (validationPolicy, validationPolicyEnd)   = ValidationPolicy.fromBytesUnsafe(bytes, activeEnd)
    val (apiVersion, apiVersionEnd)               = ContractApiVersion.fromBytesUnsafe(bytes, validationPolicyEnd)
    val (isConfidential, isConfidentialEnd)       = (bytes(apiVersionEnd) == 1)                                            -> (apiVersionEnd + 1)
    val (groupParticipants, groupParticipantsEnd) = ModelsBinarySerializer.parseAddressesSet(bytes, isConfidentialEnd)
    val (groupOwners, _)                          = ModelsBinarySerializer.parseAddressesSet(bytes, groupParticipantsEnd)

    ContractInfo(creator, contractId, contract, version, active, validationPolicy, apiVersion, isConfidential, groupParticipants, groupOwners)
  }

  def dockerContractReader(bytes: Array[Byte], offset: Offset): (DockerContract, Offset) = {
    val (imageBytes, imageOffset) = parseShortByteArray(bytes, offset)
    val (hashBytes, resultOffset) = parseShortByteArray(bytes, imageOffset)
    DockerContract(
      image = new String(imageBytes, UTF_8),
      imageHash = new String(hashBytes, UTF_8)
    ) -> resultOffset
  }

  def wasmContractReader(bytes: Array[Byte], offset: Offset): (WasmContract, Offset) = {
    val (bytecode, bytecodeOffset) = parseBigByteArray(bytes, offset)
    val (hashString, resultOffset) = parseShortString(bytes, bytecodeOffset)
    WasmContract(
      bytecode = bytecode,
      bytecodeHash = hashString
    ) -> resultOffset
  }

  def contractImageReader(bytes: Array[Byte], offset: Offset): (StoredContract, Offset) = {
    val isDockerContract = bytes(offset) == DOCKER_CONTRACT_TYPE
    if (isDockerContract) {
      dockerContractReader(bytes, offset + 1)
    } else {
      wasmContractReader(bytes, offset + 1)
    }
  }
}
