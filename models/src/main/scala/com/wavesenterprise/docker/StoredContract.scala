package com.wavesenterprise.docker

import com.wavesenterprise.docker.StoredContract.DockerContract.{ApiVersion, Image, ImageHash}
import com.wavesenterprise.docker.StoredContract.WasmContract.{Bytecode, BytecodeHash}
import com.wavesenterprise.serialization.BinarySerializer.{Offset, parseBigByteArray, parseShortByteArray, parseShortString}
import play.api.libs.json._

import scala.util.hashing.MurmurHash3
import com.wavesenterprise.utils.Base64
import com.wavesenterprise.utils.DatabaseUtils.DOCKER_CONTRACT_TYPE

import java.nio.charset.StandardCharsets.UTF_8

trait StoredContract {
  def engine(): String
}

object StoredContract {

  case class DockerContract(
      image: String,
      imageHash: String,
      apiVersion: ContractApiVersion
  ) extends StoredContract {

    override def engine(): String = "docker"

    override def equals(other: Any): Boolean = {
      other match {
        case that: DockerContract =>
          eq(that) || (image == that.image && imageHash == that.imageHash && apiVersion == that.apiVersion)
        case _ =>
          false
      }
    }

    override def hashCode(): Int = MurmurHash3.orderedHash(Seq(image, imageHash, apiVersion))
  }

  object DockerContract {
    val Image      = "image"
    val ImageHash  = "imageHash"
    val ApiVersion = "apiVersion"

    implicit val DockerContractFormat: Format[DockerContract] = Json.format
  }

  case class WasmContract(
      bytecode: Array[Byte],
      bytecodeHash: String
  ) extends StoredContract {

    override def engine(): String = "wasm"

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
    val Bytecode     = "bytecode"
    val BytecodeHash = "bytecodeHash"

    implicit val WasmContractReads: Reads[WasmContract] = {
      case jsVal: JsValue if jsVal.asOpt[JsObject].isDefined =>
        val obj      = jsVal.as[JsObject]
        val bytecode = obj.value.get(Bytecode)
        val hashcode = obj.value.get(BytecodeHash)
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
        Bytecode     -> JsString(Base64.encode(contract.bytecode)),
        BytecodeHash -> JsString(contract.bytecodeHash)
      ))
    }

    implicit val WasmContractFormat: Format[WasmContract] = Format(WasmContractReads, WasmContractWrites)
  }

  implicit val StoredContractReads: Reads[StoredContract] = { jsValue =>
    val err = JsError(
      s"""unexpected storedContract json ${jsValue.toString()}.
      |  Expecting one of:
      |    "storedContract": {"bytecode": "...", "bytecodeHash": "..."}
      |    "storedContract": {"image": "...", "imageHash": "...", "apiVersion": "..."}""".stripMargin
    )

    jsValue.asOpt[JsObject] match {
      case Some(obj) =>
        val fields = obj.value.keySet
        val isValid =
          (
            fields.contains("apiVersion") && fields.size == 3
          ) || (
            !fields.contains("apiVersion") && fields.size == 2
          )
        if (!isValid) {
          err
        } else {
          val isWasm   = fields.contains(Bytecode) && fields.contains(BytecodeHash)
          val isDocker = fields.contains(Image) && fields.contains(ImageHash) && fields.contains(ApiVersion)
          if (isWasm) {
            WasmContract.WasmContractReads.reads(jsValue)
          } else if (isDocker) {
            DockerContract.DockerContractFormat.reads(jsValue)
          } else {
            err
          }
        }
      case None =>
        err
    }
  }

  implicit val StoredContractWrites: Writes[StoredContract] = {
    case c: WasmContract   => WasmContract.WasmContractWrites.writes(c)
    case c: DockerContract => DockerContract.DockerContractFormat.writes(c)
  }

  implicit val StoredContractFormat: Format[StoredContract] = Format(StoredContractReads, StoredContractWrites)

  def dockerContractReader(bytes: Array[Byte], offset: Offset): (DockerContract, Offset) = {
    val (imageBytes, imageOffset)  = parseShortByteArray(bytes, offset)
    val (hashBytes, hashOffset)    = parseShortByteArray(bytes, imageOffset)
    val (apiVersion, resultOffset) = ContractApiVersion.fromBytesUnsafe(bytes, hashOffset)
    DockerContract(
      image = new String(imageBytes, UTF_8),
      imageHash = new String(hashBytes, UTF_8),
      apiVersion = apiVersion
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

  def storedContractReader(bytes: Array[Byte], offset: Offset): (StoredContract, Offset) = {
    val isDockerContract = bytes(offset) == DOCKER_CONTRACT_TYPE
    if (isDockerContract) {
      dockerContractReader(bytes, offset + 1)
    } else {
      wasmContractReader(bytes, offset + 1)
    }
  }

}
