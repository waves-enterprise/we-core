package com.wavesenterprise.docker

import com.wavesenterprise.docker.StoredContract.WasmContract.{BYTECODE, BYTECODE_HASH}
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
      imageHash: String
  ) extends StoredContract {

    override def engine(): String = "docker"

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

  implicit val StoredContractReads: Reads[StoredContract] = { jsValue =>
    val err = JsError(s"unexpected contract json ${jsValue.toString()}")

    jsValue.asOpt[JsObject] match {
      case Some(obj) =>
        val fields  = obj.value.keySet
        val isValid = fields.size == 2
        if (!isValid) {
          err
        } else {
          val isWasm   = fields.contains(BYTECODE) && fields.contains(BYTECODE_HASH)
          val isDocker = fields.contains("image") && fields.contains("imageHash")
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

  def storedContractReader(bytes: Array[Byte], offset: Offset): (StoredContract, Offset) = {
    val isDockerContract = bytes(offset) == DOCKER_CONTRACT_TYPE
    if (isDockerContract) {
      dockerContractReader(bytes, offset + 1)
    } else {
      wasmContractReader(bytes, offset + 1)
    }
  }

}
