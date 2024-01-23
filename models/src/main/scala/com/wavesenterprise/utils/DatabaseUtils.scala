package com.wavesenterprise.utils

import com.google.common.io.{ByteArrayDataInput, ByteArrayDataOutput}
import com.wavesenterprise.account.PublicKeyAccount
import com.wavesenterprise.crypto
import com.wavesenterprise.docker.StoredContract.{DockerContract, WasmContract}
import com.wavesenterprise.docker.StoredContract
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.smart.script.{Script, ScriptReader}

import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.Seq

object DatabaseUtils {

  val DOCKER_CONTRACT_TYPE: Byte = 0

  val WASM_CONTRACT_TYPE: Byte = 1

  implicit class ByteArrayDataOutputExt(val output: ByteArrayDataOutput) extends AnyVal {
    def writeByteStr(s: ByteStr): Unit = {
      output.write(s.arr)
    }

    def writeBigInt(v: BigInt): Unit = {
      val b = v.toByteArray
      require(b.length <= Byte.MaxValue)
      output.writeByte(b.length)
      output.write(b)
    }

    def writeBytes(value: Array[Byte]): Unit = {
      output.writeShort(value.length.ensuring(_.isValidShort))
      output.write(value)
    }

    def writeString(s: String): Unit = {
      writeBytes(s.getBytes(UTF_8))
    }

    def writeScriptOption(v: Option[Script]): Unit = {
      output.writeBoolean(v.isDefined)
      v.foreach { s =>
        val b = s.bytes().arr
        output.writeShort(b.length)
        output.write(b)
      }
    }

    def writeStoredContract(contract: StoredContract): Unit = {
      contract match {
        case DockerContract(image, imageHash) =>
          output.writeByte(DOCKER_CONTRACT_TYPE)
          for {
            i <- Seq(image.getBytes(UTF_8).array, imageHash.getBytes(UTF_8).array)
          } {
            output.writeBytes(i)
          }

        case WasmContract(bytecode, bytecodeHash) =>
          output.writeByte(WASM_CONTRACT_TYPE)
          output.writeInt(bytecode.length)
          output.write(bytecode)
          output.writeString(bytecodeHash)
      }

    }

    def writePublicKey(pka: PublicKeyAccount): Unit = {
      output.write(pka.publicKey.getEncoded)
    }
  }

  implicit class ByteArrayDataInputExt(val input: ByteArrayDataInput) extends AnyVal {
    def readBigInt(): BigInt = {
      val len = input.readByte()
      val b   = new Array[Byte](len)
      input.readFully(b)
      BigInt(b)
    }

    def readScriptOption(): Option[Script] = {
      if (input.readBoolean()) {
        val b = readBytes()
        Some(ScriptReader.fromBytes(b).explicitGet())
      } else None
    }

    def readBytes(len: Int): Array[Byte] = {
      val arr = new Array[Byte](len)
      input.readFully(arr)
      arr
    }

    def readByteStr(len: Int): ByteStr = {
      ByteStr(readBytes(len))
    }

    def readBytes(): Array[Byte] = {
      val len = input.readShort()
      readBytes(len)
    }

    def readString(): String = {
      val arr = readBytes()
      new String(arr, UTF_8)
    }

    def readPublicKey: PublicKeyAccount = {
      PublicKeyAccount(input.readBytes(crypto.KeyLength))
    }

    def readStoredContract(): StoredContract = {
      val isDockerContract = input.readByte() == DOCKER_CONTRACT_TYPE

      if (isDockerContract) {
        readDockerContract()
      } else {
        readWasmContract()
      }
    }

    def readDockerContract(): DockerContract = {
      val image = readString()
      val hash  = readString()

      DockerContract(image, hash)
    }

    def readWasmContract(): WasmContract = {
      val len      = input.readInt()
      val bytecode = input.readBytes(len)
      val hash     = input.readString()

      WasmContract(bytecode, hash)
    }
  }

}
