package com.wavesenterprise.utils

import com.google.common.io.{ByteArrayDataInput, ByteArrayDataOutput}
import com.wavesenterprise.account.PublicKeyAccount
import com.wavesenterprise.crypto
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.smart.script.{Script, ScriptReader}

import java.nio.charset.StandardCharsets.UTF_8

object DatabaseUtils {

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
        val len = input.readShort()
        val b   = new Array[Byte](len)
        input.readFully(b)
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
  }

}
