package com.wavesenterprise.docker

import com.google.common.io.ByteArrayDataOutput
import com.wavesenterprise.serialization.BinarySerializer.Offset

import java.nio.charset.StandardCharsets.UTF_8

object StoredContractOps {

  def writeBytes(contract: StoredContract, output: ByteArrayDataOutput): Unit = {
    contract match {
      case StoredContract.DockerContract(image, imageHash, apiVersion) =>
        output.writeBoolean(false)
        val bytes = image.getBytes(UTF_8)
        val hashB = imageHash.getBytes(UTF_8)
        output.writeShort(bytes.size)
        output.write(bytes)
        output.writeShort(hashB.size)
        output.write(hashB)
        output.write(apiVersion.bytes)
      case StoredContract.WasmContract(bytecode, bytecodeHash) =>
        output.writeBoolean(true)
        val size  = bytecode.length
        val hashB = bytecodeHash.getBytes(UTF_8)
        output.writeInt(size)
        output.write(bytecode)
        output.writeShort(hashB.size)
        output.write(hashB)
    }
  }

  def parse(bytes: Array[Byte], offset: Offset): (StoredContract, Offset) = {
    StoredContract.storedContractReader(bytes, offset)
  }

}
