package com.wavesenterprise.transaction.docker.assets

import com.google.common.io.ByteArrayDataOutput
import com.wavesenterprise.serialization.BinarySerializer
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.{AssetId, AssetIdLength}
import play.api.libs.json.{Format, Json}

case class ContractTransferInV1(assetId: Option[AssetId], amount: Long)

object ContractTransferInV1 {
  implicit val format: Format[ContractTransferInV1] = Json.format

  def writeBytes(value: ContractTransferInV1, output: ByteArrayDataOutput): Unit = {
    BinarySerializer.writeByteIterable(value.assetId, assetIdWriter, output)
    output.writeLong(value.amount)
  }

  def fromBytes(bytes: Array[Byte], offset: Int): (ContractTransferInV1, Int) = {
    val (assetId, assetIdEnd) = BinarySerializer.parseOption(bytes, assetIdReader, offset)
    val (amount, end)         = BinarySerializer.parseLong(bytes, assetIdEnd)

    ContractTransferInV1(assetId, amount) -> end
  }

  private def assetIdWriter(value: AssetId, output: ByteArrayDataOutput): Unit = {
    output.write(value.arr)
  }

  private def assetIdReader(bytes: Array[Byte], pos: Int): (AssetId, Int) = {
    val (result, resultEnd) = ByteStr(bytes.slice(pos, pos + AssetIdLength)) -> (pos + AssetIdLength)
    result -> resultEnd
  }
}
