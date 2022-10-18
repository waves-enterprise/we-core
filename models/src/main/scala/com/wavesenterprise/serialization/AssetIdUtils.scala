package com.wavesenterprise.serialization

import com.google.common.io.ByteArrayDataOutput
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.{AssetId, AssetIdLength}

object AssetIdUtils {
  def assetIdWriter(value: AssetId, output: ByteArrayDataOutput): Unit = {
    output.write(value.arr)
  }

  def assetIdReader(bytes: Array[Byte], pos: Int): (AssetId, Int) = {
    val (result, resultEnd) = ByteStr(bytes.slice(pos, pos + AssetIdLength)) -> (pos + AssetIdLength)
    result -> resultEnd
  }
}
