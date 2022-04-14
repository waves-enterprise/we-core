package com.wavesenterprise.transaction.validation

import com.wavesenterprise.utils.Constants.base58Length

object TransferValidation {
  val MaxTransferCount: Int        = 100
  val MaxAttachmentSize: Int       = 140
  val MaxAttachmentStringSize: Int = base58Length(MaxAttachmentSize)
}
