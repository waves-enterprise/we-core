package com.wavesenterprise.transaction

import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.docker.ValidationProof

trait ValidatorProvable {
  def resultsHash: ByteStr
  def validationProofs: List[ValidationProof]
}
