package com.wavesenterprise.transaction

import com.wavesenterprise.crypto
import com.wavesenterprise.state.ByteStr
import monix.eval.Coeval

trait FastHashId extends ProvenTransaction {

  val id: Coeval[ByteStr] = Coeval.evalOnce(ByteStr(crypto.fastHash(proofSourceBytes)))
}
