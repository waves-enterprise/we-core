package com.wavesenterprise.transaction

import com.wavesenterprise.account.PrivateKeyAccount
import com.wavesenterprise.transaction.docker.{ExecutableTransaction, ExecutedContractTransactionV1}
import com.wavesenterprise.utils.EitherUtils.EitherExt

trait CommonAtomicTransactionSpec {
  protected def enrichAtomicTx[T <: AtomicInnerTransaction](senderAcc: PrivateKeyAccount,
                                                            minerAcc: PrivateKeyAccount,
                                                            innerTxs: List[T],
                                                            tx: AtomicTransaction,
                                                            timestamp: Long): AtomicTransaction = {
    val innerTxsWithExecuted = innerTxs.map {
      case executableTx: ExecutableTransaction =>
        ExecutedContractTransactionV1.selfSigned(senderAcc, executableTx, List.empty, timestamp).explicitGet()
      case tx => tx
    }

    val unsignedMinedTx = tx match {
      case v1: AtomicTransactionV1 => v1.copy(miner = Some(minerAcc), transactions = innerTxsWithExecuted)
    }

    AtomicUtils.addMinerProof(unsignedMinedTx, minerAcc).explicitGet()
  }
}
