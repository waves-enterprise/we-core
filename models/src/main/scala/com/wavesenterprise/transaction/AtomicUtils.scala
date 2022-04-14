package com.wavesenterprise.transaction

import com.google.common.io.ByteStreams.newDataOutput
import com.wavesenterprise.account.PrivateKeyAccount
import com.wavesenterprise.crypto
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.docker.{ExecutableTransaction, ExecutedContractTransaction}

object AtomicUtils {

  def addExecutedTxs[T >: AtomicTransaction](container: AtomicTransaction, executedTxs: Seq[ExecutedContractTransaction]): T = {
    val executedMap = executedTxs.map(executedTx => executedTx.tx.id() -> executedTx).toMap

    val newInnerTransactions = container.transactions
      .map {
        case executableTx: ExecutableTransaction =>
          val id = executableTx.id()
          executedMap.getOrElse(id, throw new RuntimeException(s"No executed transaction found for id '$id' to generate signed atomic"))
        case tx => tx
      }

    container match {
      case txV1: AtomicTransactionV1 => txV1.copy(transactions = newInnerTransactions)
    }
  }

  /** reverse op for [[addExecutedTxs]] */
  def rollbackExecutedTxs[T >: AtomicTransaction](container: AtomicTransaction): T = {
    val newInnerTransactions = container.transactions
      .map {
        case executed: ExecutedContractTransaction =>
          executed.tx match {
            case atomicInnerTx: AtomicInnerTransaction =>
              atomicInnerTx

            case notAtomicInnerTx: ExecutableTransaction =>
              throw new Exception(s"Rollback executed tx - inner tx not atomic: '$notAtomicInnerTx'")
          }

        case tx => tx
      }

    container match {
      case txV1: AtomicTransactionV1 =>
        val proofsWithoutMiner = txV1.proofs.copy(proofs = txV1.proofs.proofs.init)

        txV1.copy(
          transactions = newInnerTransactions,
          miner = None,
          proofs = proofsWithoutMiner
        )
    }
  }

  /** miner proof must be added to end of list because of [[rollbackExecutedTxs]] */
  def addMinerProof[T >: AtomicTransaction](container: AtomicTransaction, account: PrivateKeyAccount): Either[ValidationError, T] = {
    val minerProofSourceBytes = extractMinerProofSource(container)
    val minerProof            = ByteStr(crypto.sign(account, minerProofSourceBytes))

    Proofs.create(container.proofs.proofs :+ minerProof).map { newProofs =>
      container match {
        case txV1: AtomicTransactionV1 => txV1.copy(miner = Some(account), proofs = newProofs)
      }
    }
  }

  def extractMinerProofSource(container: AtomicTransaction): Array[Byte] = {
    val output = newDataOutput()
    output.write(container.proofSourceBytes)

    container.transactions
      .collect {
        case executedTx: ExecutedContractTransaction => executedTx.id()
      }
      .foreach { txId =>
        output.write(txId.arr)
      }

    output.toByteArray
  }
}
