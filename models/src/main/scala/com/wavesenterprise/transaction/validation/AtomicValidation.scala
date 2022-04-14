package com.wavesenterprise.transaction.validation

import cats.implicits._
import com.wavesenterprise.account.{Address, PublicKeyAccount}
import com.wavesenterprise.transaction.ValidationError.GenericError
import com.wavesenterprise.transaction.docker.{ExecutableTransaction, ExecutedContractTransaction}
import com.wavesenterprise.transaction.{AtomicInnerTransaction, AtomicTransaction, ValidationError}

object AtomicValidation {

  def validateInnerTransactions(txs: List[AtomicInnerTransaction],
                                miner: Option[PublicKeyAccount],
                                sender: Address): Either[ValidationError, Unit] = {
    mustBeMoreThanOneTx(txs) >>
      mustNotContainDuplicatedTxs(txs) >>
      mustNotContainAtomicTxs(txs) >>
      validateBadge(txs, sender) >>
      (if (miner.nonEmpty) mustNotContainExecutableTxs(txs) else mustNotContainExecutedTxs(txs))
  }

  private def mustBeMoreThanOneTx(txs: Seq[AtomicInnerTransaction]) = Either.cond(
    txs.size > 1,
    (),
    GenericError(s"Atomic transaction must contain more than 1 transaction")
  )

  private def mustNotContainDuplicatedTxs(txs: Seq[AtomicInnerTransaction]) = Either.cond(
    txs.distinct.length == txs.size,
    (),
    GenericError(s"Atomic transaction cannot contain duplicated transactions")
  )

  private def mustNotContainExecutableTxs(txs: Seq[AtomicInnerTransaction]) = Either.cond(
    !txs.exists(_.isInstanceOf[ExecutableTransaction]),
    (),
    GenericError(s"Mined atomic transaction cannot contain executable transactions")
  )

  private def mustNotContainExecutedTxs(txs: Seq[AtomicInnerTransaction]) = Either.cond(
    !txs.exists(_.isInstanceOf[ExecutedContractTransaction]),
    (),
    GenericError(s"Atomic transaction cannot contain executed transactions")
  )

  private def mustNotContainAtomicTxs(txs: Seq[AtomicInnerTransaction]) = Either.cond(
    !txs.exists(_.isInstanceOf[AtomicTransaction]),
    (),
    GenericError(s"Atomic transaction cannot contain another atomic transaction")
  )

  private def validateBadge(txs: List[AtomicInnerTransaction], atomicSender: Address): Either[GenericError, Unit] = {
    txs.traverse {
      case _: ExecutedContractTransaction =>
        Right(())
      case tx =>
        tx.atomicBadge
          .map { atomicBadge =>
            atomicBadge.trustedSender
              .map { trustedSender =>
                Either.cond(
                  trustedSender == atomicSender,
                  (),
                  GenericError(
                    s"The trusted address '$trustedSender' of the transaction '${tx.id()}' does not match the atomic sender '$atomicSender'")
                )
              }
              .getOrElse {
                Either.cond(
                  tx.sender.toAddress == atomicSender,
                  (),
                  GenericError(s"The sender address '${tx.sender}' of the transaction '${tx.id()}' does not match the atomic sender '$atomicSender'")
                )
              }
          }
          .getOrElse {
            Left(GenericError(s"To use transaction '${tx.id()}' inside atomic container, the transaction must have atomic badge"))
          }
    }.void
  }
}
