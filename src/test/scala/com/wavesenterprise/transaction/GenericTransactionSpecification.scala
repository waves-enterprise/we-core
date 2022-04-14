package com.wavesenterprise.transaction

import com.wavesenterprise.CoreTransactionGen
import org.scalacheck.Gen
import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json._

abstract class GenericTransactionSpecification[T <: com.wavesenterprise.transaction.Transaction]
    extends PropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with CoreTransactionGen
    with WithSenderAndRecipient {

  def transactionParser: com.wavesenterprise.transaction.TransactionParserFor[T]
  def updateProofs(tx: T, p: Proofs): T
  def generator: Gen[((Seq[com.wavesenterprise.transaction.Transaction], T))]
  def assertTxs(first: T, second: T): Unit
  def jsonRepr: Seq[(JsValue, T)]
  def transactionName: String

  property(s"$transactionName serialization roundtrip") {
    forAll(generator) { t =>
      val tx        = t._2
      val recovered = transactionParser.parseBytes(tx.bytes()).get
      assertTxs(recovered, tx)
    }
  }

  property(s"$transactionName serialization from TypedTransaction") {
    forAll(generator) { t =>
      val tx        = t._2
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      assertTxs(recovered.asInstanceOf[T], tx)
    }
  }

  property(s"$transactionName id doesn't depend on proof") {
    forAll(generator, proofsGen) {
      case ((pref, tx), proofs1) =>
        val tx1 = updateProofs(tx, proofs1)
        tx1.id() shouldBe tx.id()
    }
  }

  property(s"$transactionName JSON format validation") {
    for ((js, tx) <- jsonRepr) {
      js shouldEqual tx.json()
    }
  }
}
