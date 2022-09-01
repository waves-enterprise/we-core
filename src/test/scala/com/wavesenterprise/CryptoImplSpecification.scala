package com.wavesenterprise

import org.scalatest.{FreeSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import com.wavesenterprise.utils.Base58

import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.Await
import scala.concurrent.duration._

class CryptoImplSpecification extends FreeSpec with Matchers with ScalaCheckPropertyChecks {

  val kp       = crypto.generateKeyPair()
  val publicK  = kp.getPublic
  val privateK = kp.getPrivate

  implicit val scheduler: Scheduler = Scheduler.computation()

  "hashing produce same output on same input" - {
    forAll(Gen.alphaNumStr) { s =>
      val task         = Task(crypto.fastHash(s.getBytes()))
      val tasksResults = Task.gatherUnordered((1 to 1000).map(_ => task))
      val strs         = Await.result(tasksResults.runToFuture, Duration.Inf)
      strs.map(Base58.encode).distinct.size shouldEqual 1
    }
  }

  "hashing produce same output on same input" - {
    forAll(Gen.alphaNumStr) { s =>
      val task = Task(crypto.sign(privateK, s.getBytes()))
      val verifysTask = Task
        .gatherUnordered((1 to 1000).map(_ => task))
        .map(_.map(signature => crypto.verify(signature, s.getBytes(), publicK)))
      val verifys = Await.result(verifysTask.runToFuture, Duration.Inf)
      verifys.forall(identity) shouldEqual true
    }
  }

}
