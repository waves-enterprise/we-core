package com.wavesenterprise.crypto.util

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{FreeSpec, Matchers}
import scorex.crypto.hash.Sha256

class HashSpec extends FreeSpec with Matchers {

  val mb: Int = 1024 * 1024
  val dataGen: Gen[Array[Byte]] = for {
    dataLength <- Gen.choose(mb, 2 * mb)
    data       <- Gen.containerOfN[Array, Byte](dataLength, Arbitrary.arbitrary[Byte])
  } yield data

  "A Sha256Hash" - {

    "must produce correct hash" in {
      val data         = (1 to 100000).map(_.toByte).toArray
      val expectedHash = Sha256.hash(data)

      val hasher = Sha256Hash()
      val hash = hasher
        .update(data)
        .result()

      assertResult(expectedHash)(hash)
    }

    "must produce the same hash when data processing by chunk and entirely" in {
      val data = dataGen.sample.get

      val hasher = Sha256Hash()

      val hash = hasher
        .update(data)
        .result()

      data.grouped(1024).foreach(hasher.update)
      val chunkHash = hasher.result()

      hash shouldEqual chunkHash
    }
  }

}
