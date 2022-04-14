package tools

import org.scalacheck.Gen

object GenHelper {

  implicit class ExtendedGen[A](private val gen: Gen[A]) extends AnyVal {

    def generateSample(maxRetry: Int = 100): A = gen.retryUntil(_ => true, maxRetry).sample.get
  }
}
