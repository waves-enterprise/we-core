package com.wavesenterprise.transaction.smart.script

import java.nio.charset.StandardCharsets

import com.wavesenterprise.crypto
import com.wavesenterprise.lang.ScriptVersion.Versions.V1
import com.wavesenterprise.lang.v1.Serde
import com.wavesenterprise.lang.v1.compiler.Terms.TRUE
import com.wavesenterprise.state.diffs.ProduceError.produce
import org.scalatest.{FreeSpec, Matchers}

class ScriptReaderTest extends FreeSpec with Matchers {
  val checksumLength = 4

  "should parse all bytes for V1" in {
    val body     = Array(V1.value.toByte) ++ Serde.serialize(TRUE) ++ "foo".getBytes(StandardCharsets.UTF_8)
    val allBytes = body ++ crypto.secureHash(body).take(checksumLength)
    ScriptReader.fromBytes(allBytes) should produce("bytes left")
  }
}
