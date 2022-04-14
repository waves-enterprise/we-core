package com.wavesenterprise.transaction

import com.wavesenterprise.CoreTransactionGen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class GenesisRegisterNodeTransactionV1Specification extends PropSpec with ScalaCheckPropertyChecks with Matchers with CoreTransactionGen {
  property("Genesis RegisterNode Transaction encoding round trip") {
    forAll(genesisRegisterNodeTxGen()) { regTx =>
      val encoded = regTx.bytes()

      assert(encoded.nonEmpty, "Encoded bytes are empty!")
      val encodedTypeId = encoded.head
      assert(encodedTypeId === GenesisRegisterNodeTransaction.typeId)

      val decoded = GenesisRegisterNodeTransaction.parseTail(version = 1, encoded, 1)
      decoded.fold(ex => fail(ex), _ => succeed)
    }
  }

  property("RegisterNode Transaction from TransactionParser") {
    forAll(genesisRegisterNodeTxGen()) { regTx =>
      val encoded = regTx.bytes()

      TransactionParsers
        .parseBytes(encoded)
        .fold(ex => fail(ex), tx => assert(tx.id() === regTx.id(), "Transaction ids don't match"))
    }
  }
}
