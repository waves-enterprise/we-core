package com.wavesenterprise.transaction

import com.wavesenterprise.CoreTransactionGen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class GenesisPermitTransactionSpecification extends PropSpec with ScalaCheckPropertyChecks with Matchers with CoreTransactionGen {
  property("Genesis Permit Transaction encoding round trip") {
    forAll(genesisPermitTxGen) { permTx =>
      val encoded = permTx.bytes()

      assert(encoded.nonEmpty, "Encoded bytes are empty!")
      val encodedTypeId = encoded.head
      assert(encodedTypeId === GenesisPermitTransaction.typeId)

      val decoded = GenesisPermitTransaction.parseTail(version = 1, encoded, 1)
      decoded.fold(ex => fail(ex), _ => succeed)
    }
  }

  property("Permit Transaction from TransactionParser") {
    forAll(genesisPermitTxGen) { permTx =>
      val encoded = permTx.bytes()

      TransactionParsers
        .parseBytes(encoded)
        .fold(ex => fail(ex), tx => assert(tx.id() === permTx.id(), "Transaction ids don't match"))
    }
  }
}
