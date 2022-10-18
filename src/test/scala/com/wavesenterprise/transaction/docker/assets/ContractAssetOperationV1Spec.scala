package com.wavesenterprise.transaction.docker.assets

import com.wavesenterprise.lang.EitherExt
import com.wavesenterprise.serialization.ProtoAdapter
import com.wavesenterprise.transaction.docker.ContractTransactionGen
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ContractAssetOperationV1Spec extends PropSpec with ScalaCheckPropertyChecks with Matchers with ContractTransactionGen {

  property("ContractAssetOperationV1Spec serialization/deserialization") {
    forAll(contractAssetOperationV1Gen()) {
      case (tx, _) =>
        val (recovered, _) = ContractAssetOperation.fromBytes(tx.bytes().value(), 0)
        recovered shouldBe tx
    }
  }

  property("ContractAssetOperationV1Spec proto serialization/deserialization") {
    forAll(contractAssetOperationV1Gen()) {
      case (tx, _) =>
        val recovered = ProtoAdapter.fromProto(ProtoAdapter.toProto(tx)).explicitGet()
        recovered shouldBe tx
    }
  }
}
