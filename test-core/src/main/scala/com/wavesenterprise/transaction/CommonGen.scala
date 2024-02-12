package com.wavesenterprise.transaction

import com.wavesenterprise.CryptoHelpers
import com.wavesenterprise.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesenterprise.docker.StoredContract._
import com.wavesenterprise.docker.StoredContract
import com.wavesenterprise.docker.validator.ValidationPolicy
import com.wavesenterprise.docker.{ContractApiVersion, ContractInfo}
import com.wavesenterprise.lang.WavesGlobal
import com.wavesenterprise.state.ByteStr
import monix.eval.Coeval
import org.scalacheck.{Arbitrary, Gen}

import java.nio.charset.StandardCharsets.UTF_8

trait CommonGen {

  def byteArrayGen(length: Int): Gen[Array[Byte]] = Gen.containerOfN[Array, Byte](length, Arbitrary.arbitrary[Byte])

  val bytes32gen: Gen[Array[Byte]] = byteArrayGen(32)
  val bytes64gen: Gen[Array[Byte]] = byteArrayGen(64)

  def accountGen: Gen[PrivateKeyAccount] = Gen.Choose.chooseByte.map(_ => CryptoHelpers.generatePrivateKey)

  val atomicBadgeOptGen: Gen[Option[AtomicBadge]] =
    for {
      trustedAddress <- Gen.option(accountGen.map(_.toAddress))
      atomicBadge    <- Gen.option(Gen.const(AtomicBadge(trustedAddress)))
    } yield atomicBadge

  val atomicBadgeGen: Gen[AtomicBadge] =
    for {
      trustedAddress <- accountGen.map(_.toAddress)
      atomicBadge    <- Gen.const(AtomicBadge(Some(trustedAddress)))
    } yield atomicBadge

  val validationPolicyGen: Gen[ValidationPolicy] = {
    for {
      accounts <- Gen.nonEmptyListOf(accountGen)
      addresses = accounts.map(_.toAddress)
      result <- Gen.oneOf(ValidationPolicy.Any, ValidationPolicy.Majority, ValidationPolicy.MajorityWithOneOf(addresses))
    } yield result
  }

  val contractApiVersionGen: Gen[ContractApiVersion] = Gen.oneOf(ContractApiVersion.values)

  val dockerContractGen: Gen[DockerContract] = {
    for {
      bytes <- byteArrayGen(256)
      hash = WavesGlobal.sha256(bytes)
      apiVersion <- contractApiVersionGen
    } yield DockerContract(new String(bytes, UTF_8), new String(hash, UTF_8), apiVersion)
  }

  val wasmContractGen: Gen[WasmContract] = {
    for {
      len   <- Gen.chooseNum(30, 300000)
      bytes <- byteArrayGen(len)
      hash = WavesGlobal.sha256(bytes)
    } yield WasmContract(bytes, new String(hash, UTF_8))
  }

  def contractInfoGen(
      publicKeyAccountGen: Gen[PublicKeyAccount] = accountGen.map(_.publicKey).map(PublicKeyAccount.apply),
      contractIdGen: Gen[ByteStr] = bytes32gen.map(ByteStr.apply),
      storedContractGen: Gen[StoredContract] = Gen.oneOf(wasmContractGen, dockerContractGen),
      versionGen: Gen[Int] = Gen.const(1),
      activeGen: Gen[Boolean] = Gen.const(true),
      validationPolicyGen: Gen[ValidationPolicy] = validationPolicyGen,
      contractApiVersionGen: Gen[ContractApiVersion] = contractApiVersionGen
  ): Gen[ContractInfo] = {
    for {
      sender           <- publicKeyAccountGen
      contractId       <- contractIdGen
      storedContract   <- storedContractGen
      version          <- versionGen
      active           <- activeGen
      validationPolicy <- validationPolicyGen
      apiVersion       <- contractApiVersionGen
    } yield {
      ContractInfo(
        Coeval.pure(sender),
        contractId = contractId,
        storedContract = storedContract,
        version = version,
        active = active,
        validationPolicy = validationPolicy,
      )
    }
  }
}
