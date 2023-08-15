package com.wavesenterprise.transaction

import com.wavesenterprise.CryptoHelpers
import com.wavesenterprise.account.PrivateKeyAccount
import com.wavesenterprise.docker.ContractApiVersion
import com.wavesenterprise.docker.validator.ValidationPolicy
import org.scalacheck.Gen

trait CommonGen {

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
}
