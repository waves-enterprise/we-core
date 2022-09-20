package com.wavesenterprise.transaction.docker

import cats.implicits._
import com.wavesenterprise.account.{AddressOrAlias, PrivateKeyAccount}
import com.wavesenterprise.crypto
import com.wavesenterprise.docker.ContractApiVersion
import com.wavesenterprise.docker.validator.ValidationPolicy
import com.wavesenterprise.settings.TestFees.{defaultFees => fees}
import com.wavesenterprise.state._
import com.wavesenterprise.transaction._
import com.wavesenterprise.transaction.docker.assets.{ContractAssetOperation, ContractTransferInV1}
import com.wavesenterprise.transaction.validation.IssueTransactionValidation
import com.wavesenterprise.utils.EitherUtils.EitherExt
import org.apache.commons.codec.digest.DigestUtils
import org.scalacheck.Gen
import org.scalatest.Suite

import java.nio.charset.StandardCharsets.UTF_8
import scala.annotation.tailrec
import scala.util.Random

/**
  * Docker contracts transactions generation trait
  */
trait ContractTransactionGen extends CommonGen with WithSenderAndRecipient { _: Suite =>

  private val ParamsMinCount      = 0
  private val ParamsMaxCount      = 100
  private val DataEntryMaxSize    = 10
  private val BigDataEntryMaxSize = 64 * 1024 // 64KB
  private val ResultsMinCount     = 1
  private val ResultsMaxCount     = 100

  val createTxFeeGen: Gen[Long]  = Gen.const(fees.forTxType(CreateContractTransaction.typeId))
  val callTxFeeGen: Gen[Long]    = Gen.const(fees.forTxType(CallContractTransaction.typeId))
  val updateTxFeeGen: Gen[Long]  = Gen.const(fees.forTxType(UpdateContractTransaction.typeId))
  val disableTxFeeGen: Gen[Long] = Gen.const(fees.forTxType(DisableContractTransaction.typeId))
  val issueNonceGen: Gen[Byte]   = Gen.choose(-128: Byte, 127: Byte).suchThat(_ != 0)

  val defaultTransfersCountGen: Gen[Int]  = Gen.choose(0, 50)
  val genAssetId: Gen[AssetId]            = bytes32gen.map(ByteStr(_))
  val genOptAssetId: Gen[Option[AssetId]] = assetIdGen

  val createContractV1ParamGen: Gen[CreateContractTransactionV1] = for {
    signer       <- accountGen
    image        <- genBoundedString(CreateContractTransactionV1.ImageMinLength, CreateContractTransactionV1.ImageMaxLength)
    imageHash    <- bytes32gen.map(DigestUtils.sha256Hex)
    contractName <- genBoundedString(1, CreateContractTransactionV1.ContractNameMaxLength)
    params       <- Gen.choose(ParamsMinCount, ParamsMaxCount).flatMap(Gen.listOfN(_, dataEntryGen(DataEntryMaxSize)))
    feeAmount    <- createTxFeeGen
    timestamp    <- ntpTimestampGen
  } yield
    CreateContractTransactionV1
      .selfSigned(signer, new String(image, UTF_8), imageHash, new String(contractName, UTF_8), params, feeAmount, timestamp)
      .explicitGet()

  val createContractV2ParamGen: Gen[CreateContractTransactionV2] = createContractV2ParamGen(genOptAssetId, createTxFeeGen)

  val createContractV2ParamGenWithoutSponsoring: Gen[CreateContractTransactionV2] = createContractV2ParamGen(Gen.const(None), createTxFeeGen)

  def createContractV2ParamGen(optAssetIdGen: Gen[Option[AssetId]],
                               amountGen: Gen[Long],
                               signerGen: Gen[PrivateKeyAccount] = accountGen): Gen[CreateContractTransactionV2] =
    for {
      signer       <- signerGen
      image        <- genBoundedString(CreateContractTransactionV2.ImageMinLength, CreateContractTransactionV2.ImageMaxLength)
      imageHash    <- bytes32gen.map(DigestUtils.sha256Hex)
      contractName <- genBoundedString(1, CreateContractTransactionV2.ContractNameMaxLength)
      params       <- Gen.choose(ParamsMinCount, ParamsMaxCount).flatMap(Gen.listOfN(_, dataEntryGen(DataEntryMaxSize)))
      optAssetId   <- optAssetIdGen
      feeAmount    <- amountGen
      timestamp    <- ntpTimestampGen
    } yield
      CreateContractTransactionV2
        .selfSigned(signer, new String(image, UTF_8), imageHash, new String(contractName, UTF_8), params, feeAmount, timestamp, optAssetId)
        .explicitGet()

  def createContractV3ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]] = atomicBadgeOptGen): Gen[CreateContractTransactionV3] = {
    createContractV3ParamGen(atomicBadgeGen, genOptAssetId, createTxFeeGen, accountGen)
  }

  def createContractV3ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]],
                               optAssetIdGen: Gen[Option[AssetId]],
                               amountGen: Gen[Long],
                               signerGen: Gen[PrivateKeyAccount]): Gen[CreateContractTransactionV3] =
    for {
      signer       <- signerGen
      image        <- genBoundedString(CreateContractTransactionV3.ImageMinLength, CreateContractTransactionV3.ImageMaxLength)
      imageHash    <- bytes32gen.map(DigestUtils.sha256Hex)
      contractName <- genBoundedString(1, CreateContractTransactionV3.ContractNameMaxLength)
      params       <- Gen.choose(ParamsMinCount, ParamsMaxCount).flatMap(Gen.listOfN(_, dataEntryGen(DataEntryMaxSize)))
      optAssetId   <- optAssetIdGen
      feeAmount    <- amountGen
      atomicBadge  <- atomicBadgeGen
      timestamp    <- ntpTimestampGen
    } yield
      CreateContractTransactionV3
        .selfSigned(signer,
                    new String(image, UTF_8),
                    imageHash,
                    new String(contractName, UTF_8),
                    params,
                    feeAmount,
                    timestamp,
                    optAssetId,
                    atomicBadge)
        .explicitGet()

  def createContractV4ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]] = atomicBadgeOptGen,
                               validationPolicyGen: Gen[ValidationPolicy] = validationPolicyGen,
                               contractApiVersionGen: Gen[ContractApiVersion] = contractApiVersionGen): Gen[CreateContractTransactionV4] = {
    createContractV4ParamGen(atomicBadgeGen, genOptAssetId, createTxFeeGen, accountGen, validationPolicyGen, contractApiVersionGen)
  }

  def createContractV4ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]],
                               optAssetIdGen: Gen[Option[AssetId]],
                               amountGen: Gen[Long],
                               signerGen: Gen[PrivateKeyAccount],
                               validationPolicyGen: Gen[ValidationPolicy],
                               contractApiVersionGen: Gen[ContractApiVersion]): Gen[CreateContractTransactionV4] =
    for {
      signer             <- signerGen
      image              <- genBoundedString(CreateContractTransactionV4.ImageMinLength, CreateContractTransactionV4.ImageMaxLength)
      imageHash          <- bytes32gen.map(DigestUtils.sha256Hex)
      contractName       <- genBoundedString(1, CreateContractTransactionV4.ContractNameMaxLength)
      params             <- Gen.choose(ParamsMinCount, ParamsMaxCount).flatMap(Gen.listOfN(_, dataEntryGen(DataEntryMaxSize)))
      optAssetId         <- optAssetIdGen
      feeAmount          <- amountGen
      atomicBadge        <- atomicBadgeGen
      validationPolicy   <- validationPolicyGen
      contractApiVersion <- contractApiVersionGen
      timestamp          <- ntpTimestampGen
    } yield
      CreateContractTransactionV4
        .selfSigned(
          signer,
          new String(image, UTF_8),
          imageHash,
          new String(contractName, UTF_8),
          params,
          feeAmount,
          timestamp,
          optAssetId,
          atomicBadge,
          validationPolicy,
          contractApiVersion
        )
        .explicitGet()

  val bigCreateContractParamGen: Gen[CreateContractTransactionV2] = for {
    tx     <- createContractV2ParamGen
    signer <- accountGen
    params <- Gen.listOfN(1, binaryEntryGen(BigDataEntryMaxSize))
  } yield
    CreateContractTransactionV2.selfSigned(signer, tx.image, tx.imageHash, tx.contractName, params, tx.fee, tx.timestamp, tx.feeAssetId).explicitGet()

  val callContractV1ParamGen: Gen[CallContractTransactionV1] = for {
    signer     <- accountGen
    contractId <- createContractV2ParamGen.map(_.id())
    params     <- Gen.choose(ParamsMinCount, ParamsMaxCount).flatMap(Gen.listOfN(_, dataEntryGen(DataEntryMaxSize)))
    feeAmount  <- callTxFeeGen
    timestamp  <- ntpTimestampGen
  } yield CallContractTransactionV1.selfSigned(signer, contractId, params, feeAmount, timestamp).explicitGet()

  val callContractV2ParamGen: Gen[CallContractTransactionV2] = for {
    signer          <- accountGen
    tx              <- callContractV1ParamGen
    contractVersion <- Gen.choose(1, Integer.MAX_VALUE)
  } yield CallContractTransactionV2.selfSigned(signer, tx.contractId, tx.params, tx.fee, tx.timestamp, contractVersion).explicitGet()

  val callContractV3ParamGen: Gen[CallContractTransactionV3] = callContractV3ParamGen(genOptAssetId, callTxFeeGen)

  val callContractV3ParamGenWithoutSponsoring: Gen[CallContractTransactionV3] = callContractV3ParamGen(genOptAssetId, callTxFeeGen)

  def callContractV4ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]] = atomicBadgeOptGen): Gen[CallContractTransactionV4] = {
    callContractV4ParamGen(atomicBadgeGen, genOptAssetId, callTxFeeGen)
  }

  val bigCallContractV1ParamGen: Gen[CallContractTransactionV1] = for {
    tx     <- callContractV1ParamGen
    sender <- accountGen
    params <- Gen.listOfN(1, binaryEntryGen(BigDataEntryMaxSize))
  } yield CallContractTransactionV1.selfSigned(sender, tx.contractId, params, tx.fee, tx.timestamp).explicitGet()

  val executedContractV1ParamGen: Gen[ExecutedContractTransactionV1] = for {
    sender    <- accountGen
    tx        <- Gen.oneOf(createContractV1ParamGen, createContractV2ParamGen, callContractV1ParamGen, callContractV2ParamGen, callContractV3ParamGen)
    results   <- Gen.choose(ResultsMinCount, ResultsMaxCount).flatMap(Gen.listOfN(_, dataEntryGen(DataEntryMaxSize)))
    timestamp <- ntpTimestampGen
  } yield ExecutedContractTransactionV1.selfSigned(sender, tx, results, timestamp).explicitGet()

  val executedContractV2ParamGen: Gen[ExecutedContractTransactionV2] = for {
    sender <- accountGen
    tx     <- Gen.oneOf(createContractV1ParamGen, createContractV2ParamGen, callContractV1ParamGen, callContractV2ParamGen, callContractV3ParamGen)
    (results, resultsHash) <- Gen
      .choose(ResultsMinCount, ResultsMaxCount)
      .flatMap(Gen.listOfN(_, dataEntryGen(DataEntryMaxSize)))
      .map(ds => ds -> ContractTransactionValidation.resultsHash(ds))
    validationProofs <- validationProofsGen(resultsHash)
    timestamp        <- ntpTimestampGen
  } yield ExecutedContractTransactionV2.selfSigned(sender, tx, results, resultsHash, validationProofs, timestamp).explicitGet()

  val executedContractV3ParamGen: Gen[ExecutedContractTransactionV3] = for {
    sender <- accountGen
    tx <- Gen.oneOf(
      createContractV1ParamGen,
      createContractV2ParamGen,
      callContractV1ParamGen,
      callContractV2ParamGen,
      callContractV3ParamGen
    )
    assetOperations <- listContractAssetOperationV1Gen(tx.id(), Gen.choose(0, 20), accountOrAliasGen, genOptAssetId, issueNonceGen)
    (results, resultsHash) <- Gen
      .choose(ResultsMinCount, ResultsMaxCount)
      .flatMap(Gen.listOfN(_, dataEntryGen(DataEntryMaxSize)))
      .map(ds => ds -> ContractTransactionValidation.resultsHash(ds, assetOperations))
    validationProofs <- validationProofsGen(resultsHash)
    timestamp        <- ntpTimestampGen
  } yield ExecutedContractTransactionV3.selfSigned(sender, tx, results, resultsHash, validationProofs, timestamp, assetOperations).explicitGet()

  val bigExecutedContractV1ParamGen: Gen[ExecutedContractTransactionV1] = for {
    sender    <- accountGen
    tx        <- Gen.oneOf(bigCreateContractParamGen, bigCallContractV1ParamGen)
    results   <- Gen.listOfN(1, binaryEntryGen(BigDataEntryMaxSize))
    timestamp <- ntpTimestampGen
  } yield ExecutedContractTransactionV1.selfSigned(sender, tx, results, timestamp).explicitGet()

  val bigExecutedContractV2ParamGen: Gen[ExecutedContractTransactionV2] = for {
    sender  <- accountGen
    tx      <- Gen.oneOf(bigCreateContractParamGen, bigCallContractV1ParamGen)
    results <- Gen.listOfN(1, binaryEntryGen(BigDataEntryMaxSize))
    resultsHash = ContractTransactionValidation.resultsHash(results)
    validationProofs <- validationProofsGen(resultsHash)
    timestamp        <- ntpTimestampGen
  } yield ExecutedContractTransactionV2.selfSigned(sender, tx, results, resultsHash, validationProofs, timestamp).explicitGet()

  val bigExecutedContractV3ParamGen: Gen[ExecutedContractTransactionV3] = for {
    sender          <- accountGen
    tx              <- Gen.oneOf(bigCreateContractParamGen, bigCallContractV1ParamGen)
    assetOperations <- listContractAssetOperationV1Gen(bytes32gen.map(ByteStr.apply), Gen.const(20), accountOrAliasGen, genOptAssetId, issueNonceGen)
    (results, resultsHash) <- Gen
      .listOfN(1, binaryEntryGen(BigDataEntryMaxSize))
      .map(ds => ds -> ContractTransactionValidation.resultsHash(ds, assetOperations))
    validationProofs <- validationProofsGen(resultsHash)
    timestamp        <- ntpTimestampGen
  } yield ExecutedContractTransactionV3.selfSigned(sender, tx, results, resultsHash, validationProofs, timestamp, assetOperations).explicitGet()

  val disableContractV1ParamGen: Gen[DisableContractTransactionV1] = for {
    sender     <- accountGen
    contractId <- Gen.oneOf(createContractV1ParamGen, createContractV2ParamGen).map(_.id())
    feeAmount  <- smallFeeGen
    timestamp  <- ntpTimestampGen
  } yield DisableContractTransactionV1.selfSigned(sender, contractId, feeAmount, timestamp).explicitGet()

  val disableContractV2ParamGen: Gen[DisableContractTransactionV2] = disableContractV2ParamGen(genOptAssetId, disableTxFeeGen)

  val disableContractV2ParamGenWithoutSponsoring: Gen[DisableContractTransactionV2] = disableContractV2ParamGen(genOptAssetId, disableTxFeeGen)

  def disableContractV3ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]] = atomicBadgeOptGen): Gen[DisableContractTransactionV3] = {
    disableContractV3ParamGen(atomicBadgeGen, genOptAssetId, disableTxFeeGen)
  }

  val updateContractV1ParamGen: Gen[UpdateContractTransactionV1] = for {
    sender     <- accountGen
    contractId <- Gen.oneOf(createContractV1ParamGen, createContractV2ParamGen).map(_.id())
    image      <- genBoundedString(UpdateContractTransactionV1.ImageMinLength, UpdateContractTransactionV1.ImageMaxLength)
    imageHash  <- bytes32gen.map(DigestUtils.sha256Hex)
    feeAmount  <- smallFeeGen
    timestamp  <- ntpTimestampGen
  } yield UpdateContractTransactionV1.selfSigned(sender, contractId, new String(image, UTF_8), imageHash, feeAmount, timestamp).explicitGet()

  val updateContractV2ParamGen: Gen[UpdateContractTransactionV2] = updateContractV2ParamGen(genOptAssetId, updateTxFeeGen)

  val updateContractV2ParamGenWithoutSponsoring: Gen[UpdateContractTransactionV2] = updateContractV2ParamGen(genOptAssetId, updateTxFeeGen)

  def updateContractV3ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]] = atomicBadgeOptGen): Gen[UpdateContractTransactionV3] = {
    updateContractV3ParamGen(atomicBadgeGen, genOptAssetId, updateTxFeeGen)
  }

  def executedTxV1ParamGen(signer: PrivateKeyAccount, tx: ExecutableTransaction): Gen[ExecutedContractTransactionV1] =
    for {
      results   <- Gen.choose(ResultsMinCount, ResultsMaxCount).flatMap(Gen.listOfN(_, dataEntryGen(DataEntryMaxSize)))
      timestamp <- ntpTimestampGen
    } yield ExecutedContractTransactionV1.selfSigned(signer, tx, results, timestamp).explicitGet()

  def executedTxV2ParamGen(signer: PrivateKeyAccount,
                           tx: ExecutableTransaction,
                           resultsHashTransformer: ByteStr => ByteStr = identity,
                           validationProofsTransformer: List[ValidationProof] => List[ValidationProof] = identity,
                           proofsCount: Int = 10,
                           specifiedValidators: List[PrivateKeyAccount] = List.empty): Gen[ExecutedContractTransactionV2] =
    for {
      (results, resultsHash) <- Gen
        .choose(ResultsMinCount, ResultsMaxCount)
        .flatMap(Gen.listOfN(_, dataEntryGen(DataEntryMaxSize)))
        .map(ds => ds -> resultsHashTransformer(ContractTransactionValidation.resultsHash(ds)))
      validationProofs <- validationProofsGen(resultsHash, proofsCount, specifiedValidators).map(validationProofsTransformer)
      timestamp        <- ntpTimestampGen
    } yield ExecutedContractTransactionV2.selfSigned(signer, tx, results, resultsHash, validationProofs, timestamp).explicitGet()

  def executedTxV3ParamGen(signer: PrivateKeyAccount,
                           tx: ExecutableTransaction,
                           resultsHashTransformer: ByteStr => ByteStr = identity,
                           validationProofsTransformer: List[ValidationProof] => List[ValidationProof] = identity,
                           proofsCount: Int = 10,
                           specifiedValidators: List[PrivateKeyAccount] = List.empty,
                           operationsCount: Gen[Int] = Gen.const(0),
                           recipientsGen: Gen[AddressOrAlias] = accountOrAliasGen,
                           nonceGen: Gen[Byte] = issueNonceGen): Gen[ExecutedContractTransactionV3] =
    for {
      assetOperations <- listContractAssetOperationV1Gen(Gen.const(tx.id()), operationsCount, recipientsGen, genOptAssetId, nonceGen)
      (results, resultsHash) <- Gen
        .choose(ResultsMinCount, ResultsMaxCount)
        .flatMap(Gen.listOfN(_, dataEntryGen(DataEntryMaxSize)))
        .map(ds => ds -> resultsHashTransformer(ContractTransactionValidation.resultsHash(ds, assetOperations)))
      validationProofs <- validationProofsGen(resultsHash, proofsCount, specifiedValidators).map(validationProofsTransformer)
      timestamp        <- ntpTimestampGen
    } yield ExecutedContractTransactionV3.selfSigned(signer, tx, results, resultsHash, validationProofs, timestamp, assetOperations).explicitGet()

  def executedTxV3ParamWithOperationsGen(signer: PrivateKeyAccount,
                                         tx: ExecutableTransaction,
                                         resultsHashTransformer: ByteStr => ByteStr = identity,
                                         validationProofsTransformer: List[ValidationProof] => List[ValidationProof] = identity,
                                         proofsCount: Int = 10,
                                         specifiedValidators: List[PrivateKeyAccount] = List.empty,
                                         assetOperations: List[ContractAssetOperation]): Gen[ExecutedContractTransactionV3] =
    for {
      (results, resultsHash) <- Gen
        .choose(ResultsMinCount, ResultsMaxCount)
        .flatMap(Gen.listOfN(_, dataEntryGen(DataEntryMaxSize)))
        .map(ds => ds -> resultsHashTransformer(ContractTransactionValidation.resultsHash(ds, assetOperations)))
      validationProofs <- validationProofsGen(resultsHash, proofsCount, specifiedValidators).map(validationProofsTransformer)
      timestamp        <- ntpTimestampGen
    } yield ExecutedContractTransactionV3.selfSigned(signer, tx, results, resultsHash, validationProofs, timestamp, assetOperations).explicitGet()

  def executedForUpdateGen(signer: PrivateKeyAccount, updateTx: UpdateContractTransaction): Gen[ExecutedContractTransactionV1] =
    ntpTimestampGen.map { timestamp =>
      ExecutedContractTransactionV1.selfSigned(signer, updateTx, List.empty, timestamp).explicitGet()
    }

  def createContractV1ParamGen(signer: PrivateKeyAccount): Gen[CreateContractTransactionV1] =
    createContractV1ParamGen.map { tx =>
      CreateContractTransactionV1.selfSigned(signer, tx.image, tx.imageHash, tx.contractName, tx.params, tx.fee, tx.timestamp).explicitGet()
    }

  def createContractV2ParamGen(signer: PrivateKeyAccount): Gen[CreateContractTransactionV2] =
    createContractV2ParamGenWithoutSponsoring.map { tx =>
      CreateContractTransactionV2
        .selfSigned(signer, tx.image, tx.imageHash, tx.contractName, tx.params, tx.fee, tx.timestamp, tx.feeAssetId)
        .explicitGet()
    }

  def callContractV1ParamGen(signer: PrivateKeyAccount, contractId: ByteStr): Gen[CallContractTransactionV1] =
    callContractV1ParamGen.map { tx =>
      CallContractTransactionV1.selfSigned(signer, contractId, tx.params, tx.fee, tx.timestamp).explicitGet()
    }

  def callContractV2ParamGen(signer: PrivateKeyAccount, contractId: ByteStr, contractVersion: Int): Gen[CallContractTransactionV2] =
    callContractV2ParamGen.map { tx =>
      CallContractTransactionV2.selfSigned(signer, contractId, tx.params, tx.fee, tx.timestamp, contractVersion).explicitGet()
    }

  def callContractV3ParamGenWithoutSponsoring(signer: PrivateKeyAccount, contractId: ByteStr, contractVersion: Int): Gen[CallContractTransactionV3] =
    createContractV2ParamGenWithoutSponsoring.map { tx =>
      CallContractTransactionV3.selfSigned(signer, contractId, tx.params, tx.fee, tx.timestamp, contractVersion, tx.feeAssetId).explicitGet()
    }

  def callContractV3ParamGen(optAssetIdGen: Gen[Option[AssetId]], amountGen: Gen[Long]): Gen[CallContractTransactionV3] =
    for {
      signer          <- accountGen
      tx              <- callContractV1ParamGen
      contractVersion <- Gen.choose(1, Integer.MAX_VALUE)
      optFeeAssetId   <- optAssetIdGen
      feeAmount       <- amountGen
    } yield
      CallContractTransactionV3.selfSigned(signer, tx.contractId, tx.params, feeAmount, tx.timestamp, contractVersion, optFeeAssetId).explicitGet()

  def callContractV3ParamGen(optAssetIdGen: Gen[Option[AssetId]],
                             amountGen: Gen[Long],
                             signer: PrivateKeyAccount,
                             contractId: ByteStr,
                             contractVersion: Int): Gen[CallContractTransactionV3] =
    callContractV3ParamGen(optAssetIdGen, amountGen).map { tx =>
      CallContractTransactionV3.selfSigned(signer, contractId, tx.params, tx.fee, tx.timestamp, contractVersion, tx.feeAssetId).explicitGet()
    }

  def callContractV4ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]],
                             optAssetIdGen: Gen[Option[AssetId]],
                             amountGen: Gen[Long]): Gen[CallContractTransactionV4] =
    for {
      signer          <- accountGen
      tx              <- callContractV1ParamGen
      contractVersion <- Gen.choose(1, Integer.MAX_VALUE)
      optFeeAssetId   <- optAssetIdGen
      feeAmount       <- amountGen
      atomicBadge     <- atomicBadgeGen
    } yield
      CallContractTransactionV4
        .selfSigned(signer, tx.contractId, tx.params, feeAmount, tx.timestamp, contractVersion, optFeeAssetId, atomicBadge)
        .explicitGet()

  def callContractV5ParamGen(optAssetIdGen: Gen[Option[AssetId]],
                             amountGen: Gen[Long],
                             signer: PrivateKeyAccount,
                             contractId: ByteStr,
                             contractVersion: Int,
                             transfers: List[ContractTransferInV1]): Gen[CallContractTransactionV5] =
    for {
      callFeeOptAssetId <- optAssetIdGen
      callFeeAmount     <- amountGen
      tx                <- callContractV5ParamGen(atomicBadgeGen = None)
    } yield
      CallContractTransactionV5
        .selfSigned(signer, contractId, tx.params, callFeeAmount, tx.timestamp, contractVersion, callFeeOptAssetId, tx.atomicBadge, transfers)
        .explicitGet()

  def updateContractV1ParamGen(signer: PrivateKeyAccount, createTx: CreateContractTransaction): Gen[UpdateContractTransactionV1] =
    updateContractV1ParamGen.map { tx =>
      UpdateContractTransactionV1.selfSigned(signer, createTx.contractId, tx.image, tx.imageHash, tx.fee, tx.timestamp).explicitGet()
    }

  def updateContractV2ParamGenWithoutSponsoring(signer: PrivateKeyAccount, createTx: CreateContractTransaction): Gen[UpdateContractTransactionV2] =
    updateContractV2ParamGenWithoutSponsoring.map { tx =>
      UpdateContractTransactionV2.selfSigned(signer, createTx.contractId, tx.image, tx.imageHash, tx.fee, tx.timestamp, tx.feeAssetId).explicitGet()
    }

  def updateContractV2ParamGen(optAssetIdGen: Gen[Option[AssetId]],
                               amountGen: Gen[Long],
                               signer: PrivateKeyAccount,
                               contractId: ByteStr): Gen[UpdateContractTransactionV2] =
    updateContractV2ParamGen(optAssetIdGen, amountGen).map { tx =>
      UpdateContractTransactionV2.selfSigned(signer, contractId, tx.image, tx.imageHash, tx.fee, tx.timestamp, tx.feeAssetId).explicitGet()
    }

  def disableContractV2ParamGen(optAssetIdGen: Gen[Option[AssetId]],
                                amountGen: Gen[Long],
                                signer: PrivateKeyAccount,
                                contractId: ByteStr): Gen[DisableContractTransactionV2] =
    disableContractV2ParamGen(optAssetIdGen, amountGen).map { tx =>
      DisableContractTransactionV2.selfSigned(signer, contractId, tx.fee, tx.timestamp, tx.feeAssetId).explicitGet()
    }

  def updateContractV2ParamGen(optAssetIdGen: Gen[Option[AssetId]], amountGen: Gen[Long]): Gen[UpdateContractTransactionV2] =
    for {
      sender        <- accountGen
      contractId    <- Gen.oneOf(createContractV1ParamGen, createContractV2ParamGen).map(_.id())
      image         <- genBoundedString(UpdateContractTransactionV2.ImageMinLength, UpdateContractTransactionV2.ImageMaxLength)
      imageHash     <- bytes32gen.map(DigestUtils.sha256Hex)
      timestamp     <- ntpTimestampGen
      optFeeAssetId <- optAssetIdGen
      feeAmount     <- amountGen
    } yield
      UpdateContractTransactionV2
        .selfSigned(sender, contractId, new String(image, UTF_8), imageHash, feeAmount, timestamp, optFeeAssetId)
        .explicitGet()

  def updateContractV3ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]],
                               optAssetIdGen: Gen[Option[AssetId]],
                               amountGen: Gen[Long]): Gen[UpdateContractTransactionV3] =
    for {
      sender        <- accountGen
      atomicBadge   <- atomicBadgeGen
      contractId    <- Gen.oneOf(createContractV1ParamGen, createContractV3ParamGen(atomicBadge)).map(_.id())
      image         <- genBoundedString(UpdateContractTransactionV2.ImageMinLength, UpdateContractTransactionV2.ImageMaxLength)
      imageHash     <- bytes32gen.map(DigestUtils.sha256Hex)
      timestamp     <- ntpTimestampGen
      optFeeAssetId <- optAssetIdGen
      feeAmount     <- amountGen
    } yield
      UpdateContractTransactionV3
        .selfSigned(sender, contractId, new String(image, UTF_8), imageHash, feeAmount, timestamp, optFeeAssetId, atomicBadge)
        .explicitGet()

  def updateContractV4ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]] = atomicBadgeOptGen,
                               validationPolicyGen: Gen[ValidationPolicy] = validationPolicyGen,
                               contractApiVersionGen: Gen[ContractApiVersion] = contractApiVersionGen,
                               contractIdGen: Gen[ByteStr] = bytes32gen.map(ByteStr.apply)): Gen[UpdateContractTransactionV4] = {
    updateContractV4ParamGen(atomicBadgeGen, genOptAssetId, createTxFeeGen, accountGen, validationPolicyGen, contractApiVersionGen, contractIdGen)
  }

  def updateContractV4ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]],
                               optAssetIdGen: Gen[Option[AssetId]],
                               amountGen: Gen[Long],
                               signerGen: Gen[PrivateKeyAccount],
                               validationPolicyGen: Gen[ValidationPolicy],
                               contractApiVersionGen: Gen[ContractApiVersion],
                               contractIdGen: Gen[ByteStr]): Gen[UpdateContractTransactionV4] =
    for {
      signer             <- signerGen
      atomicBadge        <- atomicBadgeGen
      contractId         <- contractIdGen
      image              <- genBoundedString(UpdateContractTransactionV2.ImageMinLength, UpdateContractTransactionV2.ImageMaxLength)
      imageHash          <- bytes32gen.map(DigestUtils.sha256Hex)
      optFeeAssetId      <- optAssetIdGen
      feeAmount          <- amountGen
      validationPolicy   <- validationPolicyGen
      contractApiVersion <- contractApiVersionGen
      timestamp          <- ntpTimestampGen
    } yield
      UpdateContractTransactionV4
        .selfSigned(signer,
                    contractId,
                    new String(image, UTF_8),
                    imageHash,
                    feeAmount,
                    timestamp,
                    optFeeAssetId,
                    atomicBadge,
                    validationPolicy,
                    contractApiVersion)
        .explicitGet()

  def disableContractV2ParamGen(optAssetIdGen: Gen[Option[AssetId]], amountGen: Gen[Long]): Gen[DisableContractTransactionV2] =
    for {
      sender        <- accountGen
      contractId    <- Gen.oneOf(createContractV1ParamGen, createContractV2ParamGen).map(_.id())
      timestamp     <- ntpTimestampGen
      optFeeAssetId <- optAssetIdGen
      feeAmount     <- amountGen
    } yield DisableContractTransactionV2.selfSigned(sender, contractId, feeAmount, timestamp, optFeeAssetId).explicitGet()

  def disableContractV3ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]],
                                optAssetIdGen: Gen[Option[AssetId]],
                                amountGen: Gen[Long]): Gen[DisableContractTransactionV3] =
    for {
      sender        <- accountGen
      contractId    <- Gen.oneOf(createContractV1ParamGen, createContractV2ParamGen).map(_.id())
      timestamp     <- ntpTimestampGen
      optFeeAssetId <- optAssetIdGen
      feeAmount     <- amountGen
      atomicBadge   <- atomicBadgeGen
    } yield DisableContractTransactionV3.selfSigned(sender, contractId, feeAmount, timestamp, optFeeAssetId, atomicBadge).explicitGet()

  def validationProofsGen(resultsHash: ByteStr,
                          proofsCount: Int = 10,
                          specifiedValidators: List[PrivateKeyAccount] = List.empty): Gen[List[ValidationProof]] = {
    (if (specifiedValidators.isEmpty) Gen.listOfN(proofsCount, accountGen) else Gen.const(Random.shuffle(specifiedValidators).take(proofsCount)))
      .map { validators =>
        validators.map { validator =>
          val signature = ByteStr(crypto.sign(validator, resultsHash.arr))
          ValidationProof(validator, signature)
        }
      }
  }

  def atomicSingleInnerTxGen(atomicBadge: Option[AtomicBadge]): Gen[AtomicInnerTransaction] =
    Gen.oneOf(
      transferV3Gen(atomicBadge),
      createContractV3ParamGen(atomicBadge),
      callContractV4ParamGen(atomicBadge),
      updateContractV3ParamGen(atomicBadge),
      disableContractV3ParamGen(atomicBadge)
    )

  def atomicInnerTxsGen(atomicBadge: Option[AtomicBadge], maxCount: Int): Gen[List[AtomicInnerTransaction]] =
    for {
      trxs <- Gen.chooseNum(2, maxCount).flatMap(Gen.listOfN(_, atomicSingleInnerTxGen(atomicBadge)))
    } yield trxs

  val atomicTxV1Gen: Gen[AtomicTransaction] =
    for {
      signer   <- accountGen
      badge    <- atomicBadgeGen
      atomicTx <- atomicTxV1MultipleInnerGen(signer, atomicInnerTxsGen(Some(badge), 5))
    } yield atomicTx

  def contractTransferOutV1Gen(recipientsGen: Gen[AddressOrAlias] = accountOrAliasGen,
                               optAssetIdGen: Gen[Option[AssetId]] = genOptAssetId): Gen[ContractAssetOperation.ContractTransferOutV1] =
    for {
      optAssetId <- optAssetIdGen
      recipient  <- recipientsGen
      amount     <- positiveLongGen
    } yield ContractAssetOperation.ContractTransferOutV1(optAssetId, recipient, amount)

  def contractTransferOutV1ParamGen(optAssetId: Option[AssetId]): Gen[ContractAssetOperation.ContractTransferOutV1] =
    for {
      recipient <- accountOrAliasGen
      amount    <- positiveLongGen
    } yield ContractAssetOperation.ContractTransferOutV1(optAssetId, recipient, amount)

  def contractIssueV1Gen(nonceGen: Gen[Byte] = issueNonceGen,
                         isReissuableGen: Gen[Boolean] = reissuanceGen,
                         quantityGen: Gen[Long] = positiveLongGen,
                         maybeParentTxId: Option[ByteStr] = None): Gen[ContractAssetOperation.ContractIssueV1] =
    for {
      nonce        <- nonceGen
      assetId      <- maybeParentTxId.map(parentTxId => Gen.const(crypto.fastHash(parentTxId.arr :+ nonce))).getOrElse(bytes32gen)
      name         <- genBoundedString(IssueTransactionValidation.MinAssetNameLength, IssueTransactionValidation.MaxAssetNameLength)
      description  <- genBoundedString(0, IssueTransactionValidation.MaxDescriptionLength)
      quantity     <- quantityGen
      decimals     <- Gen.choose(0: Byte, 8: Byte)
      isReissuable <- isReissuableGen
    } yield
      ContractAssetOperation.ContractIssueV1(ByteStr(assetId),
                                             new String(name, UTF_8),
                                             new String(description, UTF_8),
                                             quantity,
                                             decimals,
                                             isReissuable,
                                             nonce)

  def contractIssueV1ParamGen(): Gen[ContractAssetOperation.ContractIssueV1] =
    for {
      assetId      <- bytes32gen
      nonce        <- issueNonceGen
      name         <- genBoundedString(IssueTransactionValidation.MinAssetNameLength, IssueTransactionValidation.MaxAssetNameLength)
      description  <- genBoundedString(0, IssueTransactionValidation.MaxDescriptionLength)
      quantity     <- positiveLongGen
      decimals     <- Gen.choose(0: Byte, 8: Byte)
      isReissuable <- reissuanceGen
    } yield
      ContractAssetOperation.ContractIssueV1(ByteStr(assetId),
                                             new String(name, UTF_8),
                                             new String(description, UTF_8),
                                             quantity,
                                             decimals,
                                             isReissuable,
                                             nonce)

  def contractReissueV1Gen(assetGen: Gen[AssetId] = genAssetId,
                           isReissuableGen: Gen[Boolean] = reissuanceGen,
                           quantityGen: Gen[Long] = positiveLongGen): Gen[ContractAssetOperation.ContractReissueV1] =
    for {
      assetId      <- assetGen
      quantity     <- quantityGen
      isReissuable <- isReissuableGen
    } yield ContractAssetOperation.ContractReissueV1(assetId, quantity, isReissuable)

  def contractReissueV1ParamGen(assetId: AssetId,
                                cancelReissuance: Boolean,
                                quantityGen: Gen[Long] = positiveLongGen): Gen[ContractAssetOperation.ContractReissueV1] =
    for {
      quantity <- quantityGen
    } yield ContractAssetOperation.ContractReissueV1(assetId, quantity, !cancelReissuance)

  def contractBurnV1Gen(optAssetIdGen: Gen[Option[AssetId]] = genOptAssetId): Gen[ContractAssetOperation.ContractBurnV1] =
    for {
      optAssetId <- optAssetIdGen
      amount     <- positiveLongGen
    } yield ContractAssetOperation.ContractBurnV1(optAssetId, amount)

  def contractBurnV1ParamGen(burnedAssetId: Option[AssetId]): Gen[ContractAssetOperation.ContractBurnV1] =
    for {
      amount <- positiveLongGen
    } yield ContractAssetOperation.ContractBurnV1(burnedAssetId, amount)

  def contractAssetOperationV1Gen(executedTxId: Gen[ByteStr] = bytes32gen.map(ByteStr.apply),
                                  recipientsGen: Gen[AddressOrAlias] = accountOrAliasGen,
                                  assetsGen: Gen[Option[AssetId]] = genOptAssetId,
                                  nonceGen: Gen[Byte] = issueNonceGen): Gen[(ContractAssetOperation, Option[AssetId])] =
    Gen
      .oneOf(
        contractIssueV1Gen(nonceGen),
        contractTransferOutV1Gen(recipientsGen, assetsGen),
        contractReissueV1Gen(assetsGen.filter(_.isDefined).map(_.get)),
        contractBurnV1Gen(assetsGen.filter(_.isDefined))
      )
      .flatMap {
        case issue: ContractAssetOperation.ContractIssueV1 =>
          executedTxId.map(txId => issue -> ByteStr(crypto.fastHash(txId.arr :+ issue.nonce)).some)
        case op: ContractAssetOperation => Gen.const(op -> None)
      }

  def listContractAssetOperationV1Gen(executedTxIdGen: Gen[ByteStr],
                                      maxOperations: Gen[Int],
                                      recipientsGen: Gen[AddressOrAlias],
                                      assetsGen: Gen[Option[AssetId]],
                                      issueNonceGen: Gen[Byte]): Gen[List[ContractAssetOperation]] = {

    def contractOperationsGen(count: Int,
                              issuedAssetIds: List[Option[AssetId]],
                              result: List[ContractAssetOperation]): Gen[List[ContractAssetOperation]] = count match {
      case 0 => Gen.const(result)
      case _ =>
        val availableAssetIds = if (issuedAssetIds.nonEmpty) Gen.oneOf(assetsGen, Gen.oneOf(issuedAssetIds)) else assetsGen
        for {
          (operation, mbAsset) <- contractAssetOperationV1Gen(executedTxIdGen, recipientsGen, availableAssetIds, issueNonceGen)
        } yield
          (mbAsset match {
            case None          => contractOperationsGen(count - 1, issuedAssetIds, result :+ operation)
            case newOptAssetId => contractOperationsGen(count - 1, issuedAssetIds :+ newOptAssetId, result :+ operation)
          }).sample.getOrElse(List.empty)
    }

    for {
      operations <- maxOperations.flatMap(contractOperationsGen(_, List.empty, List.empty))
    } yield operations
  }

  def listContractAssetOperationV1ParamGen(executedTxIdGen: ByteStr,
                                           maxOperations: Int,
                                           availableAssets: List[AssetId] = List.empty): Gen[List[ContractAssetOperation]] = {
    @tailrec
    def contractOperationsGen(leftOperations: Int,
                              issuedAssetIds: List[AssetId],
                              result: List[ContractAssetOperation]): Gen[List[ContractAssetOperation]] = leftOperations match {
      case 0 => Gen.const(result)
      case _ =>
        val newContractAssetOperation = (for {
          assetIdOpt <- Gen.oneOf(issuedAssetIds.map(_.some) :+ none)
          newOperation <- issuedAssetIds match {
            case Nil =>
              Gen.frequency(
                (20, contractIssueV1ParamGen()),
                (5, contractBurnV1ParamGen(none[AssetId])),
                (75, contractTransferOutV1ParamGen(assetIdOpt))
              )
            case _ =>
              for {
                nonWestAssetId <- Gen.oneOf(issuedAssetIds)
                newOperation2 <- Gen.frequency(
                  (10, contractIssueV1ParamGen()),
                  (10, contractReissueV1ParamGen(nonWestAssetId, cancelReissuance = false)),
                  (10, contractBurnV1ParamGen(assetIdOpt)),
                  (70, contractTransferOutV1ParamGen(assetIdOpt))
                )
              } yield newOperation2
          }
        } yield newOperation).sample.get

        newContractAssetOperation match {
          case o: ContractAssetOperation.ContractIssueV1 =>
            contractOperationsGen(leftOperations - 1,
                                  issuedAssetIds :+ ByteStr(crypto.fastHash(executedTxIdGen.arr :+ o.nonce)),
                                  result :+ newContractAssetOperation)
          case _: ContractAssetOperation.ContractReissueV1 =>
            contractOperationsGen(leftOperations - 1, issuedAssetIds, result :+ newContractAssetOperation)
          case _: ContractAssetOperation.ContractBurnV1 =>
            contractOperationsGen(leftOperations - 1, issuedAssetIds, result :+ newContractAssetOperation)
          case _: ContractAssetOperation.ContractTransferOutV1 =>
            contractOperationsGen(leftOperations - 1, issuedAssetIds, result :+ newContractAssetOperation)
        }
    }

    contractOperationsGen(maxOperations, availableAssets, List.empty)
  }

  def contractTransferInV1Gen(optAssetIdGen: Gen[Option[AssetId]] = genOptAssetId,
                              transferAmountGen: Gen[Long] = smallFeeGen): Gen[ContractTransferInV1] =
    for {
      optAssetId <- optAssetIdGen
      amount     <- transferAmountGen
    } yield ContractTransferInV1(optAssetId, amount)

  def listContractTransferInV1Gen(transfersCount: Gen[Int], optAssetIdGen: Gen[Option[AssetId]]): Gen[List[ContractTransferInV1]] =
    for {
      listContractTransferInV1 <- transfersCount.flatMap(Gen.listOfN(_, contractTransferInV1Gen(optAssetIdGen)))
    } yield listContractTransferInV1

  def callContractV5ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]] = atomicBadgeOptGen,
                             maxPayments: Gen[Int] = defaultTransfersCountGen): Gen[CallContractTransactionV5] = {
    callContractV5ParamGen(atomicBadgeGen, genOptAssetId, callTxFeeGen, maxPayments)
  }

  def callContractV5ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]],
                             optAssetIdGen: Gen[Option[AssetId]],
                             amountGen: Gen[Long],
                             maxTransfers: Gen[Int]): Gen[CallContractTransactionV5] =
    for {
      signer          <- accountGen
      tx              <- callContractV1ParamGen
      contractVersion <- Gen.choose(1, Integer.MAX_VALUE)
      optFeeAssetId   <- optAssetIdGen
      feeAmount       <- amountGen
      atomicBadge     <- atomicBadgeGen
      payments        <- maxTransfers.flatMap(listContractTransferInV1Gen(_, Gen.const(optFeeAssetId)))
    } yield
      CallContractTransactionV5
        .selfSigned(signer, tx.contractId, tx.params, feeAmount, tx.timestamp, contractVersion, optFeeAssetId, atomicBadge, payments)
        .explicitGet()

  def createContractV5ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]] = atomicBadgeOptGen,
                               validationPolicyGen: Gen[ValidationPolicy] = validationPolicyGen,
                               contractApiVersionGen: Gen[ContractApiVersion] = contractApiVersionGen,
                               transfersCountGen: Gen[Int] = defaultTransfersCountGen): Gen[CreateContractTransactionV5] = {
    createContractV5ParamGen(atomicBadgeGen,
                             genOptAssetId,
                             createTxFeeGen,
                             accountGen,
                             validationPolicyGen,
                             contractApiVersionGen,
                             transfersCountGen,
                             transfersAssetGen = genOptAssetId)
  }

  def createContractV5ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]],
                               optAssetIdGen: Gen[Option[AssetId]],
                               amountGen: Gen[Long],
                               signerGen: Gen[PrivateKeyAccount],
                               validationPolicyGen: Gen[ValidationPolicy],
                               contractApiVersionGen: Gen[ContractApiVersion],
                               transfersCountGen: Gen[Int],
                               transfersAssetGen: Gen[Option[AssetId]]): Gen[CreateContractTransactionV5] =
    for {
      signer             <- signerGen
      image              <- genBoundedString(CreateContractTransactionV5.ImageMinLength, CreateContractTransactionV5.ImageMaxLength)
      imageHash          <- bytes32gen.map(DigestUtils.sha256Hex)
      contractName       <- genBoundedString(1, CreateContractTransactionV5.ContractNameMaxLength)
      params             <- Gen.choose(ParamsMinCount, ParamsMaxCount).flatMap(Gen.listOfN(_, dataEntryGen(DataEntryMaxSize)))
      optFeeAssetId      <- optAssetIdGen
      feeAmount          <- amountGen
      atomicBadge        <- atomicBadgeGen
      validationPolicy   <- validationPolicyGen
      contractApiVersion <- contractApiVersionGen
      timestamp          <- ntpTimestampGen
      payments           <- listContractTransferInV1Gen(transfersCountGen, transfersAssetGen)
    } yield
      CreateContractTransactionV5
        .selfSigned(
          signer,
          new String(image, UTF_8),
          imageHash,
          new String(contractName, UTF_8),
          params,
          feeAmount,
          timestamp,
          optFeeAssetId,
          atomicBadge,
          validationPolicy,
          contractApiVersion,
          payments
        )
        .explicitGet()

  def listContractTransferInV1Gen(transfersGen: Gen[List[(Option[AssetId], Long)]]): Gen[List[ContractTransferInV1]] =
    for {
      listTransfers <- transfersGen.flatMap(list => list.map((ContractTransferInV1.apply _).tupled) _)
    } yield listTransfers

  def genCallContractWithTransfers(signer: PrivateKeyAccount,
                                   contractId: ByteStr,
                                   transfersGen: Gen[List[(Option[AssetId], Long)]]): Gen[CallContractTransactionV5] =
    for {
      transfers     <- listContractTransferInV1Gen(transfersGen)
      callFeeAmount <- callTxFeeGen
      tx            <- callContractV5ParamGen(atomicBadgeGen = None)
    } yield
      CallContractTransactionV5
        .selfSigned(signer,
                    contractId,
                    tx.params,
                    callFeeAmount,
                    tx.timestamp,
                    contractVersion = 1,
                    feeAssetId = none,
                    atomicBadge = tx.atomicBadge,
                    payments = transfers)
        .explicitGet()

  def genContractTransferOutV1(optAssetId: Option[AssetId], amount: Long): Gen[ContractAssetOperation.ContractTransferOutV1] =
    for {
      recipient <- addressGen
    } yield ContractAssetOperation.ContractTransferOutV1(optAssetId, recipient, amount)

  def genContractIssueWithUniqueNonces(txId: ByteStr, issuedNonces: Set[Byte] = Set.empty): Gen[ContractAssetOperation.ContractIssueV1] = {
    val validNoncesSet = Seq.range(Byte.MinValue, Byte.MaxValue).toSet - 0.toByte

    for {
      nonce       <- Gen.oneOf(validNoncesSet -- issuedNonces)
      name        <- genBoundedString(IssueTransactionValidation.MinAssetNameLength, IssueTransactionValidation.MaxAssetNameLength)
      description <- genBoundedString(0, IssueTransactionValidation.MaxDescriptionLength)
      decimals    <- Gen.choose(0: Byte, 8: Byte)
      assetId = ByteStr(crypto.fastHash(txId.arr :+ nonce))
    } yield
      ContractAssetOperation.ContractIssueV1(assetId,
                                             new String(name, UTF_8),
                                             new String(description, UTF_8),
                                             Long.MaxValue / 3,
                                             decimals,
                                             isReissuable = true,
                                             nonce)
  }

  def listAssetOperation(executableTxId: ByteStr,
                         initIssueCount: Int,
                         maxOperations: Int,
                         withoutAssetOperations: Boolean): Gen[List[ContractAssetOperation]] = {
    @tailrec
    def genContractOperations(leftOperations: Int,
                              availableNoncesState: (Map[Byte, AssetId], Set[Byte]),
                              resultList: List[ContractAssetOperation] = List.empty): Gen[List[ContractAssetOperation]] = leftOperations match {
      case 0 => Gen.const(resultList)
      case _ =>
        val (usedNoncesAssetIdMap, burnedAssetNonces) = availableNoncesState
        val nextOperation = (usedNoncesAssetIdMap.keySet -- burnedAssetNonces) match {
          case availableIds if availableIds.nonEmpty =>
            for {
              realAssetId    <- Gen.oneOf(availableIds).map(n => ByteStr(crypto.fastHash(executableTxId.arr :+ n)))
              assetIdForSend <- Gen.frequency(80 -> realAssetId.some, 20 -> none)
              opAmount       <- smallFeeGen
              genOperation <- Gen.frequency(
                (20, genContractIssueWithUniqueNonces(executableTxId, usedNoncesAssetIdMap.keySet)),
                (20, ContractAssetOperation.ContractReissueV1(realAssetId, opAmount, isReissuable = true)),
                (10, ContractAssetOperation.ContractBurnV1(realAssetId.some, opAmount)),
                (50, genContractTransferOutV1(assetIdForSend, opAmount))
              )
            } yield genOperation
          case emptyIds if emptyIds.isEmpty =>
            for {
              amount <- smallFeeGen
              transferOut <- Gen.frequency(
                (if (!withoutAssetOperations) 30 else 0, genContractIssueWithUniqueNonces(executableTxId, usedNoncesAssetIdMap.keySet)),
                (70, genContractTransferOutV1(none, amount))
              )
            } yield transferOut

        }

        nextOperation.retryUntil(o => o.isInstanceOf[ContractAssetOperation], 5).sample.get match {
          case issue: ContractAssetOperation.ContractIssueV1 =>
            val newAssetId = ByteStr(crypto.fastHash(executableTxId.arr :+ issue.nonce))
            genContractOperations(
              leftOperations - 1,
              (usedNoncesAssetIdMap + (issue.nonce -> newAssetId), burnedAssetNonces),
              resultList :+ issue
            )
          case reissue: ContractAssetOperation.ContractReissueV1 =>
            genContractOperations(
              leftOperations - 1,
              availableNoncesState,
              resultList :+ reissue
            )
          case burn: ContractAssetOperation.ContractBurnV1 =>
            genContractOperations(
              leftOperations - 1,
              (usedNoncesAssetIdMap, burnedAssetNonces - usedNoncesAssetIdMap.filter(kv => burn.assetId.get == kv._2).head._1),
              resultList :+ burn
            )
          case sendOut: ContractAssetOperation.ContractTransferOutV1 =>
            genContractOperations(
              leftOperations - 1,
              availableNoncesState,
              resultList :+ sendOut
            )
        }
    }

    @tailrec
    def genContractIssues(leftIssues: Int,
                          usedNonces: Set[Byte] = Set.empty,
                          resultList: List[ContractAssetOperation.ContractIssueV1] = List.empty): Gen[List[ContractAssetOperation.ContractIssueV1]] =
      leftIssues match {
        case 0 => Gen.const(resultList)
        case _ =>
          val newIssue = genContractIssueWithUniqueNonces(executableTxId, usedNonces).sample.get
          genContractIssues(leftIssues - 1, usedNonces + newIssue.nonce, resultList :+ newIssue)
      }

    for {
      initIssues <- genContractIssues(initIssueCount)
      issuesNoncesIds = initIssues.map(i => i.nonce -> ByteStr(crypto.fastHash(executableTxId.arr :+ i.nonce))).toMap
      result <- genContractOperations(
        maxOperations - initIssueCount,
        (issuesNoncesIds, Set.empty),
        resultList = initIssues
      )
    } yield result
  }
}
