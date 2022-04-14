package com.wavesenterprise.transaction.docker

import com.wavesenterprise.account.PrivateKeyAccount
import com.wavesenterprise.crypto
import com.wavesenterprise.docker.ContractApiVersion
import com.wavesenterprise.docker.validator.ValidationPolicy
import com.wavesenterprise.settings.TestFees.{defaultFees => fees}
import com.wavesenterprise.state._
import com.wavesenterprise.transaction._
import com.wavesenterprise.utils.EitherUtils.EitherExt
import org.apache.commons.codec.digest.DigestUtils
import org.scalacheck.Gen
import org.scalatest.Suite

import java.nio.charset.StandardCharsets.UTF_8
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

  protected val createTxFeeGen: Gen[Long] = Gen.const(fees.forTxType(CreateContractTransaction.typeId))
  private val callTxFeeGen: Gen[Long]     = Gen.const(fees.forTxType(CallContractTransaction.typeId))
  protected val updateTxFeeGen: Gen[Long] = Gen.const(fees.forTxType(UpdateContractTransaction.typeId))
  private val disableTxFeeGen: Gen[Long]  = Gen.const(fees.forTxType(DisableContractTransaction.typeId))

  val createContractV1ParamGen: Gen[CreateContractTransactionV1] = for {
    signer       <- accountGen
    image        <- genBoundedString(CreateContractTransactionV1.ImageMinLength, CreateContractTransactionV1.ImageMaxLength)
    imageHash    <- bytes32gen.map(DigestUtils.sha256Hex)
    contractName <- genBoundedString(1, CreateContractTransactionV1.ContractNameMaxLength)
    paramsSize   <- Gen.choose(min = ParamsMinCount, max = ParamsMaxCount)
    params       <- Gen.listOfN(paramsSize, dataEntryGen(DataEntryMaxSize))
    feeAmount    <- createTxFeeGen
    timestamp    <- ntpTimestampGen
  } yield
    CreateContractTransactionV1
      .selfSigned(signer, new String(image, UTF_8), imageHash, new String(contractName, UTF_8), params, feeAmount, timestamp)
      .explicitGet()

  val createContractV2ParamGen: Gen[CreateContractTransactionV2] = createContractV2ParamGen((assetIdGen, createTxFeeGen))

  val createContractV2ParamGenWithoutSponsoring: Gen[CreateContractTransactionV2] = createContractV2ParamGen((Gen.const(None), createTxFeeGen))

  def createContractV2ParamGen(feeAssetIdGen: (Gen[Option[AssetId]], Gen[Long]),
                               signerGen: Gen[PrivateKeyAccount] = accountGen): Gen[CreateContractTransactionV2] =
    for {
      signer       <- signerGen
      image        <- genBoundedString(CreateContractTransactionV2.ImageMinLength, CreateContractTransactionV2.ImageMaxLength)
      imageHash    <- bytes32gen.map(DigestUtils.sha256Hex)
      contractName <- genBoundedString(1, CreateContractTransactionV2.ContractNameMaxLength)
      paramsSize   <- Gen.choose(min = ParamsMinCount, max = ParamsMaxCount)
      params       <- Gen.listOfN(paramsSize, dataEntryGen(DataEntryMaxSize))
      feeAmount    <- feeAssetIdGen._2
      feeAssetId   <- feeAssetIdGen._1
      timestamp    <- ntpTimestampGen
    } yield
      CreateContractTransactionV2
        .selfSigned(signer, new String(image, UTF_8), imageHash, new String(contractName, UTF_8), params, feeAmount, timestamp, feeAssetId)
        .explicitGet()

  def createContractV3ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]] = atomicBadgeOptGen): Gen[CreateContractTransactionV3] = {
    createContractV3ParamGen(atomicBadgeGen, (assetIdGen, createTxFeeGen), accountGen)
  }

  def createContractV3ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]],
                               feeAssetIdGen: (Gen[Option[AssetId]], Gen[Long]),
                               signerGen: Gen[PrivateKeyAccount]): Gen[CreateContractTransactionV3] =
    for {
      signer       <- signerGen
      image        <- genBoundedString(CreateContractTransactionV3.ImageMinLength, CreateContractTransactionV3.ImageMaxLength)
      imageHash    <- bytes32gen.map(DigestUtils.sha256Hex)
      contractName <- genBoundedString(1, CreateContractTransactionV3.ContractNameMaxLength)
      paramsSize   <- Gen.choose(min = ParamsMinCount, max = ParamsMaxCount)
      params       <- Gen.listOfN(paramsSize, dataEntryGen(DataEntryMaxSize))
      feeAmount    <- feeAssetIdGen._2
      feeAssetId   <- feeAssetIdGen._1
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
                    feeAssetId,
                    atomicBadge)
        .explicitGet()

  def createContractV4ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]] = atomicBadgeOptGen,
                               validationPolicyGen: Gen[ValidationPolicy] = validationPolicyGen,
                               contractApiVersionGen: Gen[ContractApiVersion] = contractApiVersionGen): Gen[CreateContractTransactionV4] = {
    createContractV4ParamGen(atomicBadgeGen, (assetIdGen, createTxFeeGen), accountGen, validationPolicyGen, contractApiVersionGen)
  }

  def createContractV4ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]],
                               feeAssetIdGen: (Gen[Option[AssetId]], Gen[Long]),
                               signerGen: Gen[PrivateKeyAccount],
                               validationPolicyGen: Gen[ValidationPolicy],
                               contractApiVersionGen: Gen[ContractApiVersion]): Gen[CreateContractTransactionV4] =
    for {
      signer             <- signerGen
      image              <- genBoundedString(CreateContractTransactionV4.ImageMinLength, CreateContractTransactionV4.ImageMaxLength)
      imageHash          <- bytes32gen.map(DigestUtils.sha256Hex)
      contractName       <- genBoundedString(1, CreateContractTransactionV4.ContractNameMaxLength)
      paramsSize         <- Gen.choose(min = ParamsMinCount, max = ParamsMaxCount)
      params             <- Gen.listOfN(paramsSize, dataEntryGen(DataEntryMaxSize))
      feeAmount          <- feeAssetIdGen._2
      feeAssetId         <- feeAssetIdGen._1
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
          feeAssetId,
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
    paramsSize <- Gen.choose(min = ParamsMinCount, max = ParamsMaxCount)
    params     <- Gen.listOfN(paramsSize, dataEntryGen(DataEntryMaxSize))
    feeAmount  <- callTxFeeGen
    timestamp  <- ntpTimestampGen
  } yield CallContractTransactionV1.selfSigned(signer, contractId, params, feeAmount, timestamp).explicitGet()

  val callContractV2ParamGen: Gen[CallContractTransactionV2] = for {
    signer          <- accountGen
    tx              <- callContractV1ParamGen
    contractVersion <- Gen.choose(1, Integer.MAX_VALUE)
  } yield CallContractTransactionV2.selfSigned(signer, tx.contractId, tx.params, tx.fee, tx.timestamp, contractVersion).explicitGet()

  val callContractV3ParamGen: Gen[CallContractTransactionV3] = callContractV3ParamGen((assetIdGen, callTxFeeGen))

  val callContractV3ParamGenWithoutSponsoring: Gen[CallContractTransactionV3] = callContractV3ParamGen((Gen.const(None), callTxFeeGen))

  def callContractV4ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]] = atomicBadgeOptGen): Gen[CallContractTransactionV4] = {
    callContractV4ParamGen(atomicBadgeGen, (assetIdGen, callTxFeeGen))
  }

  val bigCallContractV1ParamGen: Gen[CallContractTransactionV1] = for {
    tx     <- callContractV1ParamGen
    sender <- accountGen
    params <- Gen.listOfN(1, binaryEntryGen(BigDataEntryMaxSize))
  } yield CallContractTransactionV1.selfSigned(sender, tx.contractId, params, tx.fee, tx.timestamp).explicitGet()

  val executedContractV1ParamGen: Gen[ExecutedContractTransactionV1] = for {
    sender      <- accountGen
    tx          <- Gen.oneOf(createContractV1ParamGen, createContractV2ParamGen, callContractV1ParamGen, callContractV2ParamGen, callContractV3ParamGen)
    resultsSize <- Gen.choose(min = ResultsMinCount, max = ResultsMaxCount)
    results     <- Gen.listOfN(resultsSize, dataEntryGen(DataEntryMaxSize))
    timestamp   <- ntpTimestampGen
  } yield ExecutedContractTransactionV1.selfSigned(sender, tx, results, timestamp).explicitGet()

  val executedContractV2ParamGen: Gen[ExecutedContractTransactionV2] = for {
    sender      <- accountGen
    tx          <- Gen.oneOf(createContractV1ParamGen, createContractV2ParamGen, callContractV1ParamGen, callContractV2ParamGen, callContractV3ParamGen)
    resultsSize <- Gen.choose(min = ResultsMinCount, max = ResultsMaxCount)
    results     <- Gen.listOfN(resultsSize, dataEntryGen(DataEntryMaxSize))
    resultsHash = ContractTransactionValidation.resultsHash(results)
    validationProofs <- validationProofsGen(resultsHash)
    timestamp        <- ntpTimestampGen
  } yield ExecutedContractTransactionV2.selfSigned(sender, tx, results, resultsHash, validationProofs, timestamp).explicitGet()

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

  val disableContractV1ParamGen: Gen[DisableContractTransactionV1] = for {
    sender     <- accountGen
    contractId <- Gen.oneOf(createContractV1ParamGen, createContractV2ParamGen).map(_.id())
    feeAmount  <- smallFeeGen
    timestamp  <- ntpTimestampGen
  } yield DisableContractTransactionV1.selfSigned(sender, contractId, feeAmount, timestamp).explicitGet()

  val disableContractV2ParamGen: Gen[DisableContractTransactionV2] = disableContractV2ParamGen((assetIdGen, disableTxFeeGen))

  val disableContractV2ParamGenWithoutSponsoring: Gen[DisableContractTransactionV2] = disableContractV2ParamGen((Gen.const(None), disableTxFeeGen))

  def disableContractV3ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]] = atomicBadgeOptGen): Gen[DisableContractTransactionV3] = {
    disableContractV3ParamGen(atomicBadgeGen, (assetIdGen, disableTxFeeGen))
  }

  val updateContractV1ParamGen: Gen[UpdateContractTransactionV1] = for {
    sender     <- accountGen
    contractId <- Gen.oneOf(createContractV1ParamGen, createContractV2ParamGen).map(_.id())
    image      <- genBoundedString(UpdateContractTransactionV1.ImageMinLength, UpdateContractTransactionV1.ImageMaxLength)
    imageHash  <- bytes32gen.map(DigestUtils.sha256Hex)
    feeAmount  <- smallFeeGen
    timestamp  <- ntpTimestampGen
  } yield UpdateContractTransactionV1.selfSigned(sender, contractId, new String(image, UTF_8), imageHash, feeAmount, timestamp).explicitGet()

  val updateContractV2ParamGen: Gen[UpdateContractTransactionV2] = updateContractV2ParamGen((assetIdGen, updateTxFeeGen))

  val updateContractV2ParamGenWithoutSponsoring: Gen[UpdateContractTransactionV2] = updateContractV2ParamGen((Gen.const(None), updateTxFeeGen))

  def updateContractV3ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]] = atomicBadgeOptGen): Gen[UpdateContractTransactionV3] = {
    updateContractV3ParamGen(atomicBadgeGen, (assetIdGen, updateTxFeeGen))
  }

  def executedContractV1ParamGen(signer: PrivateKeyAccount, tx: ExecutableTransaction): Gen[ExecutedContractTransactionV1] =
    for {
      resultsSize <- Gen.choose(min = ResultsMinCount, max = ResultsMaxCount)
      results     <- Gen.listOfN(resultsSize, dataEntryGen(DataEntryMaxSize))
      timestamp   <- ntpTimestampGen
    } yield ExecutedContractTransactionV1.selfSigned(signer, tx, results, timestamp).explicitGet()

  def executedContractV2ParamGen(signer: PrivateKeyAccount,
                                 tx: ExecutableTransaction,
                                 resultsHashTransformer: ByteStr => ByteStr = identity,
                                 validationProofsTransformer: List[ValidationProof] => List[ValidationProof] = identity,
                                 proofsCount: Int = 10,
                                 specifiedValidators: List[PrivateKeyAccount] = List.empty): Gen[ExecutedContractTransactionV2] =
    for {
      resultsSize <- Gen.choose(min = ResultsMinCount, max = ResultsMaxCount)
      results     <- Gen.listOfN(resultsSize, dataEntryGen(DataEntryMaxSize))
      resultsHash = resultsHashTransformer(ContractTransactionValidation.resultsHash(results))
      validationProofs <- validationProofsGen(resultsHash, proofsCount, specifiedValidators).map(validationProofsTransformer)
      timestamp        <- ntpTimestampGen
    } yield ExecutedContractTransactionV2.selfSigned(signer, tx, results, resultsHash, validationProofs, timestamp).explicitGet()

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

  def callContractV3ParamGen(feeAssetIdGen: (Gen[Option[AssetId]], Gen[Long])): Gen[CallContractTransactionV3] =
    for {
      signer          <- accountGen
      tx              <- callContractV1ParamGen
      contractVersion <- Gen.choose(1, Integer.MAX_VALUE)
      feeAmount       <- feeAssetIdGen._2
      feeAssetId      <- feeAssetIdGen._1
    } yield CallContractTransactionV3.selfSigned(signer, tx.contractId, tx.params, feeAmount, tx.timestamp, contractVersion, feeAssetId).explicitGet()

  def callContractV3ParamGen(feeAssetIdGen: (Gen[Option[AssetId]], Gen[Long]),
                             signer: PrivateKeyAccount,
                             contractId: ByteStr,
                             contractVersion: Int): Gen[CallContractTransactionV3] =
    callContractV3ParamGen(feeAssetIdGen).map { tx =>
      CallContractTransactionV3.selfSigned(signer, contractId, tx.params, tx.fee, tx.timestamp, contractVersion, tx.feeAssetId).explicitGet()
    }

  def callContractV4ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]],
                             feeAssetIdGen: (Gen[Option[AssetId]], Gen[Long])): Gen[CallContractTransactionV4] =
    for {
      signer          <- accountGen
      tx              <- callContractV1ParamGen
      contractVersion <- Gen.choose(1, Integer.MAX_VALUE)
      feeAmount       <- feeAssetIdGen._2
      feeAssetId      <- feeAssetIdGen._1
      atomicBadge     <- atomicBadgeGen
    } yield
      CallContractTransactionV4
        .selfSigned(signer, tx.contractId, tx.params, feeAmount, tx.timestamp, contractVersion, feeAssetId, atomicBadge)
        .explicitGet()

  def updateContractV1ParamGen(signer: PrivateKeyAccount, createTx: CreateContractTransaction): Gen[UpdateContractTransactionV1] =
    updateContractV1ParamGen.map { tx =>
      UpdateContractTransactionV1.selfSigned(signer, createTx.contractId, tx.image, tx.imageHash, tx.fee, tx.timestamp).explicitGet()
    }

  def updateContractV2ParamGenWithoutSponsoring(signer: PrivateKeyAccount, createTx: CreateContractTransaction): Gen[UpdateContractTransactionV2] =
    updateContractV2ParamGenWithoutSponsoring.map { tx =>
      UpdateContractTransactionV2.selfSigned(signer, createTx.contractId, tx.image, tx.imageHash, tx.fee, tx.timestamp, tx.feeAssetId).explicitGet()
    }

  def updateContractV2ParamGen(feeAssetIdGen: (Gen[Option[AssetId]], Gen[Long]),
                               signer: PrivateKeyAccount,
                               contractId: ByteStr): Gen[UpdateContractTransactionV2] =
    updateContractV2ParamGen(feeAssetIdGen).map { tx =>
      UpdateContractTransactionV2.selfSigned(signer, contractId, tx.image, tx.imageHash, tx.fee, tx.timestamp, tx.feeAssetId).explicitGet()
    }

  def disableContractV2ParamGen(feeAssetIdGen: (Gen[Option[AssetId]], Gen[Long]),
                                signer: PrivateKeyAccount,
                                contractId: ByteStr): Gen[DisableContractTransactionV2] =
    disableContractV2ParamGen(feeAssetIdGen).map { tx =>
      DisableContractTransactionV2.selfSigned(signer, contractId, tx.fee, tx.timestamp, tx.feeAssetId).explicitGet()
    }

  def updateContractV2ParamGen(feeAssetIdGen: (Gen[Option[AssetId]], Gen[Long])): Gen[UpdateContractTransactionV2] =
    for {
      sender     <- accountGen
      contractId <- Gen.oneOf(createContractV1ParamGen, createContractV2ParamGen).map(_.id())
      image      <- genBoundedString(UpdateContractTransactionV2.ImageMinLength, UpdateContractTransactionV2.ImageMaxLength)
      imageHash  <- bytes32gen.map(DigestUtils.sha256Hex)
      timestamp  <- ntpTimestampGen
      feeAssetId <- feeAssetIdGen._1
      feeAmount  <- feeAssetIdGen._2
    } yield
      UpdateContractTransactionV2.selfSigned(sender, contractId, new String(image, UTF_8), imageHash, feeAmount, timestamp, feeAssetId).explicitGet()

  def updateContractV3ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]],
                               feeAssetIdGen: (Gen[Option[AssetId]], Gen[Long])): Gen[UpdateContractTransactionV3] =
    for {
      sender      <- accountGen
      atomicBadge <- atomicBadgeGen
      contractId  <- Gen.oneOf(createContractV1ParamGen, createContractV3ParamGen(atomicBadge)).map(_.id())
      image       <- genBoundedString(UpdateContractTransactionV2.ImageMinLength, UpdateContractTransactionV2.ImageMaxLength)
      imageHash   <- bytes32gen.map(DigestUtils.sha256Hex)
      timestamp   <- ntpTimestampGen
      feeAssetId  <- feeAssetIdGen._1
      feeAmount   <- feeAssetIdGen._2
    } yield
      UpdateContractTransactionV3
        .selfSigned(sender, contractId, new String(image, UTF_8), imageHash, feeAmount, timestamp, feeAssetId, atomicBadge)
        .explicitGet()

  def updateContractV4ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]] = atomicBadgeOptGen,
                               validationPolicyGen: Gen[ValidationPolicy] = validationPolicyGen,
                               contractApiVersionGen: Gen[ContractApiVersion] = contractApiVersionGen,
                               contractIdGen: Gen[ByteStr] = bytes32gen.map(ByteStr.apply)): Gen[UpdateContractTransactionV4] = {
    updateContractV4ParamGen(atomicBadgeGen, (assetIdGen, createTxFeeGen), accountGen, validationPolicyGen, contractApiVersionGen, contractIdGen)
  }

  def updateContractV4ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]],
                               feeAssetIdGen: (Gen[Option[AssetId]], Gen[Long]),
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
      feeAmount          <- feeAssetIdGen._2
      feeAssetId         <- feeAssetIdGen._1
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
                    feeAssetId,
                    atomicBadge,
                    validationPolicy,
                    contractApiVersion)
        .explicitGet()

  def disableContractV2ParamGen(feeAssetIdGen: (Gen[Option[AssetId]], Gen[Long])): Gen[DisableContractTransactionV2] =
    for {
      sender     <- accountGen
      contractId <- Gen.oneOf(createContractV1ParamGen, createContractV2ParamGen).map(_.id())
      timestamp  <- ntpTimestampGen
      feeAssetId <- feeAssetIdGen._1
      feeAmount  <- feeAssetIdGen._2
    } yield DisableContractTransactionV2.selfSigned(sender, contractId, feeAmount, timestamp, feeAssetId).explicitGet()

  def disableContractV3ParamGen(atomicBadgeGen: Gen[Option[AtomicBadge]],
                                feeAssetIdGen: (Gen[Option[AssetId]], Gen[Long])): Gen[DisableContractTransactionV3] =
    for {
      sender      <- accountGen
      contractId  <- Gen.oneOf(createContractV1ParamGen, createContractV2ParamGen).map(_.id())
      timestamp   <- ntpTimestampGen
      feeAssetId  <- feeAssetIdGen._1
      feeAmount   <- feeAssetIdGen._2
      atomicBadge <- atomicBadgeGen
    } yield DisableContractTransactionV3.selfSigned(sender, contractId, feeAmount, timestamp, feeAssetId, atomicBadge).explicitGet()

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
      count             <- Gen.chooseNum(2, maxCount)
      innerTransactions <- Gen.listOfN(count, atomicSingleInnerTxGen(atomicBadge))
    } yield innerTransactions

  val atomicTxV1Gen: Gen[AtomicTransaction] =
    for {
      signer   <- accountGen
      badge    <- atomicBadgeGen
      atomicTx <- atomicTxV1MultipleInnerGen(signer, atomicInnerTxsGen(Some(badge), 5))
    } yield atomicTx
}
