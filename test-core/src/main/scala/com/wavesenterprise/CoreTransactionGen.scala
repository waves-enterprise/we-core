package com.wavesenterprise

import cats.syntax.semigroup._
import com.wavesenterprise.account.PublicKeyAccount._
import com.wavesenterprise.account._
import com.wavesenterprise.acl.OpType.{Add, Remove}
import com.wavesenterprise.acl.{OpType, PermissionOp, PermissionsGen, Role}
import com.wavesenterprise.lang.WavesGlobal
import com.wavesenterprise.lang.ScriptVersion.Versions.V1
import com.wavesenterprise.lang.v1.compiler.CompilerV1
import com.wavesenterprise.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesenterprise.lang.v1.testing.ScriptGen
import com.wavesenterprise.privacy.PolicyDataHash
import com.wavesenterprise.settings.TestFees
import com.wavesenterprise.state._
import com.wavesenterprise.transaction._
import com.wavesenterprise.transaction.acl.{PermitTransactionV1, PermitTransactionV2}
import com.wavesenterprise.transaction.assets._
import com.wavesenterprise.transaction.assets.exchange._
import com.wavesenterprise.transaction.lease._
import com.wavesenterprise.transaction.smart.script.Script
import com.wavesenterprise.transaction.smart.script.v1.ScriptV1
import com.wavesenterprise.transaction.smart.{SetScriptTransactionV1, SetScriptValidation}
import com.wavesenterprise.transaction.transfer._
import com.wavesenterprise.transaction.validation.TransferValidation.MaxTransferCount
import com.wavesenterprise.transaction.validation.{DataValidation, IssueTransactionValidation, PolicyValidation, TransferValidation}
import com.wavesenterprise.utils.EitherUtils.EitherExt
import com.wavesenterprise.utils.NumberUtils.DoubleExt
import org.scalacheck.Gen.{alphaLowerChar, alphaUpperChar, frequency, numChar}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Suite

import java.nio.charset.StandardCharsets.UTF_8
import scala.util.Random

trait CoreTransactionGen extends ScriptGen with CommonGen with NTPTime { _: Suite =>

  def currentChainId: Byte = AddressScheme.getAddressSchema.chainId

  protected def west(n: Float): Long = (n * 100000000L).toLong

  def genBoundedBytes(minSize: Int, maxSize: Int): Gen[Array[Byte]] =
    for {
      length <- Gen.chooseNum(minSize, maxSize)
      bytes  <- byteArrayGen(length)
    } yield bytes

  def genBoundedString(minSize: Int, maxSize: Int): Gen[Array[Byte]] = {
    Gen.choose(minSize, maxSize) flatMap { sz =>
      Gen.listOfN(sz, Gen.choose(0, 0x7f).map(_.toByte)).map(_.toArray)
    }
  }

  val ntpTimestampGen: Gen[Long] = Gen.choose(1, 1000).map(ntpTime.correctedTime() - _)

  val aliasSymbolChar: Gen[Char] = Gen.oneOf('.', '@', '_', '-')

  val invalidAliasSymbolChar: Gen[Char] = Gen.oneOf('~', '`', '!', '#', '$', '%', '^', '&', '*', '=', '+')

  val aliasAlphabetGen: Gen[Char] = frequency((1, numChar), (1, aliasSymbolChar), (9, alphaLowerChar))

  val invalidAliasAlphabetGen: Gen[Char] = frequency((1, numChar), (3, invalidAliasSymbolChar), (9, alphaUpperChar))

  val validAliasStringGen: Gen[String] = for {
    length     <- Gen.chooseNum(Alias.MinLength, Alias.MaxLength)
    aliasChars <- Gen.listOfN(length, aliasAlphabetGen)
  } yield aliasChars.mkString

  val aliasGen: Gen[Alias] = for {
    str <- validAliasStringGen
  } yield Alias.buildWithCurrentChainId(str.mkString).explicitGet()

  val invalidAliasStringGen: Gen[String] = for {
    length     <- Gen.chooseNum(Alias.MinLength, Alias.MaxLength)
    aliasChars <- Gen.listOfN(length, invalidAliasAlphabetGen)
  } yield aliasChars.mkString

  val addressGen: Gen[Address]               = accountGen.map(_.toAddress)
  val accountOrAliasGen: Gen[AddressOrAlias] = Gen.oneOf(aliasGen, addressGen)
  val policyDataHashGen: Gen[(Array[Byte], PolicyDataHash)] =
    byteArrayGen(PolicyDataHash.DataHashLength).map(bytes => (bytes, PolicyDataHash.fromDataBytes(bytes)))

  protected def severalGenerators[T](generator: Gen[T], min: Int, max: Int): Gen[List[T]] = {
    for {
      addressesCnt <- Gen.choose(min, max)
      elements     <- Gen.listOfN(addressesCnt, generator)
    } yield elements
  }

  def severalAddressGenerator(min: Int = 2, max: Int = 20): Gen[List[Address]] = {
    severalGenerators(addressGen, min, max)
  }

  val positiveLongGen: Gen[Long] = Gen.choose(1, 100000000L * 100000000L / 100)
  val positiveIntGen: Gen[Int]   = Gen.choose(1, Int.MaxValue / 100)
  val smallFeeGen: Gen[Long]     = Gen.choose(1000000, 100000000)

  val maxOrderTimeGen: Gen[Long] = Gen.choose(10000L, Order.MaxLiveTime).map(_ + ntpTime.correctedTime())

  val timestampGen: Gen[Long]       = Gen.choose(1, Long.MaxValue - 100)
  protected val lowerBound: Long    = Long.MaxValue / 4
  protected val upperBound: Long    = (Long.MaxValue / 4) * 3
  val narrowTimestampGen: Gen[Long] = Gen.choose(lowerBound, upperBound)

  val westAssetGen: Gen[Option[ByteStr]] = Gen.const(None)
  val assetIdGen: Gen[Option[ByteStr]]   = Gen.frequency((1, westAssetGen), (10, Gen.option(bytes32gen.map(ByteStr(_)))))

  val assetPairGen = assetIdGen.flatMap {
    case None => bytes32gen.map(b => AssetPair(None, Some(ByteStr(b))))
    case a1 @ Some(a1bytes) =>
      val a2bytesGen = byteArrayGen(31).map(a2bytes => Option((~a1bytes.arr(0)).toByte +: a2bytes))
      Gen.oneOf(Gen.const(None), a2bytesGen).map(a2 => AssetPair(a1, a2.map(ByteStr(_))))
  }

  val proofsGen: Gen[Proofs] = for {
    proofsAmount <- Gen.choose(1, 8)
    proofs       <- Gen.listOfN(proofsAmount, genBoundedBytes(0, 50))
  } yield Proofs.create(proofs.map(ByteStr(_))).explicitGet()

  val scriptGen = BOOLgen(100).map {
    case (expr, _) =>
      val typed =
        CompilerV1(PureContext.build(V1).compilerContext |+| CryptoContext.compilerContext(WavesGlobal), expr).explicitGet()
      ScriptV1(typed._1).explicitGet()
  }

  val issueParamGen = for {
    sender: PrivateKeyAccount <- accountGen
    assetName                 <- genBoundedString(IssueTransactionValidation.MinAssetNameLength, IssueTransactionValidation.MaxAssetNameLength)
    description               <- genBoundedString(0, IssueTransactionValidation.MaxDescriptionLength)
    quantity                  <- Gen.choose(Long.MaxValue / 200, Long.MaxValue / 100)
    decimals                  <- Gen.choose(0: Byte, 8: Byte)
    reissuable                <- Arbitrary.arbitrary[Boolean]
    fee                       <- Gen.choose(MinIssueFee, 2 * MinIssueFee)
    timestamp                 <- timestampGen
  } yield (sender, assetName, description, quantity, decimals, reissuable, fee, timestamp)

  val setAssetScriptTransactionGen: Gen[(Seq[Transaction], SetAssetScriptTransactionV1)] = {
    for {
      (sender, assetName, description, quantity, decimals, _, iFee, _) <- issueParamGen
      timestamp                                                        <- timestampGen
      proofs                                                           <- proofsGen
      script                                                           <- Gen.option(scriptGen)
      issue = IssueTransactionV2
        .selfSigned(currentChainId, sender, assetName, description, quantity, decimals, reissuable = true, iFee, timestamp, script)
        .explicitGet()
    } yield {
      (Seq(issue),
       SetAssetScriptTransactionV1
         .create(currentChainId, sender, issue.id(), script, 1.west, timestamp, proofs)
         .explicitGet())
    }
  }

  val setScriptTransactionGen: Gen[SetScriptTransactionV1] =
    for {
      sender: PrivateKeyAccount <- accountGen
      fee                       <- smallFeeGen
      timestamp                 <- timestampGen
      proofs                    <- proofsGen
      script                    <- Gen.option(scriptGen)
      name                      <- genBoundedString(1, SetScriptValidation.MaxNameSize)
      description               <- genBoundedString(1, 1024) // to fit in BaseGlobal#MaxBase64String size for base64 presentation of tx
    } yield SetScriptTransactionV1
      .create(currentChainId, sender, script, name, description, fee, timestamp, proofs)
      .explicitGet()

  protected val leaseParamGen = for {
    sender    <- accountGen
    amount    <- positiveLongGen
    fee       <- smallFeeGen
    timestamp <- ntpTimestampGen
    recipient <- addressGen
  } yield (sender, amount, fee, timestamp, recipient)

  def createLease(sender: PrivateKeyAccount,
                  amount: Long,
                  fee: Long,
                  timestamp: Long,
                  recipient: AddressOrAlias,
                  atomicBadge: Option[AtomicBadge]): Gen[(LeaseTransactionV2, LeaseTransactionV3)] = {
    val v2 = LeaseTransactionV2.selfSigned(None, sender, recipient, amount, fee, timestamp).explicitGet()
    val v3 = LeaseTransactionV3.selfSigned(None, sender, recipient, amount, fee, timestamp, atomicBadge).explicitGet()
    Gen.const((v2, v3))
  }

  def createLeaseCancel(sender: PrivateKeyAccount,
                        leaseId: ByteStr,
                        cancelFee: Long,
                        timestamp: Long,
                        atomicBadge: Option[AtomicBadge]): Gen[(LeaseCancelTransactionV2, LeaseCancelTransactionV3)] = {
    val v2 = LeaseCancelTransactionV2
      .selfSigned(currentChainId, sender, cancelFee, timestamp + 1, leaseId)
      .explicitGet()
    val v3 = LeaseCancelTransactionV3
      .selfSigned(currentChainId, sender, cancelFee, timestamp + 1, leaseId, atomicBadge)
      .explicitGet()

    Gen.const((v2, v3))
  }
  val leaseAndCancelGen: Gen[(LeaseTransaction, LeaseTransactionV3, LeaseCancelTransaction, LeaseCancelTransactionV3)] = for {
    (sender, amount, fee, timestamp, recipient) <- leaseParamGen
    atomicBadgeOpt                              <- atomicBadgeOptGen
    (lease, leaseAtomic)                        <- createLease(sender, amount, fee, timestamp, recipient, atomicBadgeOpt)
    cancelFee                                   <- smallFeeGen
    (leaseCancel, leaseCancelAtomic)            <- createLeaseCancel(sender, lease.id(), cancelFee, timestamp + 1, atomicBadgeOpt)
  } yield (lease, leaseAtomic, leaseCancel, leaseCancelAtomic)

  val leaseGen: Gen[LeaseTransaction]                 = leaseAndCancelGen.map(_._1)
  val leaseV3Gen: Gen[LeaseTransactionV3]             = leaseAndCancelGen.map(_._2)
  val leaseCancelGen: Gen[LeaseCancelTransaction]     = leaseAndCancelGen.map(_._3)
  val leaseCancelV3Gen: Gen[LeaseCancelTransactionV3] = leaseAndCancelGen.map(_._4)

  val leaseV2Gen: Gen[LeaseTransactionV2] = leaseParamGen.flatMap {
    case (sender, amount, fee, timestamp, recipient) =>
      createLease(sender, amount, fee, timestamp, recipient, None).map(_._1)
  }

  val transferParamGen = for {
    amount     <- positiveLongGen
    feeAmount  <- smallFeeGen
    assetId    <- Gen.option(bytes32gen)
    feeAssetId <- Gen.option(bytes32gen)
    timestamp  <- ntpTimestampGen
    sender     <- accountGen
    attachment <- genBoundedBytes(0, TransferValidation.MaxAttachmentSize)
    recipient  <- accountOrAliasGen
  } yield (assetId.map(ByteStr(_)), sender, recipient, amount, timestamp, feeAssetId.map(ByteStr(_)), feeAmount, attachment)

  def createWestTransfer(sender: PrivateKeyAccount,
                         recipient: Address,
                         amount: Long,
                         fee: Long,
                         timestamp: Long): Either[ValidationError, TransferTransactionV2] =
    TransferTransactionV2.selfSigned(sender, None, None, timestamp, amount, fee, recipient, Array())

  val transferV2Gen: Gen[TransferTransactionV2] = (for {
    (assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment) <- transferParamGen
  } yield TransferTransactionV2
    .selfSigned(sender, assetId, feeAssetId, timestamp, amount, feeAmount, recipient, attachment)
    .explicitGet())
    .label("VersionedTransferTransaction")

  def transferV3Gen(atomicBadgeGen: Gen[Option[AtomicBadge]] = atomicBadgeOptGen): Gen[TransferTransactionV3] =
    (for {
      (assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment) <- transferParamGen
      atomicBadge                                                                        <- atomicBadgeGen
    } yield {
      TransferTransactionV3
        .selfSigned(sender, assetId, feeAssetId, timestamp, amount, feeAmount, recipient, attachment, atomicBadge)
        .explicitGet()
    }).label("VersionedTransferTransaction")

  val massTransferV1WithAddressesOnlyGen: Gen[MassTransferTransactionV1] = massTransferV1Gen(recipientGen = addressGen)
  val massTransferV2WithAddressesOnlyGen: Gen[MassTransferTransactionV2] = massTransferV2Gen(recipientGen = addressGen)

  def massTransferV1Gen(minTransfersCount: Int = 0,
                        maxTransfersCount: Int = MaxTransferCount,
                        recipientGen: Gen[AddressOrAlias] = accountOrAliasGen): Gen[MassTransferTransactionV1] =
    for {
      (assetId, sender, recipients, timestamp, _, feeAmount, attachment) <- massTransferGen(minTransfersCount, maxTransfersCount, recipientGen)
    } yield MassTransferTransactionV1.selfSigned(sender, assetId, recipients, timestamp, feeAmount, attachment).explicitGet()

  def massTransferV2Gen(minTransfersCount: Int = 0,
                        maxTransfersCount: Int = MaxTransferCount,
                        recipientGen: Gen[AddressOrAlias] = accountOrAliasGen): Gen[MassTransferTransactionV2] =
    for {
      (assetId, sender, recipients, timestamp, feeAssetId, feeAmount, attachment) <-
        massTransferGen(minTransfersCount, maxTransfersCount, recipientGen)
    } yield MassTransferTransactionV2.selfSigned(sender, assetId, recipients, timestamp, feeAmount, attachment, feeAssetId).explicitGet()

  def massTransferV3Gen(minTransfersCount: Int = 0,
                        maxTransfersCount: Int = MaxTransferCount,
                        recipientGen: Gen[AddressOrAlias] = accountOrAliasGen): Gen[MassTransferTransactionV3] =
    for {
      (assetId, sender, recipients, timestamp, feeAssetId, feeAmount, attachment) <-
        massTransferGen(minTransfersCount, maxTransfersCount, recipientGen)
      atomicBadge <- atomicBadgeOptGen
    } yield MassTransferTransactionV3.selfSigned(sender, assetId, recipients, timestamp, feeAmount, attachment, feeAssetId, atomicBadge).explicitGet()

  protected def massTransferGen(minTransfersCount: Int, maxTransfersCount: Int, recipientGen: Gen[AddressOrAlias]) =
    for {
      (assetId, sender, _, _, timestamp, feeAssetId, feeAmount, attachment) <- transferParamGen
      transferCount                                                         <- Gen.choose(minTransfersCount, maxTransfersCount)
      transferGen = for {
        recipient <- recipientGen
        amount    <- Gen.choose(1L, Long.MaxValue / maxTransfersCount)
      } yield ParsedTransfer(recipient, amount)
      recipients <- Gen.listOfN(transferCount, transferGen)
    } yield (assetId, sender, recipients, timestamp, feeAssetId, feeAmount, attachment)

  val MinIssueFee: Long = TestFees.defaultFees.forTxType(IssueTransactionV2.typeId)

  val createAliasV2Gen: Gen[CreateAliasTransaction] = for {
    timestamp: Long           <- positiveLongGen
    sender: PrivateKeyAccount <- accountGen
    alias: Alias              <- aliasGen
    tx                        <- Gen.const(CreateAliasTransactionV2.selfSigned(sender, alias, MinIssueFee, timestamp).explicitGet())
  } yield tx

  def createAliasV2Gen(sender: PrivateKeyAccount, alias: Alias, fee: Long, timestamp: Long): Gen[CreateAliasTransaction] = {
    Gen.const(CreateAliasTransactionV2.selfSigned(sender, alias, fee, timestamp).explicitGet())
  }

  val createAliasV3Gen: Gen[CreateAliasTransaction] = for {
    timestamp: Long           <- positiveLongGen
    sender: PrivateKeyAccount <- accountGen
    alias: Alias              <- aliasGen
    feeAssetId                <- Gen.option(bytes32gen)
    tx                        <- Gen.const(CreateAliasTransactionV3.selfSigned(sender, alias, MinIssueFee, timestamp, feeAssetId.map(ByteStr(_))).explicitGet())
  } yield tx

  def createAliasV3Gen(sender: PrivateKeyAccount,
                       alias: Alias,
                       fee: Long,
                       timestamp: Long,
                       feeAssetId: Option[AssetId]): Gen[CreateAliasTransaction] = {
    Gen.const(CreateAliasTransactionV3.selfSigned(sender, alias, fee, timestamp, feeAssetId).explicitGet())
  }

  val createAliasV4Gen: Gen[CreateAliasTransactionV4] = for {
    timestamp: Long           <- positiveLongGen
    sender: PrivateKeyAccount <- accountGen
    alias: Alias              <- aliasGen
    feeAssetId                <- Gen.option(bytes32gen)
    atomicBadge               <- atomicBadgeOptGen
    tx                        <- Gen.const(CreateAliasTransactionV4.selfSigned(sender, alias, MinIssueFee, timestamp, feeAssetId.map(ByteStr(_)), atomicBadge).explicitGet())
  } yield tx

  def createAliasV4Gen(sender: PrivateKeyAccount,
                       alias: Alias,
                       fee: Long,
                       timestamp: Long,
                       feeAssetId: Option[AssetId],
                       atomicBadge: Option[AtomicBadge]): Gen[CreateAliasTransactionV4] = {
    Gen.const(CreateAliasTransactionV4.selfSigned(sender, alias, fee, timestamp, feeAssetId, atomicBadge).explicitGet())
  }

  val issueReissueBurnGen: Gen[(IssueTransaction, ReissueTransaction, BurnTransaction, BurnTransactionV3)] = for {
    amount                    <- positiveLongGen
    sender: PrivateKeyAccount <- accountGen
    r                         <- issueReissueBurnGeneratorP(amount, amount, amount, sender)
  } yield r

  def issueReissueBurnGeneratorP(issueQuantity: Long,
                                 sender: PrivateKeyAccount): Gen[(IssueTransaction, ReissueTransaction, BurnTransaction, BurnTransactionV3)] =
    issueReissueBurnGeneratorP(issueQuantity, issueQuantity, issueQuantity, sender)

  def createIssue(issuer: PrivateKeyAccount,
                  assetName: Array[Byte],
                  description: Array[Byte],
                  quantity: Long,
                  decimals: Byte,
                  reissuable: Boolean,
                  fee: Long,
                  timestamp: Long,
                  script: Option[Script] = None): Gen[IssueTransaction] = {
    Gen.const(
      IssueTransactionV2
        .selfSigned(currentChainId, issuer, assetName, description, quantity, decimals, reissuable, fee, timestamp, script)
        .explicitGet())
  }

  def createReissue(reissuer: PrivateKeyAccount,
                    assetId: ByteStr,
                    quantity: Long,
                    reissuable: Boolean,
                    fee: Long,
                    timestamp: Long): Gen[ReissueTransaction] = {
    Gen.const(
      ReissueTransactionV2
        .selfSigned(currentChainId, reissuer, assetId, quantity, reissuable, fee, timestamp)
        .explicitGet())
  }

  def createBurn(burner: PrivateKeyAccount, assetId: AssetId, amount: Long, fee: Long, timestamp: Long): Gen[BurnTransaction] = {
    Gen.const(BurnTransactionV2.selfSigned(currentChainId, burner, assetId, amount, fee, timestamp).explicitGet())
  }

  def createBurnV3(burner: PrivateKeyAccount,
                   assetId: AssetId,
                   amount: Long,
                   fee: Long,
                   timestamp: Long,
                   atomicBadgeOpt: Option[AtomicBadge]): Gen[BurnTransactionV3] = {
    Gen.const(BurnTransactionV3.selfSigned(currentChainId, burner, assetId, amount, fee, timestamp, atomicBadgeOpt).explicitGet())
  }

  protected val reissuanceGen: Gen[Boolean] = Arbitrary.arbitrary[Boolean]

  def issueReissueBurnGeneratorP(
      issueQuantity: Long,
      reissueQuantity: Long,
      burnQuantity: Long,
      sender: PrivateKeyAccount,
      script: Option[Script] = None,
      reissuableGen: Gen[Boolean] = reissuanceGen,
      timestampGen: Gen[Long] = timestampGen): Gen[(IssueTransaction, ReissueTransaction, BurnTransaction, BurnTransactionV3)] =
    for {
      (_, assetName, description, _, decimals, _, iFee, _) <- issueParamGen
      reissuable                                           <- reissuableGen
      timestamp                                            <- timestampGen
      reissuable2                                          <- reissuanceGen
      fee                                                  <- smallFeeGen
      issue                                                <- createIssue(sender, assetName, description, issueQuantity, decimals, reissuable, iFee, timestamp, script)
      reissue                                              <- createReissue(sender, issue.assetId(), reissueQuantity, reissuable2, fee, timestamp)
      burn                                                 <- createBurn(sender, issue.assetId(), burnQuantity, fee, timestamp)
      atomicBadgeOpt                                       <- atomicBadgeOptGen
      burnV3                                               <- createBurnV3(sender, issue.assetId(), burnQuantity, fee, timestamp, atomicBadgeOpt)
    } yield (issue, reissue, burn, burnV3)

  val issueGen: Gen[IssueTransaction]     = issueReissueBurnGen.map(_._1)
  val reissueGen: Gen[ReissueTransaction] = issueReissueBurnGen.map(_._2)
  val burnGen: Gen[BurnTransaction]       = issueReissueBurnGen.map(_._3)
  val burnV3Gen: Gen[BurnTransactionV3]   = issueReissueBurnGen.map(_._4)

  def issueGen(sender: PrivateKeyAccount, fixedQuantity: Option[Long] = None): Gen[IssueTransaction] =
    for {
      (_, assetName, description, quantity, decimals, _, _, timestamp) <- issueParamGen
    } yield {
      IssueTransactionV2
        .selfSigned(chainId = currentChainId,
                    sender,
                    assetName,
                    description,
                    fixedQuantity.getOrElse(quantity),
                    decimals,
                    reissuable = false,
                    MinIssueFee,
                    timestamp,
                    None)
        .explicitGet()
    }

  def sponsorFeeCancelSponsorFeeGen(
      sender: PrivateKeyAccount,
      ts: Option[Long] = None,
      atomicBadge: Option[AtomicBadge] = None)
      : Gen[(IssueTransaction, SponsorFeeTransactionV1, SponsorFeeTransactionV1, SponsorFeeTransactionV1, SponsorFeeTransactionV2)] =
    for {
      (_, assetName, description, quantity, decimals, reissuable, iFee, timestamp) <- issueParamGen
      txTimestamp = ts.getOrElse(timestamp)
      issue = IssueTransactionV2
        .selfSigned(chainId = currentChainId, sender, assetName, description, quantity, decimals, reissuable = reissuable, iFee, txTimestamp, None)
        .right
        .get
      assetId = issue.assetId()
    } yield (issue,
             SponsorFeeTransactionV1.selfSigned(sender, assetId, true, 1.west, txTimestamp).explicitGet(),
             SponsorFeeTransactionV1.selfSigned(sender, assetId, true, 1.west, txTimestamp + 1).explicitGet(),
             SponsorFeeTransactionV1.selfSigned(sender, assetId, false, 1.west, txTimestamp + 2).explicitGet(),
             SponsorFeeTransactionV2.selfSigned(sender, assetId, true, 1.west, txTimestamp + 2, atomicBadge).explicitGet())

  val sponsorFeeGen = for {
    sender           <- accountGen
    (_, tx, _, _, _) <- sponsorFeeCancelSponsorFeeGen(sender)
  } yield {
    tx
  }

  val sponsorFeeV2Gen = for {
    sender           <- accountGen
    atomicBadge      <- atomicBadgeOptGen
    (_, _, _, _, tx) <- sponsorFeeCancelSponsorFeeGen(sender, atomicBadge = atomicBadge)
  } yield {
    tx
  }

  val priceGen: Gen[Long]            = Gen.choose(1, 3 * 100000L * 100000000L)
  val matcherAmountGen: Gen[Long]    = Gen.choose(1, 3 * 100000L * 100000000L)
  val matcherFeeAmountGen: Gen[Long] = Gen.choose(1, 3 * 100000L * 100000000L)

  val orderTypeGen: Gen[OrderType] = Gen.oneOf(OrderType.BUY, OrderType.SELL)

  val orderParamGen = for {
    sender     <- accountGen
    matcher    <- accountGen
    pair       <- assetPairGen
    orderType  <- orderTypeGen
    amount     <- matcherAmountGen
    price      <- priceGen
    timestamp  <- timestampGen
    expiration <- maxOrderTimeGen
    matcherFee <- matcherFeeAmountGen
  } yield (sender, matcher, pair, orderType, amount, price, timestamp, expiration, matcherFee)

  val exchangeTransactionGen: Gen[ExchangeTransaction] = for {
    sender1: PrivateKeyAccount <- accountGen
    sender2: PrivateKeyAccount <- accountGen
    assetPair                  <- assetPairGen
    r                          <- exchangeV2GeneratorP(sender1, sender2, assetPair.amountAsset, assetPair.priceAsset)
  } yield r

  protected type OrderConstructor = (PrivateKeyAccount, PublicKeyAccount, AssetPair, Long, Long, Long, Long, Long) => Order

  def exchangeV2GeneratorP(buyer: PrivateKeyAccount,
                           seller: PrivateKeyAccount,
                           amountAssetId: Option[ByteStr],
                           priceAssetId: Option[ByteStr],
                           fixedMatcherFee: Option[Long] = None,
                           orderVersions: Set[Byte] = Set(1, 2)): Gen[ExchangeTransactionV2] = {
    def mkBuyOrder(version: Byte): OrderConstructor  = if (version == 1) OrderV1.buy else OrderV2.buy
    def mkSellOrder(version: Byte): OrderConstructor = if (version == 1) OrderV1.sell else OrderV2.sell

    for {
      (_, matcher, _, _, price, amount1, timestamp, expiration, genMatcherFee) <- orderParamGen
      amount2: Long                                                            <- matcherAmountGen
      matcherFee = fixedMatcherFee.getOrElse(genMatcherFee)
      matchedAmount: Long <- Gen.choose(Math.min(amount1, amount2) / 2000, Math.min(amount1, amount2) / 1000)
      assetPair = AssetPair(amountAssetId, priceAssetId)
      mkO1 <- Gen.oneOf(orderVersions.map(mkBuyOrder).toSeq)
      mkO2 <- Gen.oneOf(orderVersions.map(mkSellOrder).toSeq)
    } yield {
      val buyFee  = (BigInt(matcherFee) * BigInt(matchedAmount) / BigInt(amount1)).longValue()
      val sellFee = (BigInt(matcherFee) * BigInt(matchedAmount) / BigInt(amount2)).longValue()

      val o1 = mkO1(seller, matcher, assetPair, amount1, price, timestamp, expiration, matcherFee)
      val o2 = mkO2(seller, matcher, assetPair, amount2, price, timestamp, expiration, matcherFee)

      ExchangeTransactionV2
        .create(matcher, o1, o2, matchedAmount, price, buyFee, sellFee, (buyFee + sellFee) / 2, expiration - 100)
        .explicitGet()
    }
  }

  def atomicTxV1Gen(signer: PrivateKeyAccount, innerTxsGen: Gen[List[AtomicInnerTransaction]], tsGen: Gen[Long]): Gen[AtomicTransaction] = {
    for {
      txs <- innerTxsGen
      ts  <- tsGen
    } yield AtomicTransactionV1.selfSigned(signer, None, txs, ts).explicitGet()
  }

  def atomicTxV1Gen(signer: PrivateKeyAccount, innerTxGen: Gen[AtomicInnerTransaction]): Gen[AtomicTransaction] = {
    atomicTxV1Gen(signer, innerAtomicTxs2orMoreGen(innerTxGen), positiveLongGen)
  }

  def innerAtomicTxs2orMoreGen(txGen: Gen[AtomicInnerTransaction]): Gen[List[AtomicInnerTransaction]] =
    for {
      tx  <- txGen
      txs <- Gen.nonEmptyListOf(txGen)
    } yield tx :: txs

  def atomicTxV1MultipleInnerGen(signer: PrivateKeyAccount, innerTxsGen: Gen[List[AtomicInnerTransaction]]): Gen[AtomicTransaction] = {
    atomicTxV1Gen(signer, innerTxsGen, positiveLongGen)
  }

  import DataTransactionEntryOps.MaxKeySize

  val dataKeyGen = for {
    size <- Gen.choose[Byte](5, MaxKeySize)
  } yield Random.nextString(size)

  val dataScriptsKeyGen = for {
    size <- Gen.choose[Byte](1, 10)
  } yield Random.nextString(size)

  val dataAsciiKeyGen = for {
    size <- Gen.choose[Byte](5, MaxKeySize)
  } yield Random.alphanumeric.take(size).mkString

  val dataAsciiRussianGen = for {
    size <- Gen.choose[Byte](5, MaxKeySize)
    s    <- Gen.pick(size, List('!' to '~', 'а' to 'я').flatten)
  } yield s.mkString

  def longEntryGen(keyGen: Gen[String] = dataAsciiRussianGen) =
    for {
      key   <- keyGen
      value <- Gen.choose[Long](Long.MinValue, Long.MaxValue)
    } yield IntegerDataEntry(key, value)

  def booleanEntryGen(keyGen: Gen[String] = dataAsciiRussianGen) =
    for {
      key   <- keyGen
      value <- Gen.oneOf(true, false)
    } yield BooleanDataEntry(key, value)

  def binaryEntryGen(maxSize: Int, keyGen: Gen[String] = dataAsciiRussianGen) =
    for {
      key   <- keyGen
      size  <- Gen.choose(0, maxSize)
      value <- byteArrayGen(size)
    } yield BinaryDataEntry(key, ByteStr(value))

  def stringEntryGen(maxSize: Int, keyGen: Gen[String] = dataAsciiRussianGen) =
    for {
      key   <- keyGen
      size  <- Gen.choose(0, maxSize)
      value <- Gen.listOfN(size, aliasAlphabetGen)
    } yield StringDataEntry(key, value.mkString)

  def dataEntryGen(maxSize: Int, keyGen: Gen[String] = dataAsciiRussianGen) =
    Gen.oneOf(longEntryGen(keyGen), booleanEntryGen(keyGen), binaryEntryGen(maxSize, keyGen), stringEntryGen(maxSize, keyGen))

  val dataTransactionV1Gen: Gen[DataTransactionV1] = dataTransactionV1Gen(DataValidation.MaxEntryCount)
  val dataTransactionV2Gen: Gen[DataTransactionV2] = dataTransactionV2Gen(DataValidation.MaxEntryCount)
  val dataTransactionV3Gen: Gen[DataTransactionV3] = dataTransactionV3Gen(DataValidation.MaxEntryCount)

  def dataTransactionV1Gen(maxEntryCount: Int, useForScript: Boolean = false) =
    (for {
      (sender, uniq, timestamp) <- dataGen(maxEntryCount, useForScript)
    } yield DataTransactionV1.selfSigned(sender, sender, uniq, timestamp, 15000000).explicitGet())
      .label("DataTransactionV1")

  def dataTransactionV2Gen(maxEntryCount: Int, useForScript: Boolean = false) =
    (for {
      (sender, uniq, timestamp) <- dataGen(maxEntryCount, useForScript)
      feeAssetId                <- Gen.option(bytes32gen)
    } yield DataTransactionV2.selfSigned(sender, sender, uniq, timestamp, 15000000, feeAssetId.map(ByteStr(_))).explicitGet())
      .label("DataTransactionV2")

  def dataTransactionV3Gen(maxEntryCount: Int, useForScript: Boolean = false) =
    (for {
      (sender, uniq, timestamp) <- dataGen(maxEntryCount, useForScript)
      feeAssetId                <- Gen.option(bytes32gen)
      atomicBadge               <- atomicBadgeOptGen
    } yield DataTransactionV3.selfSigned(sender, sender, uniq, timestamp, 15000000, feeAssetId.map(ByteStr(_)), atomicBadge).explicitGet())
      .label("DataTransactionV3")

  protected def dataGen(maxEntryCount: Int, useForScript: Boolean) =
    for {
      sender    <- accountGen
      timestamp <- timestampGen
      size      <- Gen.choose(0, maxEntryCount)
      maxEntrySize = if (useForScript) 200 else (DataValidation.MaxBytes - 122) / (size max 1) min DataTransactionEntryOps.MaxValueSize
      data <- if (useForScript) Gen.listOfN(size, dataEntryGen(maxEntrySize, dataScriptsKeyGen)) else Gen.listOfN(size, dataEntryGen(maxEntrySize))
      uniq = data.foldRight(List.empty[DataEntry[_]]) { (e, es) =>
        if (es.exists(_.key == e.key)) es else e :: es
      }
    } yield (sender, uniq, timestamp)

  def smartIssueTransactionGen(senderGen: Gen[PrivateKeyAccount] = accountGen,
                               scriptOptGen: Gen[Option[Script]] = Gen.option(scriptGen)): Gen[IssueTransactionV2] =
    for {
      script                                                                      <- scriptOptGen
      (_, assetName, description, quantity, decimals, reissuable, fee, timestamp) <- issueParamGen
      sender                                                                      <- senderGen
    } yield IssueTransactionV2
      .selfSigned(currentChainId, sender, assetName, description, quantity, decimals, reissuable, fee, timestamp, script)
      .explicitGet()

  def smartIssueTransactionV3Gen(senderGen: Gen[PrivateKeyAccount] = accountGen,
                                 scriptOptGen: Gen[Option[Script]] = Gen.option(scriptGen)): Gen[IssueTransactionV3] =
    for {
      script                                                                      <- scriptOptGen
      (_, assetName, description, quantity, decimals, reissuable, fee, timestamp) <- issueParamGen
      sender                                                                      <- senderGen
      atomictBadgeOpt                                                             <- atomicBadgeOptGen
    } yield IssueTransactionV3
      .selfSigned(currentChainId, sender, assetName, description, quantity, decimals, reissuable, fee, timestamp, atomictBadgeOpt, script)
      .explicitGet()

  def permitTransactionV1Gen(accountGen: Gen[PrivateKeyAccount] = accountGen,
                             targetGen: Gen[AddressOrAlias] = accountOrAliasGen,
                             permissionOpGen: Gen[PermissionOp] = PermissionsGen.permissionOpGen,
                             timestampGen: Gen[Long] = timestampGen): Gen[PermitTransactionV1] = {
    for {
      sender       <- accountGen
      target       <- targetGen
      txTimestamp  <- timestampGen
      permissionOp <- permissionOpGen.map(_.copy(timestamp = txTimestamp))
      fee          <- smallFeeGen
      permitTx     <- PermitTransactionV1.selfSigned(sender, target, txTimestamp, fee, permissionOp).fold(_ => Gen.fail, Gen.const)
    } yield permitTx
  }

  def permitTransactionV2Gen(atomicBadgeGen: Gen[Option[AtomicBadge]] = atomicBadgeOptGen,
                             accountGen: Gen[PrivateKeyAccount] = accountGen,
                             targetGen: Gen[AddressOrAlias] = accountOrAliasGen,
                             permissionOpGen: Gen[PermissionOp] = PermissionsGen.permissionOpGen,
                             timestampGen: Gen[Long] = timestampGen): Gen[PermitTransactionV2] = {
    for {
      atomicBadge  <- atomicBadgeGen
      sender       <- accountGen
      target       <- targetGen
      txTimestamp  <- timestampGen
      permissionOp <- permissionOpGen.map(_.copy(timestamp = txTimestamp))
      fee          <- smallFeeGen
      permitTx     <- PermitTransactionV2.selfSigned(sender, target, txTimestamp, fee, permissionOp, atomicBadge).fold(_ => Gen.fail, Gen.const)
    } yield permitTx
  }

  def genesisPermitTxGen: Gen[GenesisPermitTransaction] =
    for {
      target    <- addressGen
      role      <- PermissionsGen.roleGen
      timestamp <- timestampGen
      tx        <- GenesisPermitTransaction.create(target, role, timestamp).fold(_ => Gen.fail, Gen.const)
    } yield tx

  def genesisPermitTxGen(target: Address, role: Role, timestamp: Long): Gen[GenesisPermitTransaction] = {
    GenesisPermitTransaction.create(target, role, timestamp).fold(_ => Gen.fail, Gen.const)
  }

  def registerNodeTransactionGen(opTypeGen: Gen[OpType] = PermissionsGen.permissionOpTypeGen): Gen[RegisterNodeTransactionV1] = {
    registerNodeTransactionGen(accountGen, accountGen, opTypeGen)
  }

  def registerNodeTransactionGen(senderGen: Gen[PrivateKeyAccount],
                                 targetGen: Gen[PrivateKeyAccount],
                                 opTypeGen: Gen[OpType]): Gen[RegisterNodeTransactionV1] = {
    for {
      sender      <- senderGen
      target      <- targetGen
      opType      <- opTypeGen
      fee         <- smallFeeGen
      txTimestamp <- ntpTimestampGen
      nodeName    <- genBoundedString(5, 20)
      registerNodeTx <- RegisterNodeTransactionV1
        .selfSigned(sender, target, Some(new String(nodeName, UTF_8)), opType, txTimestamp, fee)
        .fold(_ => Gen.fail, Gen.const)
    } yield registerNodeTx
  }

  def registerNodeTransactionV2Gen(opTypeGen: Gen[OpType] = PermissionsGen.permissionOpTypeGen): Gen[RegisterNodeTransactionV2] = {
    registerNodeTransactionV2Gen(accountGen, accountGen, opTypeGen)
  }

  def registerNodeTransactionV2Gen(senderGen: Gen[PrivateKeyAccount],
                                   targetGen: Gen[PrivateKeyAccount],
                                   opTypeGen: Gen[OpType]): Gen[RegisterNodeTransactionV2] = {
    for {
      sender      <- senderGen
      target      <- targetGen
      opType      <- opTypeGen
      fee         <- smallFeeGen
      txTimestamp <- ntpTimestampGen
      nodeName    <- genBoundedString(5, 20)
      atomicBadge <- atomicBadgeOptGen
      registerNodeTx <- RegisterNodeTransactionV2
        .selfSigned(sender, target, Some(new String(nodeName, UTF_8)), opType, txTimestamp, fee, atomicBadge)
        .fold(_ => Gen.fail, Gen.const)
    } yield registerNodeTx
  }

  def genesisRegisterNodeTxGen(timestampGen: Gen[Long] = timestampGen): Gen[GenesisRegisterNodeTransaction] = {
    for {
      targetPubkey <- accountGen
      timestamp    <- timestampGen
      tx           <- GenesisRegisterNodeTransaction.create(targetPubkey, timestamp).fold(_ => Gen.fail, Gen.const)
    } yield tx
  }

  case class CreatePolicyTransactionV1TestWrap(tx: CreatePolicyTransactionV1, policyOwner: PrivateKeyAccount)
  case class CreatePolicyTransactionV2TestWrap(tx: CreatePolicyTransactionV2, policyOwner: PrivateKeyAccount)
  case class CreatePolicyTransactionV3TestWrap(tx: CreatePolicyTransactionV3, policyOwner: PrivateKeyAccount)
  case class CreatePolicyTransactionV1WithRecipientsPrivKey(txWrap: CreatePolicyTransactionV1TestWrap, recipientsPriKey: List[PrivateKeyAccount])
  case class CreatePolicyTransactionV2WithRecipientsPrivKey(txWrap: CreatePolicyTransactionV2TestWrap, recipientsPriKey: List[PrivateKeyAccount])
  case class CreatePolicyTransactionV3WithRecipientsPrivKey(txWrap: CreatePolicyTransactionV3TestWrap, recipientsPriKey: List[PrivateKeyAccount])

  def createPolicyTransactionV1Gen(recipientsMaxSize: Int = 30, ownersMaxSize: Int = 30): Gen[CreatePolicyTransactionV1TestWrap] = {
    createPolicyTransactionV1GenWithRecipients(recipientsMaxSize, ownersMaxSize).map(_.txWrap)
  }

  def createPolicyTransactionV2Gen(recipientsMaxSize: Int = 30, ownersMaxSize: Int = 30): Gen[CreatePolicyTransactionV2TestWrap] = {
    createPolicyTransactionV2GenWithRecipients(recipientsMaxSize, ownersMaxSize).map(_.txWrap)
  }

  def createPolicyTransactionV3Gen(recipientsMaxSize: Int = 30,
                                   ownersMaxSize: Int = 30,
                                   atomicBadgeGen: Gen[Option[AtomicBadge]] = atomicBadgeOptGen): Gen[CreatePolicyTransactionV3TestWrap] = {
    createPolicyTransactionV3GenWithRecipients(recipientsMaxSize, ownersMaxSize, atomicBadgeGen).map(_.txWrap)
  }

  def createPolicyTransactionV1GenWithRecipients(
      recipientsMaxSize: Int = 30,
      ownersMaxSize: Int = 30,
      senderGen: Gen[PrivateKeyAccount] = accountGen): Gen[CreatePolicyTransactionV1WithRecipientsPrivKey] = {
    for {
      sender      <- senderGen
      policyName  <- genBoundedString(10, PolicyValidation.MaxPolicyNameLength)
      description <- genBoundedString(10, 100)
      recipients  <- severalGenerators(accountGen, 1, recipientsMaxSize)
      owners      <- severalAddressGenerator(1, ownersMaxSize)
      txTimestamp <- ntpTimestampGen
      fee = TestFees.defaultFees.forTxType(CreatePolicyTransaction.typeId)
      createPolicyTx <- CreatePolicyTransactionV1
        .selfSigned(
          sender,
          new String(policyName, UTF_8),
          new String(description, UTF_8),
          recipients.map(_.toAddress),
          sender.toAddress :: owners,
          txTimestamp,
          fee
        )
        .fold(_ => Gen.fail, Gen.const)
    } yield CreatePolicyTransactionV1WithRecipientsPrivKey(CreatePolicyTransactionV1TestWrap(createPolicyTx, sender), recipients)
  }

  def createPolicyTransactionV2GenWithRecipients(recipientsMaxSize: Int = 30,
                                                 ownersMaxSize: Int = 30): Gen[CreatePolicyTransactionV2WithRecipientsPrivKey] = {
    for {
      sender      <- accountGen
      policyName  <- genBoundedString(10, PolicyValidation.MaxPolicyNameLength)
      description <- genBoundedString(10, 100)
      recipients  <- severalGenerators(accountGen, 0, recipientsMaxSize)
      owners      <- severalAddressGenerator(1, ownersMaxSize)
      feeAssetId  <- Gen.option(bytes32gen)
      txTimestamp <- ntpTimestampGen
      fee = TestFees.defaultFees.forTxType(CreatePolicyTransaction.typeId)
      createPolicyTx <- CreatePolicyTransactionV2
        .selfSigned(
          sender,
          new String(policyName, UTF_8),
          new String(description, UTF_8),
          recipients.map(_.toAddress),
          sender.toAddress :: owners,
          txTimestamp,
          fee,
          feeAssetId.map(ByteStr(_))
        )
        .fold(_ => Gen.fail, Gen.const)
    } yield CreatePolicyTransactionV2WithRecipientsPrivKey(CreatePolicyTransactionV2TestWrap(createPolicyTx, sender), recipients)
  }

  def createPolicyTransactionV3GenWithRecipients(
      recipientsMaxSize: Int = 30,
      ownersMaxSize: Int = 30,
      atomicBadgeGen: Gen[Option[AtomicBadge]] = atomicBadgeOptGen): Gen[CreatePolicyTransactionV3WithRecipientsPrivKey] = {
    for {
      sender      <- accountGen
      policyName  <- genBoundedString(10, PolicyValidation.MaxPolicyNameLength)
      description <- genBoundedString(10, 100)
      recipients  <- severalGenerators(accountGen, 0, recipientsMaxSize)
      owners      <- severalAddressGenerator(1, ownersMaxSize)
      feeAssetId  <- Gen.option(bytes32gen)
      txTimestamp <- ntpTimestampGen
      fee = TestFees.defaultFees.forTxType(CreatePolicyTransaction.typeId)
      atomicBadge <- atomicBadgeGen
      createPolicyTx <- CreatePolicyTransactionV3
        .selfSigned(
          sender,
          new String(policyName, UTF_8),
          new String(description, UTF_8),
          recipients.map(_.toAddress),
          sender.toAddress :: owners,
          txTimestamp,
          fee,
          feeAssetId.map(ByteStr(_)),
          atomicBadge
        )
        .fold(_ => Gen.fail, Gen.const)
    } yield CreatePolicyTransactionV3WithRecipientsPrivKey(CreatePolicyTransactionV3TestWrap(createPolicyTx, sender), recipients)
  }

  protected def internalUpdatePolicyTransactionV1Gen(policyIdGen: Gen[Array[Byte]],
                                                     ownersToAdd: Gen[List[Address]],
                                                     ownersToRemove: Gen[List[Address]],
                                                     recipientsToAdd: Gen[List[Address]],
                                                     recipientsToRemove: Gen[List[Address]],
                                                     opTypeGen: Gen[OpType],
                                                     policyOwner: PrivateKeyAccount): Gen[UpdatePolicyTransactionV1] = {
    for {
      policyId <- policyIdGen
      opType   <- opTypeGen
      recipients <- opType match {
        case Add    => recipientsToAdd
        case Remove => recipientsToRemove
      }
      owners <- opType match {
        case Add    => ownersToAdd
        case Remove => ownersToRemove
      }
      txTimestamp <- ntpTimestampGen
      fee = TestFees.defaultFees.forTxType(UpdatePolicyTransactionV1.typeId)
      updatePolicyTx <- UpdatePolicyTransactionV1
        .selfSigned(
          policyOwner,
          ByteStr(policyId),
          recipients,
          owners,
          opType,
          txTimestamp,
          fee
        )
        .fold(_ => Gen.fail, Gen.const)
    } yield updatePolicyTx
  }

  protected def internalUpdatePolicyTransactionV2Gen(policyIdGen: Gen[Array[Byte]],
                                                     ownersToAdd: Gen[List[Address]],
                                                     ownersToRemove: Gen[List[Address]],
                                                     recipientsGen: Gen[List[Address]],
                                                     opTypeGen: Gen[OpType],
                                                     feeAssetIdGen: Gen[Option[Array[Byte]]],
                                                     policyOwner: PrivateKeyAccount): Gen[UpdatePolicyTransactionV2] = {
    for {
      policyId   <- policyIdGen
      recipients <- recipientsGen
      opType     <- opTypeGen
      owners <- opType match {
        case Add    => ownersToAdd
        case Remove => ownersToRemove
      }
      txTimestamp <- ntpTimestampGen
      feeAssetId  <- feeAssetIdGen
      fee = TestFees.defaultFees.forTxType(UpdatePolicyTransactionV1.typeId)
      updatePolicyTx <- UpdatePolicyTransactionV2
        .selfSigned(
          policyOwner,
          ByteStr(policyId),
          recipients,
          owners,
          opType,
          txTimestamp,
          fee,
          feeAssetId.map(ByteStr(_))
        )
        .fold(_ => Gen.fail, Gen.const)
    } yield updatePolicyTx
  }

  protected def internalUpdatePolicyTransactionV3Gen(atomicBadgeGen: Gen[Option[AtomicBadge]],
                                                     policyIdGen: Gen[Array[Byte]],
                                                     ownersToAdd: Gen[List[Address]],
                                                     ownersToRemove: Gen[List[Address]],
                                                     recipientsGen: Gen[List[Address]],
                                                     opTypeGen: Gen[OpType],
                                                     feeAssetIdGen: Gen[Option[Array[Byte]]],
                                                     policyOwner: PrivateKeyAccount): Gen[UpdatePolicyTransactionV3] = {
    for {
      policyId   <- policyIdGen
      recipients <- recipientsGen
      opType     <- opTypeGen
      owners <- opType match {
        case Add    => ownersToAdd
        case Remove => ownersToRemove
      }
      txTimestamp <- ntpTimestampGen
      feeAssetId  <- feeAssetIdGen
      fee = TestFees.defaultFees.forTxType(UpdatePolicyTransactionV1.typeId)
      atomicBadge <- atomicBadgeGen
      updatePolicyTx <- UpdatePolicyTransactionV3
        .selfSigned(
          policyOwner,
          ByteStr(policyId),
          recipients,
          owners,
          opType,
          txTimestamp,
          fee,
          feeAssetId.map(ByteStr(_)),
          atomicBadge
        )
        .fold(_ => Gen.fail, Gen.const)
    } yield updatePolicyTx
  }

  def updatePolicyTransactionV1Gen(policyOwner: PrivateKeyAccount = accountGen.sample.get,
                                   policyIdGen: Gen[Array[Byte]] = genBoundedString(10, 100),
                                   recipientsMaxSize: Int = 30,
                                   ownersMaxSize: Int = 30): Gen[UpdatePolicyTransactionV1] = {
    internalUpdatePolicyTransactionV1Gen(
      policyIdGen,
      severalAddressGenerator(1, ownersMaxSize),
      Gen.const(List(policyOwner.toAddress)),
      severalAddressGenerator(0, recipientsMaxSize),
      severalAddressGenerator(0, recipientsMaxSize),
      PermissionsGen.permissionOpTypeGen,
      policyOwner
    )
  }

  def updatePolicyTransactionV2Gen(policyOwner: PrivateKeyAccount = accountGen.sample.get,
                                   policyIdGen: Gen[Array[Byte]] = genBoundedString(10, 100),
                                   recipientsMaxSize: Int = 30,
                                   ownersMaxSize: Int = 30,
                                   feeAssetIdGen: Gen[Option[Array[Byte]]] = Gen.option(bytes32gen)): Gen[UpdatePolicyTransactionV2] = {
    internalUpdatePolicyTransactionV2Gen(
      policyIdGen,
      severalAddressGenerator(1, ownersMaxSize),
      Gen.const(List(policyOwner.toAddress)),
      severalAddressGenerator(0, recipientsMaxSize),
      PermissionsGen.permissionOpTypeGen,
      feeAssetIdGen,
      policyOwner
    )
  }

  def updatePolicyTransactionV3Gen(atomicBadgeGen: Gen[Option[AtomicBadge]] = atomicBadgeOptGen,
                                   policyOwner: PrivateKeyAccount = accountGen.sample.get,
                                   policyIdGen: Gen[Array[Byte]] = genBoundedString(10, 100),
                                   recipientsMaxSize: Int = 30,
                                   ownersMaxSize: Int = 30,
                                   feeAssetIdGen: Gen[Option[Array[Byte]]] = Gen.option(bytes32gen)): Gen[UpdatePolicyTransactionV3] = {
    internalUpdatePolicyTransactionV3Gen(
      atomicBadgeGen,
      policyIdGen,
      severalAddressGenerator(1, ownersMaxSize),
      Gen.const(List(policyOwner.toAddress)),
      severalAddressGenerator(0, recipientsMaxSize),
      PermissionsGen.permissionOpTypeGen,
      feeAssetIdGen,
      policyOwner
    )
  }

  case class PolicyDataWithTxV1(data: Array[Byte], tx: PolicyDataHashTransactionV1)
  case class PolicyDataWithTxV2(data: Array[Byte], tx: PolicyDataHashTransactionV2)
  case class PolicyDataWithTxV3(data: Array[Byte], tx: PolicyDataHashTransactionV3)

  def policyDataHashTransactionV1Gen(policyIdGen: Gen[Array[Byte]] = genBoundedString(10, 100),
                                     senderGen: Gen[PrivateKeyAccount] = accountGen): Gen[PolicyDataWithTxV1] = {
    for {
      sender           <- senderGen
      policyId         <- policyIdGen
      (data, dataHash) <- policyDataHashGen
      txTimestamp      <- ntpTimestampGen
      fee = TestFees.defaultFees.forTxType(PolicyDataHashTransactionV1.typeId)
      policyDataHashTx <- PolicyDataHashTransactionV1
        .selfSigned(sender, dataHash, ByteStr(policyId), txTimestamp, fee)
        .fold(_ => Gen.fail, Gen.const)
    } yield PolicyDataWithTxV1(data, policyDataHashTx)
  }

  def policyDataHashTransactionV2Gen(policyIdGen: Gen[Array[Byte]] = genBoundedString(10, 100),
                                     feeAssetIdGen: Gen[Option[Array[Byte]]] = Gen.option(bytes32gen)): Gen[PolicyDataWithTxV2] = {
    for {
      sender           <- accountGen
      policyId         <- policyIdGen
      (data, dataHash) <- policyDataHashGen
      txTimestamp      <- ntpTimestampGen
      feeAssetId       <- feeAssetIdGen
      fee = TestFees.defaultFees.forTxType(PolicyDataHashTransactionV2.typeId)
      policyDataHashTx <- PolicyDataHashTransactionV2
        .selfSigned(sender, dataHash, ByteStr(policyId), txTimestamp, fee, feeAssetId.map(ByteStr(_)))
        .fold(_ => Gen.fail, Gen.const)
    } yield PolicyDataWithTxV2(data, policyDataHashTx)
  }

  def policyDataHashTransactionV3Gen(policyIdGen: Gen[Array[Byte]] = genBoundedString(10, 100),
                                     feeAssetIdGen: Gen[Option[Array[Byte]]] = Gen.option(bytes32gen),
                                     atomicBadgeGen: Gen[Option[AtomicBadge]] = atomicBadgeOptGen): Gen[PolicyDataWithTxV3] = {
    for {
      sender           <- accountGen
      policyId         <- policyIdGen
      (data, dataHash) <- policyDataHashGen
      txTimestamp      <- ntpTimestampGen
      feeAssetId       <- feeAssetIdGen
      atomicBadge      <- atomicBadgeGen
      fee = TestFees.defaultFees.forTxType(PolicyDataHashTransactionV2.typeId)
      policyDataHashTx <- PolicyDataHashTransactionV3
        .selfSigned(sender, dataHash, ByteStr(policyId), txTimestamp, fee, feeAssetId.map(ByteStr(_)), atomicBadge)
        .fold(_ => Gen.fail, Gen.const)
    } yield PolicyDataWithTxV3(data, policyDataHashTx)
  }
}
