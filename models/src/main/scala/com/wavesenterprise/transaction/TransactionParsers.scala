package com.wavesenterprise.transaction

import com.wavesenterprise.crypto
import com.wavesenterprise.transaction.acl.{PermitTransactionV1, PermitTransactionV2}
import com.wavesenterprise.transaction.assets._
import com.wavesenterprise.transaction.assets.exchange.ExchangeTransactionV2
import com.wavesenterprise.transaction.docker._
import com.wavesenterprise.transaction.lease.{LeaseCancelTransactionV2, LeaseCancelTransactionV3, LeaseTransactionV2, LeaseTransactionV3}
import com.wavesenterprise.transaction.smart.SetScriptTransactionV1
import com.wavesenterprise.transaction.transfer._
import com.wavesenterprise.utils.Constants.base58Length

import scala.util.{Failure, Success, Try}

object TransactionParsers {

  val TimestampLength            = 8
  val AmountLength               = 8
  val TypeLength                 = 1
  val SignatureStringLength: Int = base58Length(crypto.SignatureLength)
  val ModernTxFlag               = 0

  private val old: Map[Byte, TransactionParser] = Seq[TransactionParser](
    GenesisTransaction,
    MassTransferTransactionV1,
    GenesisPermitTransaction,
    GenesisRegisterNodeTransaction
  ).map { x =>
    x.typeId -> x
  }(collection.breakOut)

  private val modern: Map[(Byte, Byte), TransactionParser] = Seq[TransactionParser](
    DataTransactionV1,
    DataTransactionV2,
    DataTransactionV3,
    TransferTransactionV2,
    TransferTransactionV3,
    SetScriptTransactionV1,
    IssueTransactionV2,
    IssueTransactionV3,
    CreateAliasTransactionV2,
    CreateAliasTransactionV3,
    CreateAliasTransactionV4,
    ReissueTransactionV2,
    ReissueTransactionV3,
    BurnTransactionV2,
    BurnTransactionV3,
    ExchangeTransactionV2,
    LeaseTransactionV2,
    LeaseTransactionV3,
    LeaseCancelTransactionV2,
    LeaseCancelTransactionV3,
    SponsorFeeTransactionV1,
    SponsorFeeTransactionV2,
    SetAssetScriptTransactionV1,
    PermitTransactionV1,
    PermitTransactionV2,
    RegisterNodeTransactionV1,
    RegisterNodeTransactionV2,
    CreateContractTransactionV1,
    CreateContractTransactionV2,
    CreateContractTransactionV3,
    CreateContractTransactionV4,
    CreateContractTransactionV5,
    CallContractTransactionV1,
    CallContractTransactionV2,
    CallContractTransactionV3,
    CallContractTransactionV4,
    CallContractTransactionV5,
    ExecutedContractTransactionV1,
    ExecutedContractTransactionV2,
    ExecutedContractTransactionV3,
    DisableContractTransactionV1,
    DisableContractTransactionV2,
    DisableContractTransactionV3,
    UpdateContractTransactionV1,
    UpdateContractTransactionV2,
    UpdateContractTransactionV3,
    UpdateContractTransactionV4,
    CreatePolicyTransactionV1,
    CreatePolicyTransactionV2,
    CreatePolicyTransactionV3,
    UpdatePolicyTransactionV1,
    UpdatePolicyTransactionV2,
    UpdatePolicyTransactionV3,
    PolicyDataHashTransactionV1,
    PolicyDataHashTransactionV2,
    PolicyDataHashTransactionV3,
    MassTransferTransactionV2,
    MassTransferTransactionV3,
    AtomicTransactionV1
  ).flatMap { x =>
    x.supportedVersions.map { version =>
      ((x.typeId, version), x)
    }
  }(collection.breakOut)

  val all: Map[(Byte, Byte), TransactionParser] = old.flatMap {
    case (typeId, builder) =>
      builder.supportedVersions.map { version =>
        ((typeId, version), builder)
      }
  } ++ modern

  val byName: Map[String, TransactionParser] = (old ++ modern).map {
    case (_, builder) => builder.classTag.runtimeClass.getSimpleName -> builder
  }

  def by(name: String): Option[TransactionParser]                = byName.get(name)
  def by(typeId: Byte, version: Byte): Option[TransactionParser] = all.get((typeId, version))

  def parseBytes(data: Array[Byte]): Try[Transaction] =
    data.headOption
      .fold[Try[Byte]](Failure(new IllegalArgumentException("Can't find the significant byte: the buffer is empty")))(Success(_))
      .flatMap { headByte =>
        if (headByte == 0) modernParseBytes(data)
        else oldParseBytes(headByte, data)
      }

  private def oldParseBytes(tpe: Byte, data: Array[Byte]): Try[Transaction] =
    old
      .get(tpe)
      .fold[Try[TransactionParser]](Failure(new IllegalArgumentException(s"Unknown transaction type (old encoding): '$tpe'")))(Success(_))
      .flatMap(_.parseBytes(data))

  private def modernParseBytes(data: Array[Byte]): Try[Transaction] = {
    if (data.length < 2)
      Failure(new IllegalArgumentException(s"Can't determine the type and the version of transaction: the buffer has ${data.length} bytes"))
    else {
      val Array(_, typeId, version) = data.take(3)
      modern
        .get((typeId, version))
        .fold[Try[TransactionParser]](
          Failure(new IllegalArgumentException(s"Unknown transaction type ($typeId) and version ($version) (modern encoding)")))(Success(_))
        .flatMap(_.parseBytes(data))
    }
  }

}
