package com.wavesenterprise.transaction

import com.google.common.primitives.{Bytes, Longs}
import com.wavesenterprise.account.PublicKeyAccount
import com.wavesenterprise.crypto
import com.wavesenterprise.serialization.ProtoAdapter
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.TransactionParsers.{TimestampLength, TypeLength}
import com.wavesenterprise.transaction.protobuf.{GenesisRegisterNodeTransaction => PbGenesisRegisterNodeTransaction, Transaction => PbTransaction}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

case class GenesisRegisterNodeTransaction(targetPublicKey: PublicKeyAccount, timestamp: Long, signature: ByteStr)
    extends Transaction
    with ProtoSerializableTransaction {
  import GenesisRegisterNodeTransaction._

  override val builder: TransactionParser = GenesisRegisterNodeTransaction
  override val fee: Long                  = 0
  override val id: Coeval[AssetId]        = Coeval.evalOnce(signature)

  override val json: Coeval[JsObject] = Coeval.evalOnce(
    Json.obj(
      "type"            -> builder.typeId,
      "id"              -> id().base58,
      "fee"             -> fee,
      "timestamp"       -> timestamp,
      "signature"       -> this.signature.base58,
      "targetPublicKey" -> targetPublicKey.publicKeyBase58,
      "target"          -> targetPublicKey.address
    ))

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    val typeBytes      = Array(builder.typeId)
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TimestampLength, 0)
    val targetBytes    = targetPublicKey.publicKey.getEncoded
    require(targetBytes.length == crypto.KeyLength)

    val result = Bytes.concat(typeBytes, targetBytes, timestampBytes)
    require(result.length == TypeLength + BASE_LENGTH)
    result
  }

  override val bodyBytes: Coeval[Array[Byte]] = bytes

  override def toProto: PbTransaction = {
    val innerTx = PbTransaction.Transaction.GenesisRegisterNodeTransaction(
      PbGenesisRegisterNodeTransaction(
        ProtoAdapter.byteArrayToByteString(id().arr),
        ProtoAdapter.byteArrayToByteString(targetPublicKey.publicKey.getEncoded),
        fee,
        timestamp,
        ProtoAdapter.byteArrayToByteString(signature.arr)
      )
    )
    PbTransaction(1, innerTx)
  }
}

object GenesisRegisterNodeTransaction extends TransactionParserFor[GenesisRegisterNodeTransaction] with TransactionParser.HardcodedVersion1 {
  override val typeId: Byte = 110
  private val BASE_LENGTH   = crypto.KeyLength + TimestampLength

  def generateSignature(target: PublicKeyAccount, timestamp: Long): Array[Byte] = {
    val typeBytes      = Array(typeId)
    val targetBytes    = target.publicKey.getEncoded
    val timestampBytes = Longs.toByteArray(timestamp)

    val data = Bytes.concat(typeBytes, targetBytes, timestampBytes)

    val h = crypto.fastHash(data)
    Bytes.concat(h, h)
  }

  override def parseTail(version: Byte, bytes: Array[Byte], offset: Int): Try[GenesisRegisterNodeTransaction] = Try {
    require(bytes.length - offset >= BASE_LENGTH, s"Data does not match base length")
    val targetBytes     = bytes.slice(offset, offset + crypto.KeyLength)
    val target          = PublicKeyAccount(targetBytes)
    val bytesPastTarget = bytes.drop(offset + crypto.KeyLength)
    val timestamp       = Longs.fromByteArray(bytesPastTarget.take(Longs.BYTES))
    val signature       = generateSignature(target, timestamp)
    GenesisRegisterNodeTransaction(target, timestamp, ByteStr(signature))
  }

  def create(targetPubKey: PublicKeyAccount, timestamp: Long): Either[ValidationError, GenesisRegisterNodeTransaction] = {
    val signature = generateSignature(targetPubKey, timestamp)
    Right(GenesisRegisterNodeTransaction(targetPubKey, timestamp, ByteStr(signature)))
  }
}
