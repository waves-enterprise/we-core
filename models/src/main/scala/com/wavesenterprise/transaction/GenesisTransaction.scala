package com.wavesenterprise.transaction

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.wavesenterprise.account.Address
import com.wavesenterprise.crypto
import com.wavesenterprise.serialization.ProtoAdapter
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.TransactionParsers._
import com.wavesenterprise.transaction.protobuf.{GenesisTransaction => PbGenesisTransaction, Transaction => PbTransaction}
import com.wavesenterprise.utils.EitherUtils.EitherExt
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.{Failure, Success, Try}

case class GenesisTransaction private (recipient: Address, amount: Long, timestamp: Long, signature: ByteStr)
    extends Transaction
    with ProtoSerializableTransaction {

  import GenesisTransaction._

  override val builder: TransactionParser = GenesisTransaction
  override val fee: Long                  = 0
  override val id: Coeval[AssetId]        = Coeval.evalOnce(signature)

  override val json: Coeval[JsObject] = Coeval.evalOnce(
    Json.obj(
      "type"      -> builder.typeId,
      "id"        -> id().base58,
      "fee"       -> 0,
      "timestamp" -> timestamp,
      "signature" -> this.signature.base58,
      "recipient" -> recipient.address,
      "amount"    -> amount
    ))

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    val typeBytes      = Array(builder.typeId)
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TimestampLength, 0)
    val amountBytes    = Bytes.ensureCapacity(Longs.toByteArray(amount), AmountLength, 0)
    val rcpBytes       = recipient.bytes.arr
    require(rcpBytes.length == Address.AddressLength)
    val res = Bytes.concat(typeBytes, timestampBytes, rcpBytes, amountBytes)
    require(res.length == TypeLength + BASE_LENGTH)
    res
  }
  override val bodyBytes: Coeval[Array[Byte]] = bytes

  override def toProto: PbTransaction = {
    val innerTx = PbTransaction.Transaction.GenesisTransaction(
      PbGenesisTransaction(
        ProtoAdapter.byteArrayToByteString(id().arr),
        ProtoAdapter.byteArrayToByteString(recipient.bytes.arr),
        amount,
        fee,
        timestamp,
        ProtoAdapter.byteArrayToByteString(signature.arr)
      )
    )
    PbTransaction(1, innerTx)
  }
}

object GenesisTransaction extends TransactionParserFor[GenesisTransaction] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = 1

  private val RECIPIENT_LENGTH = Address.AddressLength
  private val BASE_LENGTH      = TimestampLength + RECIPIENT_LENGTH + AmountLength

  def generateSignature(recipient: Address, amount: Long, timestamp: Long): Array[Byte] = {
    val typeBytes      = Bytes.ensureCapacity(Ints.toByteArray(typeId), TypeLength, 0)
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TimestampLength, 0)
    val amountBytes    = Longs.toByteArray(amount)
    val amountFill     = new Array[Byte](AmountLength - amountBytes.length)

    val data = Bytes.concat(typeBytes, timestampBytes, recipient.bytes.arr, Bytes.concat(amountFill, amountBytes))

    val h = crypto.fastHash(data)
    Bytes.concat(h, h)
  }

  override protected def parseTail(version: Byte, bytes: Array[Byte], offset: Int): Try[TransactionT] =
    Try {
      require(bytes.length - offset >= BASE_LENGTH, "Data does not match base length")

      var position = offset

      val timestampBytes = java.util.Arrays.copyOfRange(bytes, position, position + TimestampLength)
      val timestamp      = Longs.fromByteArray(timestampBytes)
      position += TimestampLength

      val recipientBytes = java.util.Arrays.copyOfRange(bytes, position, position + RECIPIENT_LENGTH)
      val recipient      = Address.fromBytes(recipientBytes).explicitGet()
      position += RECIPIENT_LENGTH

      val amountBytes = java.util.Arrays.copyOfRange(bytes, position, position + AmountLength)
      val amount      = Longs.fromByteArray(amountBytes)

      GenesisTransaction.create(recipient, amount, timestamp).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(recipient: Address, amount: Long, timestamp: Long): Either[ValidationError, GenesisTransaction] = {
    if (amount < 0) {
      Left(ValidationError.NegativeAmount(amount, "WEST"))
    } else {
      val signature = ByteStr(GenesisTransaction.generateSignature(recipient, amount, timestamp))
      Right(GenesisTransaction(recipient, amount, timestamp, signature))
    }
  }
}
