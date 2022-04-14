package com.wavesenterprise.transaction

import com.google.common.primitives.{Bytes, Longs}
import com.wavesenterprise.account.Address
import com.wavesenterprise.acl.Role
import com.wavesenterprise.crypto
import com.wavesenterprise.serialization.ProtoAdapter
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.TransactionParsers.{TimestampLength, TypeLength}
import com.wavesenterprise.transaction.protobuf.{GenesisPermitTransaction => PbGenesisPermitTransaction, Transaction => PbTransaction}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.{Failure, Success, Try}

case class GenesisPermitTransaction(target: Address, role: Role, timestamp: Long, signature: ByteStr)
    extends Transaction
    with ProtoSerializableTransaction {
  import GenesisPermitTransaction._

  override val builder: TransactionParser = GenesisPermitTransaction
  override val fee: Long                  = 0
  override val id: Coeval[AssetId]        = Coeval.evalOnce(signature)

  override val json: Coeval[JsObject] = Coeval.evalOnce(
    Json.obj(
      "type"      -> builder.typeId,
      "id"        -> id().base58,
      "fee"       -> 0,
      "timestamp" -> timestamp,
      "signature" -> this.signature.base58,
      "target"    -> target.stringRepr,
      "role"      -> role.prefixS
    ))

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    val typeBytes      = Array(builder.typeId)
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TimestampLength, 0)
    val targetBytes    = target.bytes.arr
    require(targetBytes.length == Address.AddressLength)
    val roleBytes = Array(role.byte)

    val result = Bytes.concat(typeBytes, targetBytes, roleBytes, timestampBytes)
    require(result.length == TypeLength + BASE_LENGTH)
    result
  }
  override val bodyBytes: Coeval[Array[Byte]] = bytes

  override def toProto: PbTransaction = {
    val innerTx = PbTransaction.Transaction.GenesisPermitTransaction(
      PbGenesisPermitTransaction(
        ProtoAdapter.byteArrayToByteString(id().arr),
        ProtoAdapter.byteArrayToByteString(target.bytes.arr),
        Some(ProtoAdapter.toProto(role)),
        fee,
        timestamp,
        ProtoAdapter.byteArrayToByteString(signature.arr)
      )
    )
    PbTransaction(1, innerTx)
  }
}

object GenesisPermitTransaction extends TransactionParserFor[GenesisPermitTransaction] with TransactionParser.HardcodedVersion1 {
  override val typeId: Byte = 101

  private val BASE_LENGTH = Address.AddressLength + 1 + TimestampLength

  def generateSignature(target: Address, role: Role, timestamp: Long): Array[Byte] = {
    val typeBytes      = Array(typeId)
    val targetBytes    = target.bytes.arr
    val timestampBytes = Longs.toByteArray(timestamp)
    val roleBytes      = Array(role.byte)

    val data = Bytes.concat(typeBytes, targetBytes, timestampBytes, roleBytes)

    val h = crypto.fastHash(data)
    Bytes.concat(h, h)
  }

  override def parseTail(version: Byte, bytes: Array[Byte], offset: Int): Try[GenesisPermitTransaction] =
    (for {
      _ <- Either.cond(bytes.length - offset >= BASE_LENGTH, (), ValidationError.GenericError("Data does not match base length"))
      targetBytes = bytes.slice(offset, offset + Address.AddressLength)
      target <- Address.fromBytes(targetBytes)
      bytesPastTarget = bytes.drop(offset + Address.AddressLength)
      roleByte <- bytesPastTarget.headOption.toRight(ValidationError.GenericError("Role byte is missing"))
      role     <- Role.fromByte(roleByte)
      bytesPastRole = bytesPastTarget.tail
      timestamp     = Longs.fromByteArray(bytesPastRole.take(Longs.BYTES))
      signature     = generateSignature(target, role, timestamp)
    } yield GenesisPermitTransaction(target, role, timestamp, ByteStr(signature)))
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))

  def create(target: Address, role: Role, timestamp: Long): Either[ValidationError, GenesisPermitTransaction] = {
    val signature = generateSignature(target, role, timestamp)
    Right(GenesisPermitTransaction(target, role, timestamp, ByteStr(signature)))
  }

}
