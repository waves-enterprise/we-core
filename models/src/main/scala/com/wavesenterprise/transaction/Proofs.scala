package com.wavesenterprise.transaction

import com.google.common.io.ByteArrayDataOutput
import com.wavesenterprise.serialization.Deser
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.transaction.ValidationError.GenericError
import com.wavesenterprise.utils.Base58
import com.wavesenterprise.utils.Constants.base58Length
import com.wavesenterprise.utils.EitherUtils.EitherExt
import monix.eval.Coeval

import scala.util.Try

case class Proofs(proofs: List[ByteStr]) {
  val bytes: Coeval[Array[Byte]]  = Coeval.evalOnce(Proofs.Version +: Deser.serializeArrays(proofs.map(_.arr)))
  val base58: Coeval[Seq[String]] = Coeval.evalOnce(proofs.map(p => Base58.encode(p.arr)))

  def writeBytes(output: ByteArrayDataOutput): Unit = {
    output.writeByte(Proofs.Version)
    Deser.writeArrays(proofs.map(_.arr), output)
  }
}

object Proofs {

  def apply(proofs: Seq[AssetId]): Proofs = new Proofs(proofs.toList)

  val Version            = 1: Byte
  val MaxProofs          = 8
  val MaxProofSize       = 64
  val MaxProofStringSize = base58Length(MaxProofSize)

  lazy val empty = create(List.empty).explicitGet()

  protected def validate(proofs: Seq[ByteStr]): Either[ValidationError, Unit] = {
    for {
      _ <- Either.cond(proofs.lengthCompare(MaxProofs) <= 0, (), GenericError(s"Too many proofs, max $MaxProofs proofs"))
      _ <- Either.cond(!proofs.map(_.arr.length).exists(_ > MaxProofSize), (), GenericError(s"Too large proof, must be max $MaxProofSize bytes"))
    } yield ()
  }

  def createWithBytes(proofs: Seq[ByteStr], parsedBytes: Array[Byte], offset: Int): Either[ValidationError, Proofs] =
    validate(proofs) map { _ =>
      new Proofs(proofs.toList) {
        override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce {
          val proofsLength = 3 + proofs.map(_.arr.length + 2).sum
          parsedBytes.slice(offset, offset + proofsLength)
        }
      }
    }

  def create(proofs: Seq[ByteStr]): Either[ValidationError, Proofs] =
    validate(proofs).map(_ => Proofs(proofs.toList))

  def fromBytes(bytes: Array[Byte], offset: Int): Either[ValidationError, Proofs] =
    for {
      _ <- Either.cond(bytes.length - offset > 0, (), GenericError(s"Empty proofs"))
      proofsVersion = bytes(offset)
      _           <- Either.cond(proofsVersion == 1, (), GenericError(s"Proofs version must be 1, actual: $proofsVersion"))
      arrsWithEnd <- Try(Deser.parseArrays(bytes, offset + 1)).toEither.left.map(er => GenericError(er.toString))
      (arrs, _) = arrsWithEnd
      r <- createWithBytes(arrs.map(ByteStr(_)), bytes, offset)
    } yield r

  def fromBytesUnsafe(bytes: Array[Byte], offset: Int): (Proofs, Int) =
    (for {
      _ <- Either.cond(bytes.length - offset > 0, (), GenericError(s"Empty proofs"))
      proofsVersion = bytes(offset)
      _            <- Either.cond(proofsVersion == 1, (), GenericError(s"Proofs version must be 1, actual: $proofsVersion"))
      arraysAndEnd <- Try(Deser.parseArrays(bytes, offset + 1)).toEither.left.map(er => GenericError(er.toString))
      (arrays, end) = arraysAndEnd
      result <- createWithBytes(arrays.map(ByteStr(_)), bytes, offset)
    } yield {
      result -> end
    }).explicitGet()
}
