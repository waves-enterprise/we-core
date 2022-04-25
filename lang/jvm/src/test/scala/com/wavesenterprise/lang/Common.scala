package com.wavesenterprise.lang

import cats.data.EitherT
import com.wavesenterprise.lang.ScriptVersion.Versions.V1
import com.wavesenterprise.lang.v1.compiler.Terms._
import com.wavesenterprise.lang.v1.compiler.Types._
import com.wavesenterprise.lang.v1.evaluator.EvaluatorV1
import com.wavesenterprise.lang.v1.evaluator.ctx._
import com.wavesenterprise.lang.v1.evaluator.ctx.impl.{EnvironmentFunctions, PureContext, _}
import com.wavesenterprise.lang.v1.traits.domain.{Ord, Recipient, Tx}
import com.wavesenterprise.lang.v1.traits.{DataType, Environment}
import monix.eval.Coeval
import org.scalacheck.Shrink
import org.scalatest.matchers.{MatchResult, Matcher}
import shapeless.{:+:, CNil}

import scala.util.{Left, Right, Try}

object Common {
  import com.wavesenterprise.lang.v1.evaluator.ctx.impl.converters._

  val global = Global

  def ev[T <: EVALUATED](context: EvaluationContext = PureContext.build(V1).evaluationContext, expr: EXPR): Either[ExecutionError, T] =
    EvaluatorV1[T](context, expr)

  trait NoShrink {
    implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)
  }

  class ProduceError(errorMessage: String) extends Matcher[Either[_, _]] {
    override def apply(ei: Either[_, _]): MatchResult = {
      ei match {
        case r @ Right(_) => MatchResult(matches = false, "expecting Left(...{0}...) but got {1}", "got expected error", IndexedSeq(errorMessage, r))
        case l @ Left(_) =>
          MatchResult(matches = l.toString contains errorMessage,
                      "expecting Left(...{0}...) but got {1}",
                      "got expected error",
                      IndexedSeq(errorMessage, l))
      }
    }
  }

  def produce(errorMessage: String): ProduceError = new ProduceError(errorMessage)

  val multiplierFunction: NativeFunction =
    NativeFunction("MULTIPLY", 1, 10005, LONG, "test ultiplication", ("x1", LONG, "x1"), ("x2", LONG, "x2")) {
      case CONST_LONG(x1: Long) :: CONST_LONG(x2: Long) :: Nil => Try(x1 * x2).map(CONST_LONG).toEither.left.map(_.toString)
      case _                                                   => ??? // suppress pattern match warning
    }

  val pointTypeA = CaseType("PointA", List("X"  -> LONG, "YA" -> LONG))
  val pointTypeB = CaseType("PointB", List("X"  -> LONG, "YB" -> LONG))
  val pointTypeC = CaseType("PointC", List("YB" -> LONG))
  val pointTypeD = CaseType("PointD", List("YB" -> UNION(LONG, UNIT)))

  val AorB    = UNION(pointTypeA.typeRef, pointTypeB.typeRef)
  val AorBorC = UNION(pointTypeA.typeRef, pointTypeB.typeRef, pointTypeC.typeRef)
  val BorC    = UNION(pointTypeB.typeRef, pointTypeC.typeRef)
  val CorD    = UNION(pointTypeC.typeRef, pointTypeD.typeRef)

  val pointAInstance  = CaseObj(pointTypeA.typeRef, Map("X"  -> 3L, "YA" -> 40L))
  val pointBInstance  = CaseObj(pointTypeB.typeRef, Map("X"  -> 3L, "YB" -> 41L))
  val pointCInstance  = CaseObj(pointTypeC.typeRef, Map("YB" -> 42L))
  val pointDInstance1 = CaseObj(pointTypeD.typeRef, Map("YB" -> 43L))

  val pointDInstance2 = CaseObj(pointTypeD.typeRef, Map("YB" -> unit))

  val sampleTypes = Seq(pointTypeA, pointTypeB, pointTypeC, pointTypeD) ++ Seq(UnionType("PointAB", AorB.l),
                                                                               UnionType("PointBC", BorC.l),
                                                                               UnionType("PointCD", CorD.l))

  def sampleUnionContext(instance: CaseObj) =
    EvaluationContext.build(Map.empty, Map("p" -> LazyVal(EitherT.pure(instance))), Seq.empty)

  def emptyBlockchainEnvironment(h: Int = 1, in: Coeval[Tx :+: Ord :+: CNil] = Coeval(???), nByte: Byte = 'T'): Environment = new Environment {
    override def height: Long  = h
    override def chainId: Byte = nByte
    override def inputEntity   = in()

    override def transactionById(id: Array[Byte]): Option[Tx]                                                    = ???
    override def transactionHeightById(id: Array[Byte]): Option[Long]                                            = ???
    override def data(recipient: Recipient, key: String, dataType: DataType): Option[Any]                        = ???
    override def resolveAlias(name: String): Either[String, Recipient.Address]                                   = ???
    override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] = ???
  }

  def addressFromPublicKey(chainId: Byte, pk: Array[Byte], addressVersion: Byte = EnvironmentFunctions.AddressVersion): Array[Byte] = {
    val publicKeyHash   = global.secureHash(pk).take(EnvironmentFunctions.HashLength)
    val withoutChecksum = addressVersion +: chainId +: publicKeyHash
    withoutChecksum ++ global.secureHash(withoutChecksum).take(EnvironmentFunctions.ChecksumLength)
  }

  def addressFromString(chainId: Byte, str: String): Either[String, Option[Array[Byte]]] = {
    val base58String = if (str.startsWith(EnvironmentFunctions.AddressPrefix)) str.drop(EnvironmentFunctions.AddressPrefix.length) else str
    global.base58Decode(base58String, Global.MaxAddressLength) match {
      case Left(e) => Left(e)
      case Right(addressBytes) =>
        val version = addressBytes.head
        val network = addressBytes.tail.head
        lazy val checksumCorrect = {
          val checkSum = addressBytes.takeRight(EnvironmentFunctions.ChecksumLength)
          val checkSumGenerated =
            global.secureHash(addressBytes.dropRight(EnvironmentFunctions.ChecksumLength)).take(EnvironmentFunctions.ChecksumLength)
          checkSum sameElements checkSumGenerated
        }

        if (version == EnvironmentFunctions.AddressVersion && network == chainId && addressBytes.length == EnvironmentFunctions.AddressLength && checksumCorrect)
          Right(Some(addressBytes))
        else Right(None)
    }
  }
}
