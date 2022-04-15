package com.wavesenterprise.lang.v1.parser

import scodec.bits.ByteVector

object Expressions {

  sealed trait Pos {
    def end: Int
    def start: Int
  }

  object Pos {
    def apply(start: Int, end: Int): Pos = RealPos(start, end)

    override def equals(obj: scala.Any): Boolean = super.equals(obj)

    final case class RealPos(start: Int, end: Int) extends Pos { self =>
      override def equals(obj: scala.Any): Boolean = obj match {
        case AnyPos        => true
        case RealPos(s, e) => s == start && e == end
        case _             => false
      }
    }
    case object AnyPos extends Pos {
      override def equals(obj: scala.Any): Boolean = obj match {
        case _: Pos => true
        case _      => false
      }

      override def start: Int = -1
      override def end: Int   = -1
    }
  }

  trait Positioned {
    def position: Pos
  }

  sealed trait PART[+T] extends Positioned
  object PART {
    case class VALID[T](position: Pos, v: T)           extends PART[T]
    case class INVALID(position: Pos, message: String) extends PART[Nothing]
  }

  case class LET(position: Pos, name: PART[String], value: EXPR, types: Seq[PART[String]], allowShadowing: Boolean = false) extends Positioned

  sealed trait EXPR                                                             extends Positioned
  case class CONST_LONG(position: Pos, value: Long)                             extends EXPR
  case class GETTER(position: Pos, ref: EXPR, field: PART[String])              extends EXPR
  case class CONST_BYTEVECTOR(position: Pos, value: PART[ByteVector])           extends EXPR
  case class CONST_STRING(position: Pos, value: PART[String])                   extends EXPR
  case class BINARY_OP(position: Pos, a: EXPR, kind: BinaryOperation, b: EXPR)  extends EXPR
  case class BLOCK(position: Pos, let: LET, body: EXPR)                         extends EXPR
  case class IF(position: Pos, cond: EXPR, ifTrue: EXPR, ifFalse: EXPR)         extends EXPR
  case class REF(position: Pos, key: PART[String])                              extends EXPR
  case class TRUE(position: Pos)                                                extends EXPR
  case class FALSE(position: Pos)                                               extends EXPR
  case class FUNCTION_CALL(position: Pos, name: PART[String], args: List[EXPR]) extends EXPR
  case class MATCH_CASE(position: Pos, newVarName: Option[PART[String]], types: Seq[PART[String]], expr: EXPR)
  case class MATCH(position: Pos, expr: EXPR, cases: Seq[MATCH_CASE]) extends EXPR

  case class INVALID(position: Pos, message: String) extends EXPR
  implicit class PartOps[T](val self: PART[T]) extends AnyVal {
    def toEither: Either[String, T] = self match {
      case Expressions.PART.VALID(_, x)         => Right(x)
      case Expressions.PART.INVALID(p, message) => Left(s"Can't compile an invalid instruction: $message in ${p.start}-${p.end}")
    }
  }
}
