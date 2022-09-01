package com.wavesenterprise.lang.v1.parser

import com.wavesenterprise.lang.v1.parser.BinaryOperation._
import com.wavesenterprise.lang.v1.parser.Expressions._
import com.wavesenterprise.lang.v1.parser.UnaryOperation._
import fastparse.MultiLineWhitespace._
import fastparse._
import scodec.bits.ByteVector

object Parser {

  private val Global                                        = com.wavesenterprise.lang.hacks.Global // Hack for IDEA
  implicit def hack(p: fastparse.P[Any]): fastparse.P[Unit] = p.map(_ => ())

  val keywords               = Set("let", "base58", "base64", "true", "false", "if", "then", "else", "match", "case")
  def lowerChar[_: P]        = CharIn("a-z")
  def upperChar[_: P]        = CharIn("A-Z")
  def char[_: P]             = lowerChar | upperChar
  def digit[_: P]            = CharIn("0-9")
  def unicodeSymbolP[_: P]   = P("\\u" ~/ Pass ~~ (char | digit).repX(0, "", 4))
  def notEndOfString[_: P]   = CharPred(_ != '\"')
  def specialSymbols[_: P]   = P("\\" ~~ AnyChar)
  def comment[_: P]: P[Unit] = P("#" ~~ CharPred(_ != '\n').repX).rep.map(_ => ())

  def escapedUnicodeSymbolP[_: P]: P[(Int, String, Int)] = P(Index ~~ (NoCut(unicodeSymbolP) | specialSymbols).! ~~ Index)
  def stringP[_: P]: P[EXPR] =
    P(Index ~~ "\"" ~/ Pass ~~ (escapedUnicodeSymbolP | notEndOfString).!.repX ~~ "\"" ~~ Index)
      .map {
        case (start, xs, end) =>
          var errors         = Vector.empty[String]
          val consumedString = new StringBuilder

          xs.foreach { x =>
            if (x.startsWith("\\u")) {
              if (x.length == 6) {
                val hexCode = x.drop(2)
                try {
                  val int           = Integer.parseInt(hexCode, 16)
                  val unicodeSymbol = new String(Character.toChars(int))
                  consumedString.append(unicodeSymbol)
                } catch {
                  case _: NumberFormatException =>
                    consumedString.append(x)
                    errors :+= s"can't parse '$hexCode' as HEX string in '$x'"
                  case _: IllegalArgumentException =>
                    consumedString.append(x)
                    errors :+= s"invalid UTF-8 symbol: '$x'"
                }
              } else {
                consumedString.append(x)
                errors :+= s"incomplete UTF-8 symbol definition: '$x'"
              }
            } else if (x.startsWith("\\")) {
              if (x.length == 2) {
                consumedString.append(x(1) match {
                  case 'b'  => "\b"
                  case 'f'  => "\f"
                  case 'n'  => "\n"
                  case 'r'  => "\r"
                  case 't'  => "\t"
                  case '\\' => "\\"
                  case '"'  => "\""
                  case _ =>
                    errors :+= s"""unknown escaped symbol: '$x'. The valid are \b, \f, \n, \r, \t"""
                    x
                })
              } else {
                consumedString.append(x)
                errors :+= s"""invalid escaped symbol: '$x'. The valid are \b, \f, \n, \r, \t"""
              }
            } else {
              consumedString.append(x)
            }
          }

          val r =
            if (errors.isEmpty) PART.VALID(Pos(start + 1, end - 1), consumedString.toString)
            else PART.INVALID(Pos(start + 1, end - 1), errors.mkString(";"))
          (Pos(start, end), r)
      }
      .map(Function.tupled(CONST_STRING))

  def correctVarName[_: P]: P[PART[String]] =
    (Index ~~ (char ~~ (digit | char).repX()).! ~~ Index)
      .filter { case (_, x, _) => !keywords.contains(x) }
      .map { case (start, x, end) => PART.VALID(Pos(start, end), x) }

  def anyVarName[_: P]: P[PART[String]] = (Index ~~ (char ~~ (digit | char).repX()).! ~~ Index).map {
    case (start, x, end) =>
      if (keywords.contains(x)) PART.INVALID(Pos(start, end), s"keywords are restricted: $x")
      else PART.VALID(Pos(start, end), x)
  }

  def invalid[_: P]: P[INVALID] = {
    import fastparse.NoWhitespace._
    P(Index ~~ CharPred(_ != '\n').rep(1) ~~ Index)
      .map {
        case (start, end) => INVALID(Pos(start, end), "can't parse the expression")
      }
  }

  def border[_: P]: P[Unit] = CharIn(" \t\n\r({")

  def numberP[_: P]: P[CONST_LONG] =
    P(Index ~~ (CharIn("+\\-").? ~~ digit.repX(1)).! ~~ ("_" ~~ digit.repX(1).!).repX(0) ~~ Index)
      .map({ case (start, x1, x2, end) => CONST_LONG(Pos(start, end), x2.foldLeft(x1)(_ ++ _).toLong) })

  def trueP[_: P]: P[TRUE]        = P(Index ~~ "true".! ~~ !(char | digit) ~~ Index).map { case (start, _, end) => TRUE(Pos(start, end)) }
  def falseP[_: P]: P[FALSE]      = P(Index ~~ "false".! ~~ !(char | digit) ~~ Index).map { case (start, _, end) => FALSE(Pos(start, end)) }
  def bracesP[_: P]: P[EXPR]      = P("(" ~ baseExpr ~ ")")
  def curlyBracesP[_: P]: P[EXPR] = P("{" ~ baseExpr ~ "}")

  def refP[_: P]: P[REF] = P(correctVarName).map { x =>
    REF(Pos(x.position.start, x.position.end), x)
  }

  def ifP[_: P]: P[IF] = {
    def optionalPart(keyword: String, branch: String): P[EXPR] = (Index ~ (keyword ~/ Index ~ baseExpr.?).?).map {
      case (ifTruePos, ifTrueRaw) =>
        ifTrueRaw
          .map { case (pos, expr) => expr.getOrElse(INVALID(Pos(pos, pos), s"expected a $branch branch's expression")) }
          .getOrElse(INVALID(Pos(ifTruePos, ifTruePos), s"expected a $branch branch"))
    }

    def thenPart[_: P] = optionalPart("then", "true")
    def elsePart[_: P] = optionalPart("else", "false")

    P(Index ~~ "if" ~~ &(border) ~/ Index ~ baseExpr.? ~ thenPart ~ elsePart ~~ Index).map {
      case (start, condPos, condRaw, ifTrue, ifFalse, end) =>
        val cond = condRaw.getOrElse(INVALID(Pos(condPos, condPos), "expected a condition"))
        IF(Pos(start, end), cond, ifTrue, ifFalse)
    } |
      P(Index ~~ "then" ~~ &(border) ~/ Index ~ baseExpr.? ~ elsePart ~~ Index).map {
        case (start, ifTrueExprPos, ifTrueRaw, ifFalse, end) =>
          val ifTrue = ifTrueRaw.getOrElse(INVALID(Pos(ifTrueExprPos, ifTrueExprPos), "expected a true branch's expression"))
          IF(Pos(start, end), INVALID(Pos(start, start), "expected a condition"), ifTrue, ifFalse)
      } |
      P(Index ~~ "else" ~~ &(border) ~/ Index ~ baseExpr.? ~~ Index).map {
        case (start, ifFalseExprPos, ifFalseRaw, end) =>
          val ifFalse = ifFalseRaw.getOrElse(INVALID(Pos(ifFalseExprPos, ifFalseExprPos), "expected a false branch's expression"))
          IF(Pos(start, end), INVALID(Pos(start, start), "expected a condition"), INVALID(Pos(start, start), "expected a true branch"), ifFalse)
      }
  }

  def functionCallArgs[_: P]: P[Seq[EXPR]] = comment ~ baseExpr.rep(sep = comment ~ "," ~ comment) ~ comment

  def maybeFunctionCallP[_: P]: P[EXPR] = (Index ~~ refP ~~ P("(" ~/ functionCallArgs ~ ")").? ~~ Index).map {
    case (start, REF(_, functionName), Some(args), accessEnd) => FUNCTION_CALL(Pos(start, accessEnd), functionName, args.toList)
    case (_, id, None, _)                                     => id
  }

  def extractableAtom[_: P]: P[EXPR] = P(curlyBracesP | bracesP | maybeFunctionCallP)

  abstract class Accessor
  case class Getter(name: PART[String]) extends Accessor
  case class ListIndex(index: EXPR)     extends Accessor

  def matchCaseP(implicit c: fastparse.P[Any]): P[MATCH_CASE] = {
    def restMatchCaseInvalidP(implicit c: fastparse.P[Any]): P[String] = P((!P("=>") ~~ AnyChar.!).repX.map(_.mkString))
    def varDefP: P[Option[PART[String]]]                               = anyVarName.map(Some(_)) | "_".!.map(_ => None)
    def typesP: P[Seq[PART[String]]]                                   = anyVarName.rep(min = 1, sep = comment ~ "|" ~ comment)
    def typesDefP =
      (
        ":" ~ comment ~
          (typesP | (Index ~~ restMatchCaseInvalidP ~~ Index).map {
            case (start, _, end) => Seq(PART.INVALID(Pos(start, end), "the type for variable should be specified: `case varName: Type => expr`"))
          })
      ).?.map(_.getOrElse(List.empty))

    P(
      Index ~~ "case" ~~ &(border) ~ comment ~/ (
        (varDefP ~ comment ~ typesDefP) |
          (Index ~~ restMatchCaseInvalidP ~~ Index).map {
            case (start, _, end) =>
              (
                Some(PART.INVALID(Pos(start, end), "invalid syntax, should be: `case varName: Type => expr` or `case _ => expr`")),
                Seq.empty[PART[String]]
              )
          }
      ) ~ comment ~ "=>" ~/ baseExpr.? ~~ Index
    ).map {
      case (start, (v, types), e, end) =>
        val exprStart = types.lastOption.orElse(v).fold(start)(_.position.end)
        MATCH_CASE(
          Pos(start, end),
          newVarName = v,
          types = types,
          expr = e.getOrElse(INVALID(Pos(exprStart, end), "expected expression"))
        )
    }
  }

  def matchP[_: P]: P[EXPR] =
    P(Index ~~ "match" ~~ &(border) ~/ baseExpr ~ "{" ~ comment ~ matchCaseP.rep(sep = comment) ~ comment ~ "}" ~~ Index)
      .map {
        case (start, _, Nil, end)   => INVALID(Pos(start, end), "pattern matching requires case branches")
        case (start, e, cases, end) => MATCH(Pos(start, end), e, cases.toList)
      }

  def accessP[_: P]: P[(Int, Accessor, Int)] = P(
    ("" ~ comment ~ Index ~ "." ~/ comment ~ anyVarName.map(Getter) ~~ Index) |
      (Index ~~ "[" ~/ baseExpr.map(ListIndex) ~ "]" ~~ Index)
  )

  def maybeAccessP[_: P]: P[EXPR] =
    P(Index ~~ extractableAtom ~~ Index ~~ NoCut(accessP).rep)
      .map {
        case (start, obj, objEnd, accessors) =>
          accessors.foldLeft(obj) {
            case (e, (accessStart, a, accessEnd)) =>
              a match {
                case Getter(n)        => GETTER(Pos(start, accessEnd), e, n)
                case ListIndex(index) => FUNCTION_CALL(Pos(start, accessEnd), PART.VALID(Pos(accessStart, accessEnd), "getElement"), List(e, index))
              }
          }
      }

  def byteVectorP[_: P]: P[EXPR] =
    P(Index ~~ "base" ~~ ("58" | "64").! ~~ "'" ~/ Pass ~~ CharPred(_ != '\'').repX.! ~~ "'" ~~ Index)
      .map {
        case (start, base, xs, end) =>
          val innerStart = start + 8
          val innerEnd   = end - 1
          val decoded = base match {
            case "58" => Global.base58Decode(xs)
            case "64" => Global.base64Decode(xs)
          }
          decoded match {
            case Left(err) => CONST_BYTEVECTOR(Pos(start, end), PART.INVALID(Pos(innerStart, innerEnd), err))
            case Right(r)  => CONST_BYTEVECTOR(Pos(start, end), PART.VALID(Pos(innerStart, innerEnd), ByteVector(r)))
          }
      }

  def letP[_: P]: P[LET] =
    P(Index ~~ "let" ~~ &(CharIn(" \t\n\r")) ~/ comment ~ Index ~ anyVarName.? ~ comment ~ Index ~ ("=" ~/ Index ~ baseExpr.?).? ~~ Index)
      .map {
        case (start, namePos, nameRaw, valuePos, valueRaw, end) =>
          val name = nameRaw.getOrElse(PART.INVALID(Pos(namePos, namePos), "expected a variable's name"))
          val value = valueRaw
            .map { case (pos, expr) => expr.getOrElse(INVALID(Pos(pos, pos), "expected a value's expression")) }
            .getOrElse(INVALID(Pos(valuePos, valuePos), "expected a value"))
          LET(Pos(start, end), name, value, Seq.empty)
      }

  private def blockOr(otherExpr: Pos => EXPR)(implicit c: fastparse.P[Any]): P[EXPR] = {
    // Hack to force parse of "\n". Otherwise it is treated as a separator
    def newLineSep(implicit c: fastparse.P[Any]) = {
      P(CharsWhileIn(" \t\r").repX ~~ "\n").repX(1)
    }

    P(
      Index ~~
        letP.rep(1) ~/
        Pass ~~
        (
          ("" ~ ";") ~/ (baseExpr | invalid).? |
            newLineSep ~/ (baseExpr | invalid).? |
            (Index ~~ CharPred(_ != '\n').repX).map(pos => Some(otherExpr(Pos(pos, pos))))
        ) ~~
        Index
    ).map {
      case (start, ls, body, end) =>
        ls.reverse
          .foldLeft(body.getOrElse(INVALID(Pos(end, end), "expected a body"))) { (acc, l) =>
            BLOCK(Pos(start, end), l, acc)
          }
    }
  }

  def block[_: P]: P[EXPR] = blockOr(INVALID(_, "expected ';'"))

  def baseAtom[_: P](epn: fastparse.P[Any] => P[EXPR]): P[EXPR] = {
    def ep[_: P](implicit c: fastparse.P[Any]) = epn(c)
    comment ~
      P(ifP | matchP | ep | byteVectorP | stringP | numberP | trueP | falseP | block | maybeAccessP) ~
      comment
  }

  def baseExpr[_: P]: P[EXPR] = P(binaryOp(baseAtom(block(_))(_), opsByPriority))

  def binaryOp(atomA: fastparse.P[Any] => P[EXPR], rest: List[List[BinaryOperation]])(implicit c: fastparse.P[Any]): P[EXPR] = {
    def atom(implicit c: fastparse.P[Any]) = atomA(c)
    rest match {
      case Nil => unaryOp(atom(_), unaryOps)
      case kinds :: restOps =>
        def operand(implicit c: fastparse.P[Any]) = binaryOp(atom(_), restOps)
        val kindc = kinds
          .map(o => { implicit c: fastparse.P[Any] =>
            o.parser
          })
          .reduce((plc, prc) => {
            def pl(implicit c: fastparse.P[Any]) = plc(c)
            def pr(implicit c: fastparse.P[Any]) = prc(c);
            { implicit c: fastparse.P[Any] =>
              P(pl | pr)
            }
          })
        def kind(implicit c: fastparse.P[Any]) = kindc(c)
        P(Index ~~ operand ~ P(kind ~ (NoCut(operand) | Index.map(i => INVALID(Pos(i, i), "expected a second operator")))).rep).map {
          case (start, left: EXPR, r: Seq[(BinaryOperation, EXPR)]) =>
            r.foldLeft(left) { case (acc, (currKind, currOperand)) => currKind.expr(start, currOperand.position.end, acc, currOperand) }
        }
    }
  }

  def unaryOp(atom: fastparse.P[Any] => P[EXPR], ops: List[UnaryOperation])(implicit c: fastparse.P[Any]): P[EXPR] =
    (ops.foldRight(atom) {
      case (op, accc) =>
        def acc(implicit c: fastparse.P[Any]) = accc(c);
        { implicit c: fastparse.P[Any] =>
          (Index ~~ op.parser.map(_ => ()) ~ P(unaryOp(atom, ops)) ~~ Index).map {
            case (start, expr, end) => op.expr(start, end, expr)
          } | acc
        }
    })(c)

  def apply(str: String): Parsed[EXPR] = {
    def expr[_: P] = P(Start ~ (baseExpr | invalid) ~ End)
    parse(str, expr(_))
  }
}
