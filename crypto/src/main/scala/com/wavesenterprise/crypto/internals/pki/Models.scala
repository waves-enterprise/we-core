package com.wavesenterprise.crypto.internals.pki

import cats.data.{NonEmptyList, Validated}
import cats.implicits._
import com.wavesenterprise.crypto.internals.{CryptoError, PkiError, UnknownValueError}
import enumeratum.EnumEntry.Uncapitalised
import enumeratum.{Enum, EnumEntry}
import pureconfig._
import pureconfig.error.CannotConvert

import scala.collection.immutable

object Models {

  sealed abstract class KeyUsage(val id: Int) extends EnumEntry with Uncapitalised

  object KeyUsage extends Enum[KeyUsage] {
    case object DigitalSignature  extends KeyUsage(1)
    case object ContentCommitment extends KeyUsage(2)
    case object KeyEncipherment   extends KeyUsage(4)
    case object DataEncipherment  extends KeyUsage(8)
    case object KeyAgreement      extends KeyUsage(16)
    case object KeyCertSign       extends KeyUsage(32)
    case object CrlSign           extends KeyUsage(64)
    case object EncipherOnly      extends KeyUsage(128)
    case object DecipherOnly      extends KeyUsage(256)

    val values: immutable.IndexedSeq[KeyUsage] = findValues

    implicit val configReader: ConfigReader[KeyUsage] =
      ConfigReader.fromCursor { cur =>
        cur.asString.flatMap { inputStr =>
          KeyUsage.withNameInsensitiveOption(inputStr) match {
            case Some(validKeyUsage) =>
              Right(validKeyUsage)
            case _ =>
              cur.failed(UnknownValueError(inputStr, KeyUsage.values))
          }
        }
      }
  }

  sealed abstract class ExtendedKeyUsage(val ids: Array[Int]) extends EnumEntry with Uncapitalised {
    private val oidStr: String = ids.mkString(".")

    def strRepr: String = ExtendedKeyUsage.oidToValue.get(oidStr).fold(oidStr)(_.entryName)

    override def equals(other: Any): Boolean = other match {
      case that: ExtendedKeyUsage => oidStr == that.oidStr
      case _                      => false
    }

    override def hashCode(): Int = oidStr.hashCode
  }

  case class CustomExtendedKeyUsage(oid: Array[Int]) extends ExtendedKeyUsage(oid)

  object ExtendedKeyUsage extends Enum[ExtendedKeyUsage] {
    case object ServerAuth      extends ExtendedKeyUsage(Array(1, 3, 6, 1, 5, 5, 7, 3, 1))
    case object ClientAuth      extends ExtendedKeyUsage(Array(1, 3, 6, 1, 5, 5, 7, 3, 2))
    case object CodeSigning     extends ExtendedKeyUsage(Array(1, 3, 6, 1, 5, 5, 7, 3, 3))
    case object EmailProtection extends ExtendedKeyUsage(Array(1, 3, 6, 1, 5, 5, 7, 3, 4))
    case object TimeStamping    extends ExtendedKeyUsage(Array(1, 3, 6, 1, 5, 5, 7, 3, 8))
    case object OCSPSigning     extends ExtendedKeyUsage(Array(1, 3, 6, 1, 5, 5, 7, 3, 9))

    override val values: immutable.IndexedSeq[ExtendedKeyUsage] = findValues

    private val oidToValue = values.map(e => e.oidStr -> e).toMap

    private val extendedKeyUsagePattern = """^(\d+\.)*\d+$"""

    def parseString(str: String): Either[CryptoError, ExtendedKeyUsage] = {
      ExtendedKeyUsage.withNameInsensitiveOption(str) match {
        case Some(eku) => Right(eku)
        case None =>
          Either
            .cond(str.matches(extendedKeyUsagePattern), str, PkiError(s"Extended key usage '$str' mismatch OID pattern"))
            .flatMap { validStr =>
              oidToValue.get(str) match {
                case Some(eku) => Right(eku)
                case None =>
                  Either
                    .catchOnly[NumberFormatException](validStr.split("""\.""").map(_.toInt))
                    .leftMap(err => PkiError(s"Unable to parse Integer from OID value: ${err.getMessage}"))
                    .map(CustomExtendedKeyUsage)
              }
            }
      }
    }

    def parseStrings(str: String*): Either[CryptoError, List[ExtendedKeyUsage]] = {
      str.toList
        .traverse { eku =>
          Validated.fromEither(parseString(eku).leftMap(_ => NonEmptyList.of(eku)))
        }
        .toEither
        .leftMap { invalidKeyUsages =>
          PkiError(s"The following extended key usages mismatch OID pattern: [${invalidKeyUsages.toList.mkString(", ")}]")
        }
    }

    implicit val configReader: ConfigReader[ExtendedKeyUsage] = ConfigReader.fromString { str =>
      parseString(str)
        .leftMap { err =>
          CannotConvert(str, ExtendedKeyUsage.getClass.getSimpleName, err.message)
        }
    }
  }
}
