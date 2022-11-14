package com.wavesenterprise.settings

import cats.Show
import cats.implicits._
import com.wavesenterprise.crypto.internals.{CryptoContext, CryptoError}
import com.wavesenterprise.crypto.internals.pki.Models.ExtendedKeyUsage
import com.wavesenterprise.lang.v1.BaseGlobal
import com.wavesenterprise.settings.PkiCryptoSettings.{DisabledPkiSettings, EnabledPkiSettings, TestPkiSettings}
import com.wavesenterprise.utils.ScorexLogging
import enumeratum.EnumEntry
import enumeratum.EnumEntry.Uppercase
import org.reflections.Reflections
import pureconfig.error.{CannotConvert, ConfigReaderFailures, ThrowableFailure}
import pureconfig.{ConfigObjectCursor, ConfigReader}
import com.wavesenterprise.utils.StringUtilites._
import pureconfig.generic.semiauto.deriveReader

import scala.util.chaining.scalaUtilChainingOps
import scala.collection.JavaConverters._
import scala.collection.immutable
import scala.concurrent.duration.FiniteDuration

trait CryptoSettings {
  def byteId: Byte
  def cryptoContext: CryptoContext
  def rideContext: BaseGlobal

  def pkiSettings: PkiCryptoSettings
  def allowedPkiModes: Set[PkiMode]
  def checkEnvironment: Either[CryptoError, Unit]

  val name: String = CryptoSettings.getName(getClass)
}

object CryptoSettings extends ScorexLogging {

  private[this] lazy val reflectedSettingsClasses =
    new Reflections(this.getClass.getPackage.getName)
      .getSubTypesOf(classOf[CryptoSettings])
      .asScala

  def allImplementations: Set[String] = reflectedSettingsClasses.view.map(_.getSimpleName).toSet

  def findImplementation(cryptoType: String): Option[Class[_ <: CryptoSettings]] = {
    def predicate(settingsClass: Class[_ <: CryptoSettings]): Boolean = {
      val target  = cryptoType.toUpperCase
      val current = getName(settingsClass)

      current === target
    }

    reflectedSettingsClasses find predicate
  }

  def instantiateSettings(settingsClass: Class[_ <: CryptoSettings], pkiSettings: PkiCryptoSettings): CryptoSettings = {
    val constructor = settingsClass.getDeclaredConstructor(classOf[PkiCryptoSettings])
    constructor.setAccessible(true)
    constructor.newInstance(pkiSettings)
  }

  def cryptoSettingsFromString(
      cryptoType: String,
      pkiCryptoSettings: PkiCryptoSettings = PkiCryptoSettings.DisabledPkiSettings
  ): Either[String, CryptoSettings] =
    for {
      cryptoSettingsClass <- findImplementation(cryptoType)
        .toRight(
          s"Cannot get '$cryptoType' crypto: possible values are: [${CryptoSettings.allImplementations.mkString(", ")}]"
        )

      cryptoSettings = instantiateSettings(
        cryptoSettingsClass,
        pkiCryptoSettings
      )
    } yield cryptoSettings

  private def getName(settingsClass: Class[_ <: CryptoSettings]): String = {
    settingsClass.getSimpleName
      .replace("$", "") // Scala object sign
      .toUpperCase
  }

  implicit val configReader: ConfigReader[CryptoSettings] = ConfigReader.fromCursor { cursor =>
    for {
      objectCursor <- cursor.asObjectCursor
      cryptoCursor <- objectCursor.atKey("crypto").flatMap(_.asObjectCursor)
      cryptoType   <- cryptoCursor.atKey("type").flatMap(_.asString)
      cryptoSettingsClass <- findImplementation(cryptoType)
        .toRight {
          ConfigReaderFailures {
            ThrowableFailure(
              throwable = new NotImplementedError(
                s"Cannot convert '$cryptoType' to CryptoType: possible values are: [${CryptoSettings.allImplementations.mkString(", ")}]"
              ),
              location = cryptoCursor.location
            )
          }
        }
      pkiSettings <- parsePkiSettings(cryptoCursor)
      cryptoSettings = instantiateSettings(cryptoSettingsClass, pkiSettings)
      _ <- validatePkiConfig(cryptoSettings)
    } yield cryptoSettings
  }

  implicit val toPrintable: Show[CryptoSettings] = { settings =>
    s"""
         |type: ${settings.getClass.getSimpleName}
         |PKI:
         |  ${settings.pkiSettings.show pipe dashes}
         """.stripMargin
  }

  private def parsePkiSettings(cryptoCursor: ConfigObjectCursor): Either[ConfigReaderFailures, PkiCryptoSettings] =
    cryptoCursor.atKey("pki").flatMap(_.asObjectCursor) match {
      case Left(failures) =>
        log.trace(s"$failures, disabled pki settings will be used")
        Right(DisabledPkiSettings)
      case Right(pkiCursor) =>
        for {
          pkiMode <- pkiCursor.atKey("mode").flatMap(PkiMode.configReader.from)
          pkiSettings <- pkiMode match {
            case PkiMode.OFF => Right(DisabledPkiSettings)
            case PkiMode.ON =>
              for {
                crlChecksEnabled <- parseCrlChecks(pkiCursor)
                _ <- Either.cond(
                  crlChecksEnabled,
                  Unit,
                  ConfigReaderFailures {
                    ThrowableFailure(new IllegalStateException("Setting 'crl-checks-enabled = false' is forbidden for 'pki.mode = ON'"), None)
                  }
                )
                requiredOids           <- parseRequiredOIds(pkiCursor)
                crlSyncManagerSettings <- parseCrlSyncManagerSettings(pkiCursor)
              } yield EnabledPkiSettings(requiredOids, crlChecksEnabled, crlSyncManagerSettings)
            case PkiMode.TEST =>
              for {
                crlChecksEnabled       <- parseCrlChecks(pkiCursor)
                requiredOids           <- parseRequiredOIds(pkiCursor)
                crlSyncManagerSettings <- parseCrlSyncManagerSettings(pkiCursor)
              } yield {
                log.warn("WARNING: 'node.crypto.pki.mode' is set to 'TEST'. PKI functionality is running in a testing mode.")
                TestPkiSettings(requiredOids, crlChecksEnabled, crlSyncManagerSettings)
              }
          }
        } yield pkiSettings
    }

  private def parseRequiredOIds(cursor: ConfigObjectCursor): Either[ConfigReaderFailures, Set[ExtendedKeyUsage]] =
    cursor
      .atKey("required-oids")
      .flatMap { requiredOidsCursor =>
        ConfigReader[List[ExtendedKeyUsage]].from(requiredOidsCursor).map(_.toSet)
      }

  private def parseCrlChecks(cursor: ConfigObjectCursor): Either[ConfigReaderFailures, Boolean] =
    cursor.atKey("crl-checks-enabled").flatMap(ConfigReader[Boolean].from)

  private def parseCrlSyncManagerSettings(cursor: ConfigObjectCursor) =
    cursor.atKey("crl-sync-manager-settings").flatMap(CrlSyncManagerSettings.configReader.from)

  private def validatePkiConfig(cryptoSettings: CryptoSettings): Either[ConfigReaderFailures, Unit] =
    Either.cond(
      cryptoSettings.allowedPkiModes.contains(cryptoSettings.pkiSettings.getPkiMode),
      (),
      ConfigReaderFailures {
        val currentType = cryptoSettings.getClass.getSimpleName
        ThrowableFailure(
          throwable = new IllegalArgumentException(s"Usage of 'node.crypto.pki = ON | TEST' is forbidden for 'node.crypto.type = $currentType'"),
          location = None
        )
      }
    )
}

sealed abstract class PkiCryptoSettings(val isPkiActive: Boolean) { self =>
  def getPkiMode: PkiMode = self match {
    case PkiCryptoSettings.DisabledPkiSettings   => PkiMode.OFF
    case _: PkiCryptoSettings.EnabledPkiSettings => PkiMode.ON
    case _: PkiCryptoSettings.TestPkiSettings    => PkiMode.TEST
  }
}

case class CrlSyncManagerSettings(period: FiniteDuration)

object CrlSyncManagerSettings {
  val configReader: ConfigReader[CrlSyncManagerSettings] = deriveReader[CrlSyncManagerSettings]

  implicit val toPrintable: Show[CrlSyncManagerSettings] = {
    case CrlSyncManagerSettings(period) =>
      s"""
         |period: $period
         |""".stripMargin
  }
}

object PkiCryptoSettings {
  case object DisabledPkiSettings extends PkiCryptoSettings(isPkiActive = false)
  case class EnabledPkiSettings(
      requiredOids: Set[ExtendedKeyUsage],
      crlChecksEnabled: Boolean,
      crlSyncManagerSettings: CrlSyncManagerSettings
  ) extends PkiCryptoSettings(isPkiActive = true)

  case class TestPkiSettings(
      requiredOids: Set[ExtendedKeyUsage],
      crlChecksEnabled: Boolean,
      crlSyncManagerSettings: CrlSyncManagerSettings
  ) extends PkiCryptoSettings(isPkiActive = true)

  implicit val toPrintable: Show[PkiCryptoSettings] = {
    case DisabledPkiSettings => "mode: OFF"
    case EnabledPkiSettings(requiredOids, crlChecksEnabled, crlSyncManagerSettings) =>
      s"""
         |mode: ON
         |requiredOids: [${requiredOids.map(_.strRepr).mkString(", ")}]
         |crlChecksEnabled: $crlChecksEnabled
         |crlSyncManagerSettings: ${crlSyncManagerSettings.show}
       """.stripMargin
    case TestPkiSettings(requiredOids, crlChecksEnabled, crlSyncManagerSettings) =>
      s"""
         |mode: TEST
         |requiredOids: [${requiredOids.map(_.strRepr).mkString(", ")}]
         |crlChecksEnabled: $crlChecksEnabled
         |crlSyncManagerSettings: ${crlSyncManagerSettings.show}
       """.stripMargin
  }
}

sealed trait PkiMode extends EnumEntry with Uppercase

object PkiMode extends enumeratum.Enum[PkiMode] {
  case object OFF  extends PkiMode
  case object ON   extends PkiMode
  case object TEST extends PkiMode

  override val values: immutable.IndexedSeq[PkiMode] = findValues

  val configReader: ConfigReader[PkiMode] = ConfigReader.fromString { str =>
    fromStr(str).toRight(CannotConvert(str, classOf[PkiMode].getSimpleName, s"possible values are: [${values.mkString(",")}]"))
  }

  def fromStr(str: String): Option[PkiMode] = withNameInsensitiveOption(str)
}
