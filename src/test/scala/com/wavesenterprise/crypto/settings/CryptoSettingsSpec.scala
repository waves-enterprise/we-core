package com.wavesenterprise.crypto.settings

import com.wavesenterprise.crypto.internals.pki.Models.{CustomExtendedKeyUsage, ExtendedKeyUsage}
import com.wavesenterprise.settings.PkiCryptoSettings.{DisabledPkiSettings, EnabledPkiSettings}
import com.wavesenterprise.settings.{CryptoSettings, TestWavesCrypto}
import org.scalatest.{FreeSpec, Matchers}
import pureconfig.ConfigSource
import pureconfig.error.ConfigReaderException

class CryptoSettingsSpec extends FreeSpec with Matchers {

  "should read crypto config" in {
    val config = ConfigSource.string {
      """
        |crypto {
        |  type = TestWavesCrypto
        |  pki {
        |    mode = ON
        |    required-oids = ["1.2.3.4.5.6.7.8.9", "192.168.0.1.255.255.255.0", "1.3.6.1.5.5.7.3.3", "3", "EmailProtection"]
        |    crl-checks-enabled = yes
        |  }
        |}
        |""".stripMargin
    }

    val expectedEkus: Set[ExtendedKeyUsage] = Set(
      CustomExtendedKeyUsage(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)),
      CustomExtendedKeyUsage(Array(192, 168, 0, 1, 255, 255, 255, 0)),
      CustomExtendedKeyUsage(Array(3)),
      ExtendedKeyUsage.CodeSigning,
      ExtendedKeyUsage.EmailProtection
    )

    config.loadOrThrow[CryptoSettings] shouldBe TestWavesCrypto(EnabledPkiSettings(expectedEkus, crlChecksEnabled = true))
  }

  "should ignore crypto.type when waves-crypto is used" in {
    val config = ConfigSource.string {
      """
        |waves-crypto = no
        |crypto {
        |  type = TestWavesCrypto
        |  pki {
        |    mode = ON
        |    required-oids = ["1.2.3.4.5.6.7.8.9", "192.168.0.1.255.255.255.0", "1.3.6.1.5.5.7.3.3", "EmailProtection"]
        |    crl-checks-enabled = yes
        |  }
        |}
        |""".stripMargin
    }

    val expectedEkus: Set[ExtendedKeyUsage] = Set(
      CustomExtendedKeyUsage(Array(1, 2, 3, 4, 5, 6, 7, 8, 9)),
      CustomExtendedKeyUsage(Array(192, 168, 0, 1, 255, 255, 255, 0)),
      ExtendedKeyUsage.CodeSigning,
      ExtendedKeyUsage.EmailProtection
    )

    config.loadOrThrow[CryptoSettings] shouldBe TestWavesCrypto(EnabledPkiSettings(expectedEkus, crlChecksEnabled = true))
  }

  "should ignore required-oids when pki is disabled" in {
    val config = ConfigSource.string {
      """
        |crypto {
        |  type = TestWavesCrypto
        |  pki {
        |    mode = OFF
        |    required-oids = ["1.2.3.4.5.6.7.8.9", "192.168.0.1.255.255.255.0", "1.3.6.1.5.5.7.3.3", "EmailProtection"]
        |    crl-checks-enabled = no
        |  }
        |}
        |""".stripMargin
    }

    config.loadOrThrow[CryptoSettings] shouldBe TestWavesCrypto(DisabledPkiSettings)
  }

  "returns exception on enabled PKI with Waves crypto" in {
    val config = ConfigSource.string {
      """
        |crypto {
        |  type = WAVES
        |  pki {
        |    mode = ON
        |    required-oids = ["1.2.3.4.5.6.7.8.9", "192.168.0.1.255.255.255.0", "1.3.6.1.5.5.7.3.3", "EmailProtection"]
        |    crl-checks-enabled = yes
        |  }
        |}
        |""".stripMargin
    }

    (the[ConfigReaderException[_]] thrownBy {
      config.loadOrThrow[CryptoSettings]
    }).getMessage should include {
      "Usage of 'node.crypto.pki = ON | TEST' is forbidden for 'node.crypto.type = Waves'"
    }

    val testConfig = ConfigSource.string {
      """
        |crypto {
        |  type = WAVES
        |  pki {
        |    mode = TEST
        |    required-oids = ["1.2.3.4.5.6.7.8.9", "192.168.0.1.255.255.255.0", "1.3.6.1.5.5.7.3.3", "EmailProtection"]
        |    crl-checks-enabled = no
        |  }
        |}
        |""".stripMargin
    }

    (the[ConfigReaderException[_]] thrownBy {
      testConfig.loadOrThrow[CryptoSettings]
    }).getMessage should include {
      "Usage of 'node.crypto.pki = ON | TEST' is forbidden for 'node.crypto.type = Waves'"
    }
  }

  "returns exception on unexpected 'pki.mode' value" in {
    val config = ConfigSource.string {
      """
        |crypto {
        |  type = TestWavesCrypto
        |  pki {
        |    mode = ENABLED
        |    required-oids = ["1.2.3.4.5.6.7.8.9", "192.168.0.1.255.255.255.0", "1.3.6.1.5.5.7.3.3", "EmailProtection"]
        |    crl-checks-enabled = no
        |  }
        |}
        |""".stripMargin
    }

    (the[ConfigReaderException[_]] thrownBy {
      config.loadOrThrow[CryptoSettings]
    }).getMessage should include {
      "Cannot convert 'ENABLED' to PkiMode: possible values are: [OFF,ON,TEST]"
    }
  }

  "returns exception on unexpected 'crypto.type' value" in {
    val config = ConfigSource.string {
      """
        |crypto {
        |  type = AES
        |  pki {
        |    mode = ENABLED
        |    required-oids = ["1.2.3.4.5.6.7.8.9", "192.168.0.1.255.255.255.0", "1.3.6.1.5.5.7.3.3", "EmailProtection"]
        |    crl-checks-enabled = no
        |  }
        |}
        |""".stripMargin
    }

    (the[ConfigReaderException[_]] thrownBy {
      config.loadOrThrow[CryptoSettings]
    }).getMessage should include {
      "Cannot convert 'AES' to CryptoType: possible values are: [TestWavesCrypto, Waves]"
    }
  }

  "returns exception on wrong OID format" in {
    val config = ConfigSource.string {
      """
        |crypto {
        |  type = TestWavesCrypto
        |  pki {
        |    mode = ON
        |    required-oids = ["1a", "blabla"]
        |    crl-checks-enabled = yes
        |  }
        |}
        |""".stripMargin
    }

    val errorMessage = (the[ConfigReaderException[_]] thrownBy {
      config.loadOrThrow[CryptoSettings]
    }).getMessage

    errorMessage should include("Extended key usage '1a' mismatch OID pattern")
    errorMessage should include("Extended key usage 'blabla' mismatch OID pattern")
  }

  "returns exception upon disabling CRL checks for 'pki.mode = ON'" in {
    val config = ConfigSource.string {
      """
        |crypto {
        |  type = TestWavesCrypto
        |  pki {
        |    mode = ON
        |    required-oids = ["1.2.3.4.5.6.7.8.9", "192.168.0.1.255.255.255.0", "1.3.6.1.5.5.7.3.3", "EmailProtection"]
        |    crl-checks-enabled = no
        |  }
        |}
        |""".stripMargin
    }

    (the[ConfigReaderException[_]] thrownBy {
      config.loadOrThrow[CryptoSettings]
    }).getMessage should include {
      "Setting 'crl-checks-enabled = false' is forbidden for 'pki.mode = ON'"
    }
  }
}
