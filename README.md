[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.wavesenterprise/we-core/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.wavesenterprise/we-core)
[![Telegram](https://badgen.net/badge/icon/Waves%20Enterprise%20Group?icon=telegram&label=Telegram)](https://t.me/wavesenterprisegroup)

---
## 1. Introduction

`we-core` is a fundamental library for Waves Enterprise blockchain platform. It lies in the core of the Waves Enterprise node providing essential domain objects and classes as well as utilities and crypto.

The library could also be used to integrate with the node.

## 2. Using we-core
* Gradle

        dependencies {
              implementation 'com.wavesenterprise:we-core:1.12.2'
        }
* SBT

      libraryDependencies += "com.wavesenterprise" % "we-core" % "1.12.2"
---
## 3. Code walkthrough

In order to work with transactions from blockchain, cryptography must be initialised. Address scheme byte must be also provided according to a target blockchain network.

Classes from `com.wavesenterprise.javadsl` package should be considered for better compatibility with Java.

* Waves cryptography

      import com.wavesenterprise.crypto.CryptoInitializer;
      import com.wavesenterprise.settings.javadsl.CryptoSettings;
      import com.wavesenterprise.account.AddressScheme;

      CryptoInitializer.init(CryptoSettings.WAVES_CRYPTO_SETTINGS);
      AddressScheme.setAddressSchemaByte('T');

Retrieving key pair from a key store.

    import com.wavesenterprise.javadsl.crypto.internals.KeyStore;
    import com.wavesenterprise.javadsl.crypto.CryptoState;

    com.wavesenterprise.crypto.internals.KeyStore<KeyPair> keyStore = CryptoState.keyStore(Optional.of(new File("keystore_path")), "keystore_password".toCharArray());
    keyStore.getKeyPair("public_key_alias", Optional.of("private_key_password".toCharArray())).flatMap(keyPair -> {
        // any logic to work with the keyPair
    });

Create and sign a transaction:

    import com.wavesenterprise.account.PrivateKeyAccount;
    import com.wavesenterprise.javadsl.acl.OpType;
    import com.wavesenterprise.javadsl.utils.NumberUtils;
    import com.wavesenterprise.transaction.ValidationError;
    import com.wavesenterprise.transaction.RegisterNodeTransactionV1;
    import scala.Option;

    PrivateKeyAccount account = PrivateKeyAccount.apply(keyPair);
    long fee = NumberUtils.doubleToWest(1.0);
    com.wavesenterprise.acl.OpType add = OpType.ADD;
    Either<ValidationError, RegisterNodeTransactionV1> txOrError = RegisterNodeTransactionV1.selfSigned(account, account, Option.apply("node01"), add, System.currentTimeMillis(), fee);
---
## 4. gRPC API

gRPC services could be found in `com.wavesenterprise.protobuf` package.
