package com.wavesenterprise.javadsl.crypto;

import com.wavesenterprise.account.PrivateKeyAccount;
import com.wavesenterprise.account.PublicKeyAccount;
import com.wavesenterprise.crypto.internals.*;
import com.wavesenterprise.javadsl.crypto.internals.KeyStore;
import com.wavesenterprise.crypto.package$;
import com.wavesenterprise.settings.CryptoSettings;
import scala.Option;
import scala.collection.JavaConverters;
import scala.util.Either;

import java.io.File;
import java.util.List;
import java.util.Optional;

public class CryptoState {

    public static final CryptoAlgorithms<KeyPair> ALGORITHMS = package$.MODULE$.algorithms();
    public static final CryptoContext CONTEXT = package$.MODULE$.context();
    public static final CryptoSettings CRYPTO_SETTINGS = package$.MODULE$.cryptoSettings();

    public static final int DIGEST_SIZE = package$.MODULE$.DigestSize();
    public static final int KEY_LENGTH = package$.MODULE$.KeyLength();
    public static final int KEY_STRING_LENGTH = package$.MODULE$.KeyStringLength();
    public static final int SESSION_KEY_LENGTH = package$.MODULE$.SessionKeyLength();
    public static final int SIGNATURE_LENGTH = package$.MODULE$.SignatureLength();
    public static final int WRAPPED_STRUCTURE_LENGTH = package$.MODULE$.WrappedStructureLength();

    public static Optional<CryptoSettings> readEnvForCryptoSettings() {
        return Optional.ofNullable(package$.MODULE$.readEnvForCryptoSettings().getOrElse(null));
    }

    public static String toAlias(KeyPair keyPair, int hashLength, byte chainId, byte addressVersion, int checksumLength) {
        return package$.MODULE$.toAlias(keyPair, hashLength, chainId, addressVersion, checksumLength);
    }

    public static byte[] calcCheckSum(byte[] withoutChecksum, int checksumLength) {
        return package$.MODULE$.calcCheckSum(withoutChecksum, checksumLength);
    }

    public static KeyStore<KeyPair> keyStore(Optional<File> file, char[] password) {
        return new KeyStore<>(package$.MODULE$.keyStore(file.map(Option::apply).orElseGet(Option::empty), password));
    }

    public static KeyPair generateKeyPair() {
        return package$.MODULE$.generateKeyPair();
    }

    public static KeyPair generateSessionKeyPair() {
        return package$.MODULE$.generateSessionKeyPair();
    }

    public static byte[] secureHash(byte[] input) {
        return package$.MODULE$.secureHash(input);
    }

    public static byte[] secureHash(String input) {
        return package$.MODULE$.secureHash(input);
    }

    public static byte[] fastHash(byte[] input) {
        return package$.MODULE$.fastHash(input);
    }

    public static byte[] fastHash(String input) {
        return package$.MODULE$.fastHash(input);
    }

    public static PublicKeyAccount generatePublicKey() {
        return package$.MODULE$.generatePublicKey();
    }

    public static byte[] sign(PrivateKey privateKey, byte[] message) {
        return (byte[]) package$.MODULE$.sign(privateKey, message);
    }

    public static byte[] sign(PrivateKeyAccount privateKeyAccount, byte[] message) {
        return (byte[]) package$.MODULE$.sign(privateKeyAccount, message);
    }

    public static boolean verify(byte[] signature, byte[] message, byte[] privateKey) {
        return package$.MODULE$.verify(signature, message, privateKey);
    }

    public static boolean verify(byte[] signature, byte[] message, PublicKey publicKey) {
        return package$.MODULE$.verify(signature, message, publicKey);
    }

    public static Either<CryptoError, EncryptedForSingle> encrypt(byte[] data, PrivateKey senderPrivateKey, PublicKey recipientPublicKey) {
        return package$.MODULE$.encrypt(data, senderPrivateKey, recipientPublicKey);
    }

    public static Either<CryptoError, EncryptedForMany> encryptForMany(byte[] data, PrivateKey senderPrivateKey, List<PublicKey> recipientPublicKeys) {
        scala.collection.immutable.List<PublicKey> scalaList = JavaConverters.collectionAsScalaIterableConverter(recipientPublicKeys).asScala().toList();
        return package$.MODULE$.encryptForMany(data, senderPrivateKey, scalaList);
    }

    public static Either<CryptoError, byte[]> decrypt(EncryptedForSingle encryptedDataWithKey, PrivateKey recipientPrivateKey, PublicKey senderPublicKey, boolean useModerAlgo) {
        return package$.MODULE$.decrypt(encryptedDataWithKey, recipientPrivateKey, senderPublicKey);
    }

    public static boolean safeIsEqual(byte[] a, byte[] b) {
        return package$.MODULE$.safeIsEqual(a, b);
    }
}
