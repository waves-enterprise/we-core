package com.wavesenterprise.javadsl.crypto.internals;

import com.wavesenterprise.crypto.internals.CryptoError;
import com.wavesenterprise.crypto.internals.KeyPair;
import com.wavesenterprise.crypto.internals.PrivateKey;
import com.wavesenterprise.crypto.internals.PublicKey;
import scala.Boolean;
import scala.Option;
import scala.collection.JavaConverters;
import scala.util.Either;

import java.security.cert.Certificate;
import java.util.List;
import java.util.Optional;

public class KeyStore<KP extends KeyPair> {
    private com.wavesenterprise.crypto.internals.KeyStore<KP> delegate;

    public KeyStore(com.wavesenterprise.crypto.internals.KeyStore<KP> delegate) {
        this.delegate = delegate;
    }

    public Either<CryptoError, PrivateKey> getKey(String alias, Optional<char[]> pwd) {
        return delegate.getKey(alias, pwd.map(Option::apply).orElseGet(Option::empty));
    }

    public Either<CryptoError, PublicKey> getPublicKey(String alias) {
        return delegate.getPublicKey(alias);
    }

    public Either<CryptoError, KP> getKeyPair(String alias, Optional<char[]> pwd) {
        return delegate.getKeyPair(alias, pwd.map(Option::apply).orElseGet(Option::empty));
    }

    public Either<CryptoError, Certificate> getCertificate(String alias) {
        return delegate.getCertificate(alias);
    }

    public Either<CryptoError, Certificate[]> getCertificateChain(String alias) {
        return delegate.getCertificateChain(alias);
    }

    public Optional<PublicKey> generateAndStore(Optional<char[]> pwd) {
        return Optional.ofNullable(delegate.generateAndStore(pwd.map(Option::apply).orElseGet(Option::empty)).getOrElse(null));
    }

    public List<String> aliases() {
        return JavaConverters.seqAsJavaList(delegate.aliases());
    }

    public Either<CryptoError, Boolean> containsAlias(String alias) {
        return delegate.containsAlias(alias).map(b -> (scala.Boolean) b);
    }
}
