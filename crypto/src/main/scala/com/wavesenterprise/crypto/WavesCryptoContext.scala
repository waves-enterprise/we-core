package com.wavesenterprise.crypto

import java.io.File

import com.wavesenterprise.account.AddressScheme
import com.wavesenterprise.crypto.internals.{CryptoAlgorithms, CryptoContext, KeyStore, WavesAlgorithms, WavesKeyPair}

private[crypto] class WavesCryptoContext extends CryptoContext {
  override type KeyPair0 = WavesKeyPair
  override val algorithms: CryptoAlgorithms[WavesKeyPair] = WavesAlgorithms
  override def keyStore(file: Option[File], password: Array[Char]): KeyStore[WavesKeyPair] =
    new WavesKeyStore(file, password, AddressScheme.getAddressSchema.chainId)
}
