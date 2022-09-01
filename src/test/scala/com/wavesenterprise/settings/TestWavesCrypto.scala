package com.wavesenterprise.settings

import com.wavesenterprise.crypto.WavesCryptoContext
import com.wavesenterprise.crypto.internals.{CryptoContext, CryptoError}
import com.wavesenterprise.lang.WavesGlobal
import com.wavesenterprise.lang.v1.BaseGlobal

case class TestWavesCrypto(pkiSettings: PkiCryptoSettings) extends CryptoSettings {
  override val byteId: Byte                                = 2
  override def cryptoContext: CryptoContext                = new WavesCryptoContext()
  override def rideContext: BaseGlobal                     = WavesGlobal
  override def allowedPkiModes: Set[PkiMode]               = PkiMode.values.toSet
  override def checkEnvironment: Either[CryptoError, Unit] = Right(())
}
