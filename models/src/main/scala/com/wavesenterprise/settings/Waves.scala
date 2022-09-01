package com.wavesenterprise.settings
import com.wavesenterprise.crypto.WavesCryptoContext
import com.wavesenterprise.crypto.internals.{CryptoContext, CryptoError}
import com.wavesenterprise.lang.WavesGlobal
import com.wavesenterprise.lang.v1.BaseGlobal

case class Waves(pkiSettings: PkiCryptoSettings) extends CryptoSettings {
  override def byteId: Byte                                = 0
  override def cryptoContext: CryptoContext                = new WavesCryptoContext()
  override def rideContext: BaseGlobal                     = WavesGlobal
  override def allowedPkiModes: Set[PkiMode]               = Set(PkiMode.OFF)
  override def checkEnvironment: Either[CryptoError, Unit] = Right(())
}
