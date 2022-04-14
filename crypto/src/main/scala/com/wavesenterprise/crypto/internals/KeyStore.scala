package com.wavesenterprise.crypto.internals

import java.io.File
import java.security.cert.Certificate

abstract class KeyStore[KeyPair0 <: KeyPair](storageFolder: Option[File], password: Array[Char]) {
  def getKey(alias: String, pwd: Option[Array[Char]] = None): Either[CryptoError, KeyPair0#PrivateKey0]
  def getPublicKey(alias: String): Either[CryptoError, KeyPair0#PublicKey0]
  def getKeyPair(alias: String, pwd: Option[Array[Char]] = None): Either[CryptoError, KeyPair0]
  def getCertificate(alias: String): Either[CryptoError, Certificate]
  def getCertificateChain(alias: String): Either[CryptoError, Array[Certificate]]
  def generateAndStore(pwd: Option[Array[Char]]): Option[KeyPair0#PublicKey0]
  def aliases(): Seq[String]
  def containsAlias(alias: String): Either[CryptoError, Boolean]
  def additionalStorageValidation(): Unit
}

trait KeyStoreProvider[KeyPair0 <: KeyPair] {
  def useKeyStore[R](f: KeyStore[KeyPair0] => R): R
}
