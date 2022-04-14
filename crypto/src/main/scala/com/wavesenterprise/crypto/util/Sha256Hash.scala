package com.wavesenterprise.crypto.util

import java.security.MessageDigest

class Sha256Hash private (digest: MessageDigest) extends Hash {

  override def reset(): Unit = digest.reset()

  override def update(data: Array[Byte]): Sha256Hash = {
    digest.update(data)
    this
  }

  override def result(): Array[Byte] = {
    val hash = digest.digest()
    reset()
    hash
  }
}

object Sha256Hash {

  def apply(): Sha256Hash = new Sha256Hash(MessageDigest.getInstance("SHA-256"))

}
