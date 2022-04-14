package com.wavesenterprise.crypto.util

trait Hash {

  def update(data: Array[Byte]): Hash

  def result(): Array[Byte]

  def reset(): Unit

}
