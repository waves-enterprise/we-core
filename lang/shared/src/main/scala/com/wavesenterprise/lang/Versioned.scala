package com.wavesenterprise.lang

trait Versioned {
  type Ver <: ScriptVersion
  val version: Ver
}
