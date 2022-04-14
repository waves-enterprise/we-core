package com.wavesenterprise

/**
  * Configures node host address. Used by Gitlab CI environment. Default value is 'localhost'
  */
trait NodeHost {

  val nodeHost: String = {
    Option(System.getenv("NODE_HOST")).getOrElse("localhost")
  }
}
