package com.wavesenterprise

import com.github.dockerjava.api.DockerClient
import com.github.dockerjava.core.{DefaultDockerClientConfig, DockerClientImpl}
import com.github.dockerjava.httpclient5.ApacheDockerHttpClient

object DockerClientBuilder {
  def createDefaultApacheDockerClient(): DockerClient = {
    val configBuilder = DefaultDockerClientConfig.createDefaultConfigBuilder
    val clientBuilder = new ApacheDockerHttpClient.Builder()

    val config           = configBuilder.build()
    val dockerHttpClient = clientBuilder.dockerHost(config.getDockerHost).sslConfig(config.getSSLConfig).build()

    DockerClientImpl.getInstance(config, dockerHttpClient)
  }

}
