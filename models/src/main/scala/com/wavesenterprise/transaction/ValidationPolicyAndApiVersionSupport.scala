package com.wavesenterprise.transaction

import com.wavesenterprise.docker.ContractApiVersion
import com.wavesenterprise.docker.validator.ValidationPolicy
import com.wavesenterprise.transaction.docker.ExecutableTransaction

trait ValidationPolicyAndApiVersionSupport { self: ExecutableTransaction =>
  def apiVersion: ContractApiVersion
  def validationPolicy: ValidationPolicy
}
