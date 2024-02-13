package com.wavesenterprise.transaction

import com.wavesenterprise.docker.validator.ValidationPolicy
import com.wavesenterprise.transaction.docker.ExecutableTransaction

trait ValidationPolicySupport { self: ExecutableTransaction =>

  def validationPolicy: ValidationPolicy

}
