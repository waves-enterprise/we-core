package com.wavesenterprise.lang.v1.evaluator.ctx.impl

import com.wavesenterprise.lang.ExecutionError
import com.wavesenterprise.lang.v1.compiler.Terms.{CaseObj, CONST_BYTEVECTOR, CONST_STRING}
import com.wavesenterprise.lang.v1.evaluator.ctx.impl.waves.Types
import com.wavesenterprise.lang.v1.traits.domain.Recipient
import com.wavesenterprise.lang.v1.traits.domain.Recipient.{Address, Alias}
import com.wavesenterprise.lang.v1.traits.{DataType, Environment}

class EnvironmentFunctions(environment: Environment) {

  def getData(addressOrAlias: CaseObj, key: String, dataType: DataType): Either[String, Option[Any]] = {
    val objTypeName = addressOrAlias.caseType.name

    val recipientEi =
      if (objTypeName == Types.addressType.name) {
        addressOrAlias.fields
          .get("bytes")
          .toRight("Can't find 'bytes'")
          .map(_.asInstanceOf[CONST_BYTEVECTOR])
          .map(a => Address(a.bs))
      } else if (objTypeName == Types.aliasType.name) {
        addressOrAlias.fields
          .get("alias")
          .toRight("Can't find alias")
          .map(_.asInstanceOf[CONST_STRING])
          .map(a => Alias(a.s))
      } else {
        Left(s"$addressOrAlias neither Address nor alias")
      }

    recipientEi.map(environment.data(_, key, dataType))
  }

  def addressFromAlias(name: String): Either[ExecutionError, Recipient.Address] = environment.resolveAlias(name)

}

object EnvironmentFunctions {
  val ChecksumLength = 4
  val HashLength = 20
  val AddressVersion: Byte = 1
  val AddressLength: Int = 1 + 1 + ChecksumLength + HashLength
  val AddressPrefix = "address:"
}
