package com.wavesenterprise.transaction.docker

import com.wavesenterprise.account.Address

trait ConfidentialDataInCreateContractSupported {

  def isConfidential: Boolean

  def groupParticipants: Set[Address]

  def groupOwners: Set[Address]

}
