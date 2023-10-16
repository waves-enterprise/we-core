package com.wavesenterprise.transaction.docker

import com.wavesenterprise.account.Address

trait ConfidentialDataInUpdateContractSupported {

  def groupParticipants: Set[Address]

  def groupOwners: Set[Address]

}
