package com.wavesenterprise.javadsl.acl;

public interface Role {
    com.wavesenterprise.acl.Role MINER = com.wavesenterprise.acl.Role.Miner$.MODULE$;
    com.wavesenterprise.acl.Role ISSUER = com.wavesenterprise.acl.Role.Issuer$.MODULE$;
    com.wavesenterprise.acl.Role DEXER = com.wavesenterprise.acl.Role.Dexer$.MODULE$;
    com.wavesenterprise.acl.Role PERMISSIONER = com.wavesenterprise.acl.Role.Permissioner$.MODULE$;
    com.wavesenterprise.acl.Role BLACKLISTER = com.wavesenterprise.acl.Role.Blacklister$.MODULE$;
    com.wavesenterprise.acl.Role BANNED = com.wavesenterprise.acl.Role.Banned$.MODULE$;
    com.wavesenterprise.acl.Role CONTRACT_DEVELOPER = com.wavesenterprise.acl.Role.ContractDeveloper$.MODULE$;
    com.wavesenterprise.acl.Role CONNECTION_MANAGER = com.wavesenterprise.acl.Role.ConnectionManager$.MODULE$;
    com.wavesenterprise.acl.Role SENDER = com.wavesenterprise.acl.Role.Sender$.MODULE$;
    com.wavesenterprise.acl.Role CONTRACT_VALIDATOR = com.wavesenterprise.acl.Role.ContractValidator$.MODULE$;
}
