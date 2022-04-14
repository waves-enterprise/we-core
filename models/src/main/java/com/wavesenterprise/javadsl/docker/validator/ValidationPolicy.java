package com.wavesenterprise.javadsl.docker.validator;

import com.wavesenterprise.account.Address;
import scala.collection.JavaConverters;

import java.util.List;

public interface ValidationPolicy {
    com.wavesenterprise.docker.validator.ValidationPolicy DEFAULT = com.wavesenterprise.docker.validator.ValidationPolicy.Any$.MODULE$;
    com.wavesenterprise.docker.validator.ValidationPolicy ANY = com.wavesenterprise.docker.validator.ValidationPolicy.Any$.MODULE$;
    com.wavesenterprise.docker.validator.ValidationPolicy MAJORITY = com.wavesenterprise.docker.validator.ValidationPolicy.Majority$.MODULE$;

    static com.wavesenterprise.docker.validator.ValidationPolicy majorityWithOneOf(List<Address> addressList) {
        return new com.wavesenterprise.docker.validator.ValidationPolicy.MajorityWithOneOf(JavaConverters.asScalaBuffer(addressList).toList());
    }
}