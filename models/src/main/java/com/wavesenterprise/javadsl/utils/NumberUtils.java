package com.wavesenterprise.javadsl.utils;

import com.wavesenterprise.settings.Constants;

public class NumberUtils {
    public static long doubleToWest(double d) {
        return Double.valueOf(d * Constants.UnitsInWest()).longValue();
    }
}
