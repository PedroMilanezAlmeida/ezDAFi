package com.flowjo.plugin.DAFi.utilities;

import java.math.BigInteger;

public class MathUtilities {

    public static double roundTwoDecimal(double value){
        return  Math.round(value * 100.0) / 100.0;
    }

    public static double nextPowerOf10(double v) {
        return BigInteger.TEN.pow((int) Math.max(0, 1 + Math.floor(Math.log10(v)))).doubleValue();
    }

}
