package com.flowjo.plugin.ezDAFi.utilities;

import java.io.File;
import java.util.HashMap;
import java.util.Random;

public class StringUtils {

    public static String getSaltString() {
        return getSaltString(4);
    }

    public static String getSaltString(int length) {
        String chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890";
        StringBuilder salt = new StringBuilder();
        Random rnd = new Random();
        while (salt.length() < length) { // length of the random string.
            int index = (int) (rnd.nextFloat() * chars.length());
            salt.append(chars.charAt(index));
        }
        String saltStr = salt.toString();
        return saltStr;

    }

    public static String concatStrings(char separationString, String... strings) {
        String concatString = "";
        for (String string : strings) {
            concatString += string + separationString;
        }
        return concatString.substring(0, concatString.length() - 1);
    }


    public static String splitString(String string, String splitChar) {
        if (string.contains(splitChar)) {
            return string.substring(0, string.indexOf(splitChar));
        }
        return string;
    }

}
