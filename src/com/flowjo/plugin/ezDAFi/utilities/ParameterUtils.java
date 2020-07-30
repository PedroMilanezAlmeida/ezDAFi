package com.flowjo.plugin.ezDAFi.utilities;

import com.treestar.flowjo.core.GParameter;
import com.treestar.flowjo.core.ParameterList;
import com.treestar.flowjo.core.Sample;

import java.util.ArrayList;
import java.util.List;

public class ParameterUtils {

    private static boolean debug = false;

    /**
     * Print string if debug is false
     *
     * @param string String to print
     */
    private static void syso(String string) {
        if (debug) {
            System.out.println(string);
        }
    }


    /**
     * Checks if current sample has this parameter
     *
     * @param sample    Sample to check
     * @param parameter Parameter
     * @return boolean if parameter is true in Sample or false if not
     */
    public static boolean checkParameterExists(Sample sample, String parameter) {
        syso("Checking if parameter exists");
        ParameterList parameters = sample.getAllParameters();
        boolean found = false;
        for (GParameter param : parameters) {
            if (param.getName().equalsIgnoreCase(parameter)) {
                found = true;
            }
        }
        syso("Found :" + parameter + ": " + found);
        return found;
    }

    public static ArrayList<String> checkSamplesContainsAllParametersReturnMissing(Sample sample, List<String> parameters) {
        ArrayList<String> missingParameters = new ArrayList<>();

        if (sample != null) {
            ParameterList sampleParameters = sample.getAllParameters();
            for (String parameter : parameters) {
                if (!sampleParameters.contains(parameter)) {
                    missingParameters.add(parameter);
                }
            }
        }


        return missingParameters;
    }
}
