package com.flowjo.plugin.ezDAFi.utilities;

import com.treestar.flowjo.application.workspace.Workspace;
import com.treestar.flowjo.core.nodes.PopNode;
import com.treestar.flowjo.fjml.types.FJPrefsFJML;
import com.treestar.flowjo.prefs.FJPrefs;

import java.util.ArrayList;
import java.util.List;

public class PreferencesUtils {

    public static void setStandardPopNames(FJPrefs preferences, List<String> newPopulationNames) {
        preferences.set(FJPrefsFJML.Gates.standardPopulationNames, newPopulationNames);
        preferences.savePrefs();
    }


    public static void getPreferences(PopNode popNode){

       FJPrefs prefs = popNode.getWorkspace().getPrefs();
       List<String> defaultPopNames = new ArrayList<>();
       defaultPopNames.add("CD4");
        defaultPopNames.add("CD8");
        setStandardPopNames(prefs, defaultPopNames);
       List<String> popNames =  prefs.getStandardPopNames();
    }


}
