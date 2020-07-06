package com.flowjo.plugin.DAFi;
import java.io.BufferedReader;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringWriter;
import java.util.List;
import java.util.Map;

import com.treestar.lib.file.FileUtil;
import com.treestar.flowjo.engine.EngineManager;
import com.treestar.flowjo.engine.utility.RFlowCalculator;

import com.flowjo.plugin.DAFi.utils.FilenameUtils;

public class DAFiRFlowCalc extends RFlowCalculator {

    // The path to the DAFi R script template from within the jar file
    private final static String DAFiTemplatePath = "r/RScript.DAFi.Template.R";
    public static final String TRUE = "TRUE";
    public static final String FALSE = "FALSE";


    public File runDAFi(String wsName, String wsDir, File sampleFile, String sampleName, String populationName, String sampleNodeName, List<String> parameterNames, Map<String, String> options, String outputFolderPath, boolean useExistingFiles, long millisTime)
    {
        sampleName = sampleName.replaceAll(".ExtNode", "").replaceAll(".fcs", "").replaceAll(".LMD", "").trim();
        File outputFolder = new File(outputFolderPath);

        StringWriter scriptWriter = new StringWriter();
        File DAFiScript = createDAFiscript(wsName, wsDir, sampleFile, sampleName, populationName, sampleNodeName, parameterNames, options, outputFolder, scriptWriter, millisTime);
        if(DAFiScript == null) return null;
        if(useExistingFiles && DAFiScript.exists()) return DAFiScript;

        String scriptFileName = (new StringBuilder()).append("RScript.DAFi.").append(System.currentTimeMillis()).append(".R").toString().replaceAll(" ", "_");
        try
        {
            com.flowjo.plugin.DAFi.RScriptFlowCalculator calc = new com.flowjo.plugin.DAFi.RScriptFlowCalculator();
            fScriptFile = new File(outputFolderPath, scriptFileName);
            FileUtil.write(fScriptFile, scriptWriter.toString());
            calc.setRScriptMode(false);
            calc.executeRBatch(fScriptFile);
        }
        catch(Exception e)
        {
            e.printStackTrace();
        }
        return DAFiScript;
    }

    protected File createDAFiscript(String wsName, String wsDir, File sampleFile, String sampleName, String populationName, String sampleNodeName, List<String> parameterNames, Map<String, String> options, File outputFolder, StringWriter scriptWriter, long millisTime)
    {
        InputStream scriptStream = DAFiRFlowCalc.class.getResourceAsStream(DAFiTemplatePath);

        String outFileName = (new StringBuilder()).append(FilenameUtils.fixFileNamePart(sampleName)).append(".DAFi").append(".csv").toString();

        if(outputFolder == null) {
            String outputFolderMillisTime = sampleFile.getParentFile().getAbsolutePath() + File.separator + millisTime;
            outputFolder = new File(outputFolderMillisTime);
        }
        String outputFolderString = outputFolder.getAbsolutePath();

        File outFile = new File(outputFolder, outFileName);
        outFileName = outFile.getAbsolutePath();
        File gatingMLOutFile = new File(outFile, ".gating-ml2.xml");

        String dataFilePath = sampleFile.getAbsolutePath();

        String millisTimeString = Long.toString(millisTime);
        System.out.println("millisTimeString:" + millisTimeString);

        if(EngineManager.isWindows()) outFileName = outFileName.replaceAll("\\\\", "/");
        if(EngineManager.isWindows()) dataFilePath = dataFilePath.replaceAll("\\\\", "/");
        if(EngineManager.isWindows()) outputFolderString = outputFolderString.replaceAll("\\\\", "/");

        //String sParScale = options.get(com.flowjo.plugin.DAFi.DAFi.scaleOptionName);
        String sParTrans = options.get(com.flowjo.plugin.DAFi.DAFi.transOptionName);
        String sParBatch = options.get(com.flowjo.plugin.DAFi.DAFi.batchOptionName);
        String sParkMeansSom = options.get(com.flowjo.plugin.DAFi.DAFi.kMeansSomOptionName);
        String sParApplyOnChildren = options.get(com.flowjo.plugin.DAFi.DAFi.applyOnChildrenOptionName);
        String sParMinPopSize = options.get(com.flowjo.plugin.DAFi.DAFi.minPopSizeOptionName);
        String sParMinDim = options.get(com.flowjo.plugin.DAFi.DAFi.minDimOptionName);
        String sParMaxDim = options.get(com.flowjo.plugin.DAFi.DAFi.maxDimOptionName);
        String sParXDim = options.get(com.flowjo.plugin.DAFi.DAFi.xDimOptionName);
        String sParYDim = options.get(com.flowjo.plugin.DAFi.DAFi.yDimOptionName);
        String sParApplyOnPrev = options.get(com.flowjo.plugin.DAFi.DAFi.applyOnPrevOptionName);
        String sAddCellIdToResults = com.flowjo.plugin.DAFi.DAFi.isSeqGeq() ? TRUE : FALSE;
        //Added this to add runID to parameter
        String parameterName = com.flowjo.plugin.DAFi.DAFi.pluginName;

        //if (sParScale == null || sParScale.isEmpty() || com.flowjo.plugin.DAFi.DAFi.One.equals(sParScale) || com.flowjo.plugin.DAFi.DAFi.True.equals(sParScale))
//            sParScale = TRUE; // TRUE is the default
        //      else
        //sParScale = FALSE;

        if (sParTrans == null || sParTrans.isEmpty() || com.flowjo.plugin.DAFi.DAFi.One.equals(sParTrans) || com.flowjo.plugin.DAFi.DAFi.True.equals(sParTrans))
            sParTrans = TRUE; // TRUE is the default
        else
            sParTrans = FALSE;

        if (sParBatch == null || sParBatch.isEmpty() || com.flowjo.plugin.DAFi.DAFi.One.equals(sParBatch) || com.flowjo.plugin.DAFi.DAFi.True.equals(sParBatch))
            sParBatch = TRUE; // TRUE is the default
        else
            sParBatch = FALSE;

        if (sParkMeansSom == null || sParkMeansSom.isEmpty() || com.flowjo.plugin.DAFi.DAFi.One.equals(sParkMeansSom) || com.flowjo.plugin.DAFi.DAFi.True.equals(sParkMeansSom))
            sParkMeansSom = TRUE; // TRUE is the default
        else
            sParkMeansSom = FALSE;

        if (sParApplyOnChildren == null || sParApplyOnChildren.isEmpty() || com.flowjo.plugin.DAFi.DAFi.One.equals(sParApplyOnChildren) || com.flowjo.plugin.DAFi.DAFi.True.equals(sParApplyOnChildren))
            sParApplyOnChildren = TRUE; // TRUE is the default
        else
            sParApplyOnChildren = FALSE;

        if (sParApplyOnPrev == null || sParApplyOnPrev.isEmpty())
            sParApplyOnPrev = com.flowjo.plugin.DAFi.DAFi.defaultApplyOnPrev;
        if (sParApplyOnPrev.length() > 5) // i.e., not "None"
            sParApplyOnPrev = outputFolder + File.separator + sParApplyOnPrev;
        if(EngineManager.isWindows()) sParApplyOnPrev = sParApplyOnPrev.replaceAll("\\\\", "/");

        if(EngineManager.isWindows()) wsDir = wsDir.replaceAll("\\\\", "/");
        if(EngineManager.isWindows()) wsName = wsName.replaceAll("\\\\", "/");

        try {
            if ((Integer.parseInt(sParMinPopSize) < 100) || (Integer.parseInt(sParMinPopSize) > 1000000))
                sParMinPopSize= Integer.toString(com.flowjo.plugin.DAFi.DAFi.defaultMinPopSize);
        } catch (Exception e) {
            sParMinPopSize = Integer.toString(com.flowjo.plugin.DAFi.DAFi.defaultMinPopSize);
        }

        try {
            if ((Integer.parseInt(sParMinDim) < 1) || (Integer.parseInt(sParMinDim) > 999999))
                sParMinDim= Integer.toString(com.flowjo.plugin.DAFi.DAFi.defaultMinDim);
        } catch (Exception e) {
            sParMinDim = Integer.toString(com.flowjo.plugin.DAFi.DAFi.defaultMinDim);
        }

        try {
          if ((Integer.parseInt(sParMaxDim) < 1) || (Integer.parseInt(sParMaxDim) > 999999))
              sParMaxDim= Integer.toString(com.flowjo.plugin.DAFi.DAFi.defaultMaxDim);
        } catch (Exception e) {
          sParMaxDim = Integer.toString(com.flowjo.plugin.DAFi.DAFi.defaultMaxDim);
        }

        try {
            if ((Integer.parseInt(sParXDim) < 3) || (Integer.parseInt(sParXDim) > 32))
                sParXDim = Integer.toString(com.flowjo.plugin.DAFi.DAFi.defaultXDim);
        } catch (Exception e) {
            sParXDim = Integer.toString(com.flowjo.plugin.DAFi.DAFi.defaultXDim);
        }

        try {
            if ((Integer.parseInt(sParYDim) < 3) || (Integer.parseInt(sParYDim) > 32))
                sParYDim = Integer.toString(com.flowjo.plugin.DAFi.DAFi.defaultYDim);
        } catch (Exception e) {
            sParYDim = Integer.toString(com.flowjo.plugin.DAFi.DAFi.defaultYDim);
        }

        int minPopSize = com.flowjo.plugin.DAFi.DAFi.defaultMinPopSize;
        int minDim = com.flowjo.plugin.DAFi.DAFi.defaultMinDim;
        int maxDim = com.flowjo.plugin.DAFi.DAFi.defaultMaxDim;
        int xDim = com.flowjo.plugin.DAFi.DAFi.defaultXDim;
        int yDim = com.flowjo.plugin.DAFi.DAFi.defaultYDim;
        try {
            minPopSize = Integer.parseInt(sParMinPopSize);
            minDim = Integer.parseInt(sParMinDim);
            maxDim = Integer.parseInt(sParMaxDim);
            xDim = Integer.parseInt(sParXDim);
            yDim = Integer.parseInt(sParYDim);
        } catch (Exception e) {}

        BufferedReader rTemplateReader = null;
        try {
            rTemplateReader = new BufferedReader(new InputStreamReader(scriptStream));
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }

        String scriptLine;
        try {
            while((scriptLine = rTemplateReader.readLine()) != null)
            {
                // Added to get runID in parameter - MVP
                //scriptLine = scriptLine.replace("FJ_PARENT_NAME", csvParentFileName);
                scriptLine = scriptLine.replace("FJ_PARM_SAMPLENAME", sampleName);
                scriptLine = scriptLine.replace("FJ_PARM_WSPDIR", wsDir);
                scriptLine = scriptLine.replace("FJ_PARM_WSPNAME", wsName);
                scriptLine = scriptLine.replace("FJ_PARM_NAME", parameterName);
                scriptLine = scriptLine.replace("FJ_DATA_FILE_PATH", dataFilePath);
                scriptLine = scriptLine.replace("FJ_CSV_OUPUT_FILE", outFileName);
                scriptLine = scriptLine.replace("FJ_GATING_ML_OUTPUT_FILE", gatingMLOutFile.getAbsolutePath());
                //scriptLine = scriptLine.replace("FJ_PAR_SCALE", sParScale);
                scriptLine = scriptLine.replace("FJ_TRANSFORM", sParTrans);
                scriptLine = scriptLine.replace("FJ_BATCH_MODE", sParBatch);
                scriptLine = scriptLine.replace("FJ_PAR_SOM", sParkMeansSom);
                scriptLine = scriptLine.replace("FJ_PAR_CHILDREN", sParApplyOnChildren);
                scriptLine = scriptLine.replace("FJ_PAR_MINPOPSIZE", sParMinPopSize);
                scriptLine = scriptLine.replace("FJ_MIN_N_PAR", sParMinDim);
                scriptLine = scriptLine.replace("FJ_MAX_N_PAR", sParMaxDim);
                scriptLine = scriptLine.replace("FJ_PAR_XDIM", sParXDim);
                scriptLine = scriptLine.replace("FJ_PAR_YDIM", sParYDim);
                scriptLine = scriptLine.replace("FJ_PAR_APPLY_ON_PREV", sParApplyOnPrev);
                scriptLine = scriptLine.replace("FJ_PAR_ADD_CELLIDS_TO_RESULT", sAddCellIdToResults);
                scriptLine = scriptLine.replace("FJ_POPULATION_NAME", populationName);
                scriptLine = scriptLine.replace("FJ_SAMPLE_NODE_NAME", sampleNodeName);
                scriptLine = scriptLine.replace("FJ_MILLIS_TIME", millisTimeString);
                scriptLine = scriptLine.replace("FJ_OUTPUT_FOLDER", outputFolderString);

                if(scriptLine.contains("FJ_PARAMS_LIST")) {
                    String parListStr = "";
                    for (String parName : parameterNames)
                    {
                        // We don't want the TIME parameter to be in the parameter list given to the R script
                        if(parName.compareToIgnoreCase("TIME") != 0) {
                            if(!parListStr.isEmpty()) parListStr = (new StringBuilder()).append(parListStr).append(",").toString();
                            parListStr = (new StringBuilder()).append(parListStr).append("\"").append(parName).append("\"").toString();
                        }
                    }
                    scriptLine = scriptLine.replaceAll("FJ_PARAMS_LIST", parListStr);
                }
                scriptWriter.append(scriptLine).append('\n');
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        if(rTemplateReader != null) {
            try { rTemplateReader.close(); }
            catch (Exception e) { e.printStackTrace(); }
        }

        return outFile;
    }

}
