package com.flowjo.plugin.ezDAFi;
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

import com.flowjo.plugin.ezDAFi.utils.FilenameUtils;

public class ezDAFiRFlowCalc extends RFlowCalculator {

    // The path to the ezDAFi R script template from within the jar file
    private final static String ezDAFiTemplatePath = "r/RScript.ezDAFi.Template.R";
    public static final String TRUE = "TRUE";
    public static final String FALSE = "FALSE";


    public File runezDAFi(String thisSampleURI, String wsName, String wsDir, File sampleFile, String sampleName, String populationName, String sampleNodeName, List<String> parameterNames, Map<String, String> options, String outputFolderPath, boolean useExistingFiles, long millisTime)
    {
        sampleName = sampleName.replaceAll(".ExtNode", "").replaceAll(".fcs", "").replaceAll(".LMD", "").trim();
        File outputFolder = new File(outputFolderPath);

        StringWriter scriptWriter = new StringWriter();
        File ezDAFiScript = createezDAFiscript(thisSampleURI, wsName, wsDir, sampleFile, sampleName, populationName, sampleNodeName, parameterNames, options, outputFolder, scriptWriter, millisTime);
        if(ezDAFiScript == null) return null;
        if(useExistingFiles && ezDAFiScript.exists()) return ezDAFiScript;

        String scriptFileName = (new StringBuilder()).append("RScript.ezDAFi.").append(millisTime).append(".R").toString().replaceAll(" ", "_");
        try
        {
            com.flowjo.plugin.ezDAFi.RScriptFlowCalculator calc = new com.flowjo.plugin.ezDAFi.RScriptFlowCalculator();
            fScriptFile = new File(outputFolderPath, scriptFileName);
            FileUtil.write(fScriptFile, scriptWriter.toString());
            calc.setRScriptMode(false);
            calc.executeRBatch(fScriptFile);
        }
        catch(Exception e)
        {
            e.printStackTrace();
        }
        return ezDAFiScript;
    }

    protected File createezDAFiscript(String thisSampleURI, String wsName, String wsDir, File sampleFile, String sampleName, String populationName, String sampleNodeName, List<String> parameterNames, Map<String, String> options, File outputFolder, StringWriter scriptWriter, long millisTime)
    {
        InputStream scriptStream = ezDAFiRFlowCalc.class.getResourceAsStream(ezDAFiTemplatePath);

        String outFileName = (new StringBuilder()).append(FilenameUtils.fixFileNamePart(sampleName)).append(".ezDAFi").append(".csv").toString();

        if(outputFolder == null) {
            String outputFolderMillisTime = sampleFile.getParentFile().getAbsolutePath() + File.separator + millisTime;
            outputFolder = new File(outputFolderMillisTime);
        }
        String outputFolderString = outputFolder.getAbsolutePath();

        File outFile = new File(outputFolder, outFileName);
        outFileName = outFile.getAbsolutePath();

        System.out.println("outFileName:" + outFileName);

        File gatingMLOutFile = new File(outFile, ".gating-ml2.xml");

        String dataFilePath = sampleFile.getAbsolutePath();

        String millisTimeString = Long.toString(millisTime);
        System.out.println("millisTimeString:" + millisTimeString);

        if(EngineManager.isWindows()) outFileName = outFileName.replaceAll("\\\\", "/");
        if(EngineManager.isWindows()) dataFilePath = dataFilePath.replaceAll("\\\\", "/");
        if(EngineManager.isWindows()) outputFolderString = outputFolderString.replaceAll("\\\\", "/");

        //String sParScale = options.get(com.flowjo.plugin.ezDAFi.ezDAFi.scaleOptionName);
        //String sParPlotStats = options.get(ezDAFi.plotStatsOptionName);
        //String sParTrans = options.get(com.flowjo.plugin.ezDAFi.ezDAFi.transOptionName);
        String sParBatch = options.get(ezDAFi.batchOptionName);
        String sParkMeansSom = options.get(ezDAFi.kMeansSomOptionName);
        String sParMulti = options.get(ezDAFi.multiOptionName);
        //String sParEzExp = options.get(ezDAFi.ezExpOptionName);
        String sParNaive = options.get(ezDAFi.naiveOptionName);
        //String sParPLS = options.get(ezDAFi.PLSOptionName);
        //String sParMeta = options.get(ezDAFi.metaOptionName);
        String sParApplyOnChildren = options.get(ezDAFi.applyOnChildrenOptionName);
        //String sParEzExpSeed = options.get(ezDAFi.ezExpSeedOptionName);
        String sParMinDim = options.get(ezDAFi.minDimOptionName);
        String sParMaxDim = options.get(ezDAFi.maxDimOptionName);
        String sParXDim = options.get(ezDAFi.xDimOptionName);
        String sParYDim = options.get(ezDAFi.yDimOptionName);
        String sParApplyOnPrev = options.get(ezDAFi.applyOnPrevOptionName);
        String sAddCellIdToResults = ezDAFi.isSeqGeq() ? TRUE : FALSE;
        //Added this to add runID to parameter
        String parameterName = ezDAFi.pluginName;

        //Integer sParEzExpInteger = Integer.parseInt(sParEzExp);
        //System.out.println("sParEzExpSeed:" + sParEzExpSeed);
        //System.out.println("sParEzExpSeed.getClass:" + sParEzExpSeed.getClass());


        //if (sParScale == null || sParScale.isEmpty() || com.flowjo.plugin.ezDAFi.ezDAFi.One.equals(sParScale) || com.flowjo.plugin.ezDAFi.ezDAFi.True.equals(sParScale))
//            sParScale = TRUE; // TRUE is the default
        //      else
        //sParScale = FALSE;

        //if (sParPlotStats == null || sParPlotStats.isEmpty() || ezDAFi.One.equals(sParPlotStats) || ezDAFi.True.equals(sParPlotStats))
        //  sParPlotStats = TRUE; // TRUE is the default
        //    else
        //sParPlotStats = FALSE;

        //if (sParTrans == null || sParTrans.isEmpty() || com.flowjo.plugin.ezDAFi.ezDAFi.One.equals(sParTrans) || com.flowjo.plugin.ezDAFi.ezDAFi.True.equals(sParTrans))
        //  sParTrans = TRUE; // TRUE is the default
        //else
        //  sParTrans = FALSE;

        if (sParBatch == null || sParBatch.isEmpty() || ezDAFi.One.equals(sParBatch) || ezDAFi.True.equals(sParBatch))
            sParBatch = TRUE; // TRUE is the default
        else
            sParBatch = FALSE;

        //if(sParEzExp == null || sParEzExp.isEmpty() || ezDAFi.Zero.equals(sParEzExp) || ezDAFi.False.equals(sParEzExp))
        //    sParEzExp = FALSE; // FALSE is the default
        //else
        //   sParEzExp = TRUE;

        if (sParkMeansSom == null || sParkMeansSom.isEmpty() || ezDAFi.One.equals(sParkMeansSom) || ezDAFi.True.equals(sParkMeansSom))
            sParkMeansSom = TRUE; // TRUE is the default
        else
            sParkMeansSom = FALSE;

        if (sParMulti == null || sParMulti.isEmpty() || ezDAFi.One.equals(sParMulti) || ezDAFi.True.equals(sParMulti))
            sParMulti = TRUE; // TRUE is the default
        else
            sParMulti = FALSE;

        //if (sParPLS == null || sParPLS.isEmpty() || ezDAFi.One.equals(sParPLS) || ezDAFi.True.equals(sParPLS))
        //  sParPLS = TRUE; // TRUE is the default
        //else
        //  sParPLS = FALSE;

        //if (sParMeta == null || sParMeta.isEmpty() || ezDAFi.One.equals(sParMeta) || ezDAFi.True.equals(sParMeta))
        //  sParMeta = TRUE; // TRUE is the default
        //else
        //  sParMeta = FALSE;

        if (sParApplyOnChildren == null || sParApplyOnChildren.isEmpty() || ezDAFi.One.equals(sParApplyOnChildren) || ezDAFi.True.equals(sParApplyOnChildren))
            sParApplyOnChildren = TRUE; // TRUE is the default
        else
            sParApplyOnChildren = FALSE;

        if (sParApplyOnPrev == null || sParApplyOnPrev.isEmpty())
            sParApplyOnPrev = ezDAFi.defaultApplyOnPrev;
        if (sParApplyOnPrev.length() > 5) // i.e., not "None"
            sParApplyOnPrev = outputFolder + File.separator + sParApplyOnPrev;
        if(EngineManager.isWindows()) sParApplyOnPrev = sParApplyOnPrev.replaceAll("\\\\", "/");

        if(EngineManager.isWindows()) wsDir = wsDir.replaceAll("\\\\", "/");
        if(EngineManager.isWindows()) wsName = wsName.replaceAll("\\\\", "/");
        if(EngineManager.isWindows()) thisSampleURI = thisSampleURI.replaceAll("\\\\", "/");

        //try {
        //  if ((Integer.parseInt(sParEzExpSeed) < 0) || (Integer.parseInt(sParEzExpSeed) > 999))
        //    sParEzExpSeed= Integer.toString(ezDAFi.defaultEzExpSeed);
        //} catch (Exception e) {
        //sParEzExpSeed = Integer.toString(ezDAFi.defaultEzExpSeed);
        //}

        try {
            if ((Integer.parseInt(sParMinDim) < 1) || (Integer.parseInt(sParMinDim) > 999999))
                sParMinDim= Integer.toString(ezDAFi.defaultMinDim);
        } catch (Exception e) {
            sParMinDim = Integer.toString(ezDAFi.defaultMinDim);
        }

        try {
          if ((Integer.parseInt(sParMaxDim) < 1) || (Integer.parseInt(sParMaxDim) > 999999))
              sParMaxDim= Integer.toString(ezDAFi.defaultMaxDim);
        } catch (Exception e) {
          sParMaxDim = Integer.toString(ezDAFi.defaultMaxDim);
        }

        try {
            if ((Integer.parseInt(sParXDim) < 3) || (Integer.parseInt(sParXDim) > 32))
                sParXDim = Integer.toString(ezDAFi.defaultXDim);
        } catch (Exception e) {
            sParXDim = Integer.toString(ezDAFi.defaultXDim);
        }

        try {
            if ((Integer.parseInt(sParYDim) < 3) || (Integer.parseInt(sParYDim) > 32))
                sParYDim = Integer.toString(ezDAFi.defaultYDim);
        } catch (Exception e) {
            sParYDim = Integer.toString(ezDAFi.defaultYDim);
        }

        //int ezExpSeed = ezDAFi.defaultEzExpSeed;
        //int minDim = ezDAFi.defaultMinDim;
        //int maxDim = ezDAFi.defaultMaxDim;
        //int xDim = ezDAFi.defaultXDim;
        //int yDim = ezDAFi.defaultYDim;
        //try {
        //ezExpSeed = Integer.parseInt(sParEzExpSeed);
        //  minDim = Integer.parseInt(sParMinDim);
        //  maxDim = Integer.parseInt(sParMaxDim);
        //  xDim = Integer.parseInt(sParXDim);
        //  yDim = Integer.parseInt(sParYDim);
        //} catch (Exception e) {}

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
                scriptLine = scriptLine.replace("FJ_PARM_SAMPLE_URI", thisSampleURI);
                scriptLine = scriptLine.replace("FJ_PARM_NAME", parameterName);
                scriptLine = scriptLine.replace("FJ_DATA_FILE_PATH", dataFilePath);
                scriptLine = scriptLine.replace("FJ_CSV_OUPUT_FILE", outFileName);
                scriptLine = scriptLine.replace("FJ_GATING_ML_OUTPUT_FILE", gatingMLOutFile.getAbsolutePath());
                //scriptLine = scriptLine.replace("FJ_PAR_SCALE", sParScale);
                //scriptLine = scriptLine.replace("FJ_PLOT_STATS", sParPlotStats);
                //scriptLine = scriptLine.replace("FJ_TRANSFORM", sParTrans);
                scriptLine = scriptLine.replace("FJ_BATCH_MODE", sParBatch);
                scriptLine = scriptLine.replace("FJ_PAR_SOM", sParkMeansSom);
                scriptLine = scriptLine.replace("FJ_PAR_MULTI", sParMulti);
                //scriptLine = scriptLine.replace("FJ_PAR_EX", sParEzExp);
                scriptLine = scriptLine.replace("FJ_PAR_NAIVE", sParNaive);
                //scriptLine = scriptLine.replace("FJ_PAR_PLSDA", sParPLS);
                //scriptLine = scriptLine.replace("FJ_PAR_META", sParMeta);
                scriptLine = scriptLine.replace("FJ_PAR_CHILDREN", sParApplyOnChildren);
                //scriptLine = scriptLine.replace("FJ_PAR_EZEXP_SEED", sParEzExpSeed);
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
