//////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2020 Pedro Milanez-Almeida, Ph.D., NIAID/NIH
//
// License
// The software is distributed under the terms of the
// Artistic License 2.0
// http://www.r-project.org/Licenses/Artistic-2.0
//
// Disclaimer
// This software and documentation come with no warranties of any kind.
// This software is provided "as is" and any express or implied
// warranties, including, but not limited to, the implied warranties of
// merchantability and fitness for a particular purpose are disclaimed.
// In no event shall the  copyright holder be liable for any direct,
// indirect, incidental, special, exemplary, or consequential damages
// (including but not limited to, procurement of substitute goods or
// services; loss of use, data or profits; or business interruption)
// however caused and on any theory of liability, whether in contract,
// strict liability, or tort arising in any way out of the use of this
// software.
//////////////////////////////////////////////////////////////////////////////
// Based on the FlowSOM plugin
//////////////////////////////////////////////////////////////////////////////


package com.flowjo.plugin.ezDAFi;

import com.flowjo.plugin.ezDAFi.utils.MakeLink;
import com.treestar.flowjo.application.workspace.Workspace;
import com.treestar.flowjo.application.workspace.manager.FJApplication;
import com.treestar.flowjo.application.workspace.manager.WSDocument;
import com.treestar.flowjo.core.Sample;
import com.treestar.flowjo.core.nodes.AppNode;
import com.treestar.flowjo.core.nodes.PopNode;
import com.treestar.flowjo.engine.FEML;
import com.treestar.flowjo.engine.utility.R_Algorithm;
import com.treestar.lib.FJPluginHelper;
import com.treestar.lib.PluginHelper;
import com.treestar.lib.core.ExportFileTypes;
import com.treestar.lib.core.ExternalAlgorithmResults;
import com.treestar.lib.data.StringUtil;
import com.treestar.lib.file.FJFileRef;
import com.treestar.lib.file.FJFileRefFactory;
import com.treestar.lib.fjml.FJML;
import com.treestar.lib.fjml.types.FileTypes;
import com.treestar.lib.gui.FJList;
import com.treestar.lib.gui.GuiFactory;
import com.treestar.lib.gui.HBox;
import com.treestar.lib.gui.numberfields.RangedIntegerTextField;
import com.treestar.lib.gui.panels.FJLabel;
import com.treestar.lib.gui.swing.FJCheckBox;
import com.treestar.lib.gui.swing.FJComboBox;
import com.treestar.lib.parsing.interpreter.CSVReader;
import com.treestar.lib.parsing.interpreter.ParseUtil;
import com.treestar.lib.xml.SElement;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.*;
import java.net.URL;
import java.util.List;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static com.flowjo.plugin.ezDAFi.RScriptFlowCalculator.fOutFile;
import static com.flowjo.plugin.ezDAFi.RScriptFlowCalculator.fOutFileLastLines;
import static com.flowjo.plugin.ezDAFi.utils.CustomDAFTiLayout.*;
import static com.flowjo.plugin.ezDAFi.utils.PopTraverser.scaryTraversal;
import static java.lang.System.currentTimeMillis;
import static java.lang.Thread.sleep;

public class ezDAFi extends R_Algorithm {

    private static final String pluginVersion = "0.5";
    public static String pluginName = "ezDAFi";
    public static boolean runAgain = false;
    public static boolean nameSet = false;
    public static String runID = "";

    public static final String One = "1";
    public static final String Zero = "0";
    public static final String True = "true";
    public static final String False = "false";
    public static final String cellIdParName = "CellId";


    private RangedIntegerTextField fDimXField = null, fDimYField = null;
    //private RangedIntegerTextField fEzExpSeedField = null;
    private RangedIntegerTextField fMinDimField = null;
    private RangedIntegerTextField fMaxDimField = null;
    private FJComboBox fApplyOnPrevCombo = null;
    //private FJCheckBox fScaleOptionCheckbox = null;
    //private FJCheckBox fPlotStatsOptionCheckbox = null;
    //private FJCheckBox fTransOptionCheckbox = null;
    private FJCheckBox fBatchOptionCheckbox = null;
    //private FJCheckBox fEzExpCheckbox = null;
    private FJCheckBox fNaiveCheckbox = null;
    private FJCheckBox fKMeansSomOptionCheckbox = null;
    private FJCheckBox fMultiOptionCheckbox = null;
    //private FJCheckBox fPLSOptionCheckbox = null;
    //private FJCheckBox fMetaOptionCheckbox = null;
    private FJCheckBox fApplyOnChildrenCheckbox = null;

    private FJCheckBox fSummaryOptionCheckbox = null;
    private FJCheckBox fOverlayCheckbox = null;

    private static final int fixedLabelWidth = 130;
    private static final int fixedFieldWidth = 75;
    private static final int fixedLabelHeigth = 25;
    private static final int fixedFieldHeigth = 25;
    private static final int fixedToolTipWidth = 300;
    private static final int fixedComboWidth = 150;
    private static final int hSpaceHeigth = 5;
    private SElement fsElement = null;
    private static final String space = " ";

    private static final String applyOnPrevLabel = "Apply on map";
    private static final String applyOnPrevTooltip = "If you have executed ezDAFi before, you can apply new data to a map generated by previous runs of ezDAFi. A new ezDAFi object will be created with the same grid but new mapping, node sizes and mean values.";
    private static final String dimXLabel = "SOM grid size (W x H)";
    private static final String dimXTooltip = "Width of the grid for building the self-organizing map.";
    private static final String dimYTooltip = "Height of the grid for building the self-organizing map.";
    //private static final String ezExpSeedLabel = "          Reproducible seed:";
    //private static final String ezExpSeedTooltip = "Used as seed generator for random numbers to ensure reproducibility. Compare results across several seeds to ensure results are stable.";
    private static final String minDimLabel = "# of dimensions: min";
    private static final String maxDimLabel = "Additional Clustering Dimensions:";
    //private static final String mustBeMinDimLabel = "(use min AND max = 1 for auto-selection)";
    private static final String minDimTooltip = "Min and max number of dimensions to apply ezDAFi on. High number of dimensions lead to high dimensional noise, low number of dimensions lead to no improvement over manual gate.";
    private static final String maxDimTooltip = "Expand your bi-dimensional gates with hidden dimensions. ezDAFi learns the most informative hidden dimensions from the data for every downstream gate. This number determines how many hidden dimensions to include in the dim-expanded gates.";

    private static final String orPerformezDAFiLabel = "or perform new ezDAFi.";
    //private static final String scaleLabel = "Scale parameters to mean = 0 and sd = 1 (use with care)";
    //private static final String scaleTooltip = "Should the data be scaled prior to clustering?";
    //private static final String plotStatsLabel = "Experimental: save plots and stats (can be slow).";
    //private static final String plotStatsTooltip = "Should side-by-side plots of manual and ezDAFi gates be automatically saved as well as frequency of parents and counts?";
    //private static final String transLabel = "Apply FJ data transformation.";
    //private static final String transTooltip = "If not working with raw FCS files but pre-processed CSV files from other applications such as CITE-seq or histo-cytometry, the data may already have been transformed and this box should be unchecked.";
    private static final String batchLabel = "Advanced (results not re-imported to FlowJo; batch mode).";
    private static final String batchTooltip = "ezDAFi all samples, plot back-gating results and continue analysis in R. Gates will not be re-imported to FlowJo.";
    //private static final String ezExpLabel = "Data exploration with ezExplorer.";
    //private static final String ezExpTooltip = "Help to guide manual gating. This function provides a quick overview of population structure with ultra-fast automated clustering, dimensionality reduction and suggested dimensions and hierarchy.";
    private static final String naiveLabel = "Use naive DAFi algorithm.";
    private static final String naiveTooltip = "Select this only if all dimensions should be used in clustering. Leave unchecked for automated dimension selection, which reduces the number of false positives and negatives.";
    private static final String kMeansSomLabel = "Cluster with self organizing maps (SOM; uncheck for k-means).";
    private static final String kMeansSomTooltip = "Which algorithm should be used for clustering?";
    private static final String multiLabel = "Use gene set-derived scores.";
    private static final String multiTooltip = "Should scores be derived from gene sets/gene expression data and used in clustering? Please provide gene sets as csv file (lists of gene ids in columns, first row  = gene set name) and normalized gene expression data as rds file containing a dgCMatrix (gene ids must match those in gene set csv files). Csv and rds files must be in the same folder and named exactly as the fcs files they correspond to, with the addition of '_genesets.csv' and '_RNA.rds' after '.fcs'.";
    //private static final String PLSLabel = "Experimental: cluster on PLS-DA latent variables.";
    //private static final String PLSTooltip = "Should the data be pre-processed with PLS-DA prior to clustering?";
    //private static final String metaLabel = "Experimental: meta-cluster centroids (can be slow).";
    //private static final String metaTooltip = "Should the centroids be meta-clustered prior to gating? Intended to stabilize gating results in the presence of technical variation.";
    private static final String applyOnChildrenLabel = "<html>Apply on children only."
            + "<br>(otherwise, recursive).";
    private static final String applyOnChildrenTooltip = "If checked, ezDAFi will refine only the children of the selected population. If unchecked, all children of children will be refined recursively (i.e., all sub-populations downstream of the selected one).";

    private static final String supportedByString = "This work was supported by the Intramural Research Program of NIAID, NIH.";

    private static final String summaryLabel = "Create a summary layout.";
    private static final String summaryTooltip = "Adds a layout of summary plots to the workspace.";
    private static final String overlayLabel = "Overlay ezDAFi population.";
    private static final String overlayTooltip = "Creates an overlay on manual gating, otherwise plots comparing manual gates to ezDAFi clusters sit side by side.";

    //public static final String scaleOptionName = "scale";
    //public static final String plotStatsOptionName = "plotStats";
    //public static final String transOptionName = "trans";
    public static final String batchOptionName = "batch";
    //public static final String ezExpOptionName = "ezExp";
    public static final String naiveOptionName = "naive";
    public static final String kMeansSomOptionName = "kMeansSom";
    public static final String multiOptionName = "multi";
    //public static final String PLSOptionName = "PLS";
    //public static final String metaOptionName = "spec";
    public static final String applyOnChildrenOptionName = "childrenOnly";
    public static final String xDimOptionName = "xdim";
    public static final String yDimOptionName = "ydim";
    //public static final String ezExpSeedOptionName = "ezExpSeed";
    public static final String minDimOptionName = "minDim";
    public static final String maxDimOptionName = "maxDim";
    public static final String applyOnPrevOptionName = "applyOn"; // "None" or file path to an RData file with a ezDAFi object
    public static final String pluginFolderAttName = "pluginFolder";
    public static final String sampleURISlot = "sampleURI";
    public static final String samplePopNodeSlot = "samplePopNode";
    public static final String sampleFileSlot = "sampleFile";

    public static final String summaryLayout = "summaryLayout";
    public static final String fOverlayString = "fOverlay";

    public static final String RDataFileExtension = ".RData";
    public static final String RDataFileSuffix = ".csv.ezDAFi.csv.RData";
    public static final String CSVwithParsFileSuffix = ".csv.ezDAFi.csv.pars.csv";

    public static final int defaultXDim = 10;
    public static final int defaultYDim = 10;
    //public static final int defaultEzExpSeed = 42;
    public static final int defaultMinDim = 1;
    public static final int defaultMaxDim = 3;
    public static final String defaultApplyOnPrev = "None";
    //public static final boolean defaultScale = false;
    public static final boolean defaultPlotStats = false;
    //public static final boolean defaultTrans = true;
    public static final boolean defaultBatch = false;
    //public static final boolean defaultEzExp = false;
    public static final boolean defaultNaive = false;
    public static final boolean defaultKMeansSom = true;
    public static final boolean defaultMulti = false;
    public static final boolean defaultPLS = false;
    public static final boolean defaultMeta = false;
    public static final boolean defaultApplyOnChildren = false;

    public static final boolean defaultSummaryLayout = true;
    public static final boolean defaultOverlay = false;

    //private boolean fScale = defaultScale;
    //private boolean fPlotStats = defaultPlotStats;
    //private boolean fTrans = defaultTrans;
    private boolean fBatch = defaultBatch;
    //private boolean fEzExp = defaultEzExp;
    private boolean fNaive = defaultNaive;
    private boolean fKMeansSom = defaultKMeansSom;
    private boolean fMulti = defaultMulti;
    //private boolean fPLS = defaultPLS;
    //private boolean fMeta = defaultMeta;
    private boolean fApplyOnChildren = defaultApplyOnChildren;

    private boolean summaryLayoutBool = defaultSummaryLayout;
    private boolean fOverlay = defaultOverlay;


    private int fndimx = defaultXDim, fndimy = defaultYDim;
    //private int fnEzExpSeed = defaultEzExpSeed;
    private int fnMinDim = defaultMinDim;
    private int fnMaxDim = defaultMaxDim;
    private String fAnalysisPathSampleURI = "test1";
    private String fAnalysisPathSamplePopNode = "test2";
    private String fAnalysisPathSampleFile = "test3";

    private static final String channelsLabelLine0 = "Make sure the selected population has at least one child gate.";
    private static final String channelsLabelLine1 = "";

    //private static final String channelsLabelLineEzExp0 = "The following options are meant only for data exploration (gates are not returned).";
    //private static final String channelsLabelLineEzExp1 = "";
    //private static final String channelsLabelLineEzExp2 = "";

    private static final String channelsLabelLineCITESeq0 = "The following option applies to CITE-Seq only.";
    private static final String channelsLabelLineCITESeq1 = "";

    //private static final String pathToScriptLabelLine1 = "The RScript created in this analysis is located in the same folder as";
    //private static final String pathToScriptLabelLine2 = "the FJ workspace, under /WORKSPACE_NAME/ezDAFi/RScript.'numbers'.R.txt,";
    //private static final String pathToScriptLabelLine3 = "where 'numbers' is time of creation in milliseconds.";
    //private static final String pathToScriptLabelLine4 = "";

    // Initial Publication:
    /**
     * Lee, Alexandra J., et al. "DAFi: A directed recursive data filtering and clustering
     * approach for improving and interpreting data clustering identification of cell populations
     * from polychromatic flow cytometry data." Cytometry Part A 93.6 (2018): 597-610.
     */

    private static final String citingLabelLine1 = "If using ezDAFi, please cite:";
    private static final String citingLabelLine2 = "Lee, Alexandra J., et al. \"DAFi: A directed recursive data filtering and clustering";
    private static final String citingLabelLine3 = "approach for improving and interpreting data clustering identification of cell populations";
    private static final String citingLabelLine4 = "from polychromatic flow cytometry data.\" Cytometry Part A 93.6 (2018): 597-610.";

    //    private static final String citation = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6030426";
    private static final String citation = "https://onlinelibrary.wiley.com/doi/pdf/10.1002/cyto.a.23371";    // Link redirects too much on Dan's machine... nope misunderstood - haha

    protected static final String sIconName = "images/ezDAFiIcon.png";

    private static Icon myIcon = null;
    private static final String Failed = "Failed";

    public ezDAFi() {
        super(pluginName);
    }

    public String getVersion() {
        return (pluginVersion);
    }

    @Override
    public String getName() {
        return pluginName;
    }

    @Override
    public Icon getIcon() {
        if (myIcon == null) {
            URL url = ezDAFi.class.getResource(getIconName());
            if (url != null)
                myIcon = new ImageIcon(url);
        }
        return myIcon;
    }

    @Override
    protected String getIconName() {
        return sIconName;
    }

    @Override
    protected boolean showClusterInputField() {
        return false;
    }

    @Override
    public ExportFileTypes useExportFileType() {
        return ExportFileTypes.CSV_CHANNEL;
    }

    public ExternalAlgorithmResults invokeAlgorithm(SElement fcmlQueryElement, File sampleFile, File outputFolder) {
        ExternalAlgorithmResults results = new ExternalAlgorithmResults();

        String savedSampleURI = fOptions.get(sampleURISlot);
        String savedSamplePopNode = fOptions.get(samplePopNodeSlot);

        System.out.println("savedSampleURI: " + savedSampleURI);
        System.out.println("savedSamplePopNode: " + savedSamplePopNode);

        String thisSampleURI = FJPluginHelper.getSampleURI(fcmlQueryElement);
        String thisSamplePopNode;
        try{
            thisSamplePopNode = FJPluginHelper.getParentPopNode(fcmlQueryElement).getName();
        } catch (Exception e) {
            thisSamplePopNode = "__pluginCalledOnRoot__";
        }

        //String thisSamplePopNode = FJPluginHelper.getParentPopNode(fcmlQueryElement).getName();

        System.out.println("thisSampleURI: " + thisSampleURI);
        System.out.println("thisSamplePopNode: " + thisSamplePopNode);

        boolean checkPrevRun = savedSamplePopNode.equals(thisSamplePopNode) && !savedSampleURI.equals(thisSampleURI);

        System.out.println("checkPrevRun: " + checkPrevRun);

        System.out.println("runAgain: " + runAgain);

        if(checkPrevRun){
            runAgain = true;
        }

        System.out.println("runAgain: " + runAgain);

        // RunAgain is set to true when the user double clicks on the plugin node, this avoids the recalculation on update.
        if (!runAgain) {
            return results;
        }
        // If the plugin fails, we need to avoid the recalculation as it might not get fixed anyways.
        runAgain = false;

        System.out.println("!sampleFile.exists(): " + !sampleFile.exists());

        // trying to separate each run using time in millisecond
        // in case you want to add time to separate between runs
        long millisTime = currentTimeMillis();

        System.out.println("millisTime: " + millisTime);
        String outputFolderMillisTime = outputFolder.getAbsolutePath() + File.separator + millisTime;
        System.out.println("outputFolderMillisTime: " + outputFolderMillisTime);
        outputFolder = new File(outputFolderMillisTime);
        new File(outputFolderMillisTime).mkdirs();

        if (!sampleFile.exists()) {
            // results.setErrorMessage("Input file did not exist"); // We purposely don't want to set the error as there may be undesirable side-effects
            JOptionPane.showMessageDialog(null, "Input file did not exist", "ezDAFi error", JOptionPane.ERROR_MESSAGE);
            results.setWorkspaceString(ezDAFi.Failed);
            return results;
        } else {
            // Let's force recalculation all the time because it's relatively quick and we don't seem to handle
            // checkUseExistingFiles well (i.e., if input settings change a bit, we still tend to return previous
            // results instead of recalculating
            // checkUseExistingFiles(fcmlQueryElement);
            fUseExistingFiles = false;

            fOptions.put(sampleFileSlot, sampleFile.getAbsolutePath());
            fOptions.put(sampleURISlot, thisSampleURI);
            fOptions.put(samplePopNodeSlot, thisSamplePopNode);


            //save workspace before running plugin
            Sample sample = FJPluginHelper.getSample(fcmlQueryElement);
            Workspace workspace = sample.getWorkspace();
            WSDocument wsd = workspace.getDoc();
            //wsd.save();


            //get workspace directory and path to enable working with acs files
            String wsDir = wsd.getWorkspaceDirectory().getAbsolutePath();
            String wsName = wsd.getFilename();

            //Create variable with names of parameters
            List<String> parameterNames = preprocessCompParameterNames();

            //get sample name
            String sampleName = sampleFile.getName();

            // Get gate name and the parent popnode
            //PopNode popNode = FJPluginHelper.getParentPopNode(fcmlQueryElement);
            //PopNode parentPopNode = popNode.getParentPop();
            //if (parentPopNode == null) { // This means the current parent node is the root sample, if it is just take the sample node.
            //    parentPopNode = sample.getSampleNode();
            //}

            // tried to add ability to run ezDAFi on parent of selected pop: FAILED (see line 283)
            //List params = new ArrayList();
            //params.add("EventNumberDP");

            //File csvParentFile = ExportUtils.exportParameters(parentPopNode, params, outputFolder, sampleName);
            //String csvParentFileName = sampleName + ".PARENT" + FJSML.FORMATS.FILE.CSV.EXTENSION;

            //Get name of .FCS file
            PopNode sampleNode = sample.getSampleNode();

            System.out.println("sampleName: " + sampleName);

            ezDAFiRFlowCalc calculator = new ezDAFiRFlowCalc();
            // Added the population node
            File ezDAFiResult = calculator.runezDAFi(thisSampleURI, wsName, wsDir, sampleFile, sampleName, thisSamplePopNode, sampleNode.getName(), parameterNames, fOptions, outputFolder.getAbsolutePath(), useExistingFiles(), millisTime);
//            calculator.deleteScriptFile();
            checkROutFile(calculator);

            boolean invFlag = false;

            //This is a workaround for the bug that FlowJo is not showing errors in R:
            //Try to read the results (import derived parameters and gatingML files), and, if requested, print the Rscript.
            //If this fails, print only the last 30 lines of the Rscript to make the error visible to the user.

            //String sParEzExp = fOptions.get(ezExpOptionName);

            //System.out.println("sParEzExp: " + sParEzExp);

            //if (sParEzExp == null || sParEzExp.isEmpty() || ezDAFi.One.equals(sParEzExp) || ezDAFi.True.equals(sParEzExp)){
            //  fEzExp = true;
            //} else {
            //  fEzExp = false;
            //}

            //System.out.println("fEzExp: " + fEzExp);

            try {

                //if (fEzExp) {
                    //String sParEzExpSeed = fOptions.get(ezExpSeedOptionName);
                    //System.out.println("sParEzExpSeed: " + sParEzExpSeed);
                    //try {
                        //String ezExpPDF = "ezExplorer." + thisSamplePopNode + ".seed_" + sParEzExpSeed + ".pdf";

                        //System.out.println("ezExpPDF: " + ezExpPDF);

                        //File ezExpPDFFile = new File(wsDir, ezExpPDF);
                        //Desktop.getDesktop().open(ezExpPDFFile.getParentFile());
                        //Desktop.getDesktop().open(ezExpPDFFile);
                        //} catch (IOException e) {
                        //    e.printStackTrace();
                        //}

                    //} else {


                    mergeCSVFile(fcmlQueryElement, results, ezDAFiResult, sampleFile, outputFolder);

                    System.out.println(ezDAFiResult.getAbsolutePath());
                    List<Float> values = extractUniqueValuesForParameter(ezDAFiResult);

                    //create a string which represents the gating-ml file
                    String xmlEnding = sampleFile.getName() + ".gating-ml2.xml";

                    //create a filter to find the gating-ml file in the folder
                    FilenameFilter xmlFileFilter = (dir, name) -> name.endsWith(xmlEnding);

                    File[] xmlFiles = outputFolder.listFiles(xmlFileFilter);

                    for (File xmlFile : xmlFiles) {
                        String gatingML = readGatingMLFile(xmlFile);
                        results.setGatingML(gatingML);
                        invFlag = true;
                    }

                    //}

            } catch (Exception error) {
                try{
                    BufferedWriter writer = new BufferedWriter(new FileWriter(fOutFileLastLines));
                    writer.write(tail2(fOutFile, 10));
                    writer.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }

                try {
                    Desktop.getDesktop().open(fOutFileLastLines);
                } catch (IOException e) {
                    e.printStackTrace();
                }

            }

            // Rename populations to clean up the full gating path there.
            if (invFlag) {

                ExecutorService threadPool = Executors.newFixedThreadPool(1);

                PopNode theNode = FJPluginHelper.getParentPopNode(fcmlQueryElement) == null ? sample.getSampleNode() : FJPluginHelper
                        .getParentPopNode(fcmlQueryElement);

                invFlag = false;
                final HashMap<AppNode, Integer>[] popHash = new HashMap[]{new HashMap<>()};

                try {
                    threadPool.submit(new Runnable() {
                        public void run() {

                            // wish we didn't need to do this, but want to try and be sure the gatingML is in place.
                            try {
                                sleep(7000);
                            } catch (InterruptedException e) {
                                e.printStackTrace();
                            }

                            popHash[0] = scaryTraversal(theNode, popHash[0], fcmlQueryElement);

                            // DONE List:
                            //      - Create "geneset library" from top features for each population within R
                            //      - Add class for dealing with layouts
                            //      - Impliment that crafted output
                            //      - Give flag for summary layout creation
                            //      - Add information from geneset libraries to annotations in LE
                            //      - Get recording and add it to documentation

                            // TODO List:
                            //      - Give option to not overlay in summary Layout


                            boolean bLayout = false;
                            String sLayout = fOptions.get(summaryLayout);
                            if (sLayout != null && !sLayout.isEmpty())
                                bLayout = One.equals(sLayout) || True.equals(sLayout);

                            boolean lOverlay = false;
                            String sOverlay = fOptions.get(fOverlayString);
                            if (sLayout != null && !sLayout.isEmpty())
                                lOverlay = One.equals(sOverlay) || True.equals(sOverlay);


                            if(bLayout) {
                                try {
                                    sleep(500);
                                } catch (InterruptedException e) {
                                    e.printStackTrace();
                                }

                                try {

                                    if(lOverlay) {
                                        createOverlay(popHash[0], sample, "ezDAFi Summary " + sample.getName(),
                                                30, fcmlQueryElement, true, fOutFile.getParentFile());

                                        formatLegend(sample);
                                    } else {
                                        createNoOverlay(popHash[0], sample, "ezDAFi Summary " + sample.getName(),
                                                30, fcmlQueryElement, true, fOutFile.getParentFile());
                                    }

                                } catch (Exception ee) {
                                    ee.printStackTrace();
                                }
                            }
                        }
                    });
                } catch (Exception e) {
                    System.out.println(e);
                }
            }
            return results;
        }
    }

    private void mergeCSVFile(SElement fcmlQueryElement, ExternalAlgorithmResults results, File umapResults, File sampleFile, File outputFolder) {
        File restFile = null;
        try {
            restFile = generateDerivedParameterCSVFile2(fcmlQueryElement, results, umapResults, sampleFile, getName(), outputFolder.getAbsolutePath(), 0.0);
        } catch (IOException ex) {
        }
        FJFileRef csvFile = null;
        try {
            csvFile = FJFileRefFactory.make(restFile.getAbsolutePath());
        } catch (IOException ex) {
        }

        int numExport = FJPluginHelper.getNumTotalEvents(fcmlQueryElement);
        //String sampleName = FJPluginHelper.getSampleName(fcmlQueryElement);

        FJFileRef retFile = null;

        // Extract the hierarchical gating path from <ExternalPopNode>
        SElement externalPopNodeElement = PluginHelper.getExternalPopNodeElement(fcmlQueryElement);
        String pathString = externalPopNodeElement.getString(FJML.path);
        List<String> paths = StringUtil.stringToPath(pathString);
        paths = new ArrayList<String>(paths);
        if (paths.size() > 1) // remove last name in the path, it's the plugin node
        {
            paths.remove(paths.size() - 1);
        }
        pathString = space;
        for (String p : paths) {
            pathString += p + ".";
        }

        String sampleName = FJPluginHelper.getSampleName(fcmlQueryElement);
        if (sampleName.endsWith(FileTypes.FCS_SUFFIX)) {
            sampleName = sampleName.substring(0, sampleName.length() - FileTypes.FCS_SUFFIX.length());
        }
        if (sampleName.endsWith(FileTypes.CSV_SUFFIX) || sampleName.endsWith(FileTypes.TXT_SUFFIX)) {
            sampleName = sampleName.substring(0, sampleName.length() - FileTypes.CSV_SUFFIX.length());
        }
        String outFileName = pathString + sampleName + ".EPA.2" + FileTypes.CSV_SUFFIX;

        try {
            retFile = generateDerivedParameterCSVFile(csvFile, numExport, outFileName, outputFolder.getAbsolutePath());
        } catch (IOException ex) {
        }
        PluginHelper.createClusterParameter(results, pluginName, retFile.getLocalFile());
    }


    public FJFileRef generateDerivedParameterCSVFile(FJFileRef inputCSVFileRef, int numEvents, String popName, String outputFolder) throws IOException {
        File inputCSVFile = inputCSVFileRef.getLocalFile();
        if (!inputCSVFile.exists()) // return early if input file does not exist
            return null;

        BufferedReader inputCSVFileReader = new BufferedReader(new FileReader(inputCSVFile));
        // read the first header line of the input CSV sample file
        String csvLine = inputCSVFileReader.readLine();
        // determine which column is the event number column
        int eventNumColumnIndex = -1; // the column index of the event number column
        int colCt = 0;
        String headerLine = ""; // the column header to write in the new file
        String noEventLine = ""; // the line to write when there is no derived parameter value
        StringTokenizer tokenizer = new StringTokenizer(csvLine, ",");
        while (tokenizer.hasMoreTokens()) {
            String token = tokenizer.nextToken();
            if (token.contains(FJML.EventNumberDP))
                eventNumColumnIndex = colCt;
            else if (eventNumColumnIndex >= 0) {
                headerLine += token + ",";
                noEventLine += "0,";
            }
            colCt++;
        }
        if (headerLine.endsWith(",")) // get rid of trailing comma of header line
            headerLine = headerLine.substring(0, headerLine.length() - 1);
        headerLine += "\n";
        if (noEventLine.endsWith(",")) // get rid of trailing comma of no parameter value line
            noEventLine = noEventLine.substring(0, noEventLine.length() - 1);
        noEventLine += "\n";

        File outFile = new File(outputFolder, popName + FEML.EPA_Suffix);

        System.out.println("outFile generateDerivedParameterCSVFile: " + outFile);

        Writer output = new BufferedWriter(new FileWriter(outFile));
        if (true)
            output.write(headerLine);
        int ct = 0;
        int eventNum = 0;
        String separator = colCt == 2 ? "" : ","; // if only 2 columns, one is event number, other is single column, so don't need the trailing comma
        while ((csvLine = inputCSVFileReader.readLine()) != null) {
            tokenizer = new StringTokenizer(csvLine, ",");
            colCt = 0;
            String line = "";
            while (tokenizer.hasMoreTokens()) {
                String token = tokenizer.nextToken();
                if (colCt == eventNumColumnIndex) // get the event number as integer
                    eventNum = (int) ParseUtil.getDouble(token);
                else if (colCt > eventNumColumnIndex) // it's a column after the event number, make it a derived parameter value
                    line += token + separator;
                colCt++;
            }
            if (eventNum < 0)
                break;
            while (ct < eventNum - 1 && ct < numEvents) {
                output.write(noEventLine);
                ct++;
            }
            output.write(line);
            output.write("\n");
            ct++;
        }
        while (ct < numEvents) {
            output.write(noEventLine);
            ct++;
        }
        inputCSVFileReader.close();
        output.close();

        FJFileRef fjFileRef = null;
        try {
            fjFileRef = FJFileRefFactory.make(outFile.getAbsolutePath());
        } catch (IOException e) {
            e.printStackTrace();
        }
        return fjFileRef;
    }

    private String readGatingMLFile(File file){
        CSVReader csvReader = null;
        try {
            csvReader = new CSVReader(new FileReader(file));
            String gatingML = "";
            List<String[]> data = csvReader.readAll();
            for (String[] row : data){
                for (int i = 0; i < row.length; i++) {
                    gatingML+= row[i];
                }
                gatingML+=" ";
            }
            return gatingML;
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return "";
    }

    private static File generateDerivedParameterCSVFile2(SElement fcmlElem, ExternalAlgorithmResults algorithmResults, File pluginCSVFile, File sampleCSVFile, String pluginName, String outputFolder, double noVal) throws IOException {
        if (!pluginCSVFile.exists() || !sampleCSVFile.exists()) // return early if input files do not exist
        {
            return null;
        }
        if (sampleCSVFile.getName().endsWith(FileTypes.FCS_SUFFIX)) {
            algorithmResults.setErrorMessage("The Population Plugin must specify CSV for the useExportType to create cluster parameters.");
            return null;
        }
        SElement externalPopNodeElement = PluginHelper.getExternalPopNodeElement(fcmlElem);
        if (externalPopNodeElement == null) // if no <ExternalPopNode> element, something's wrong
        {
            return null;
        }
        int numEvents = PluginHelper.getNumTotalEvents(fcmlElem);
        BufferedReader sampleCSVFileReader = new BufferedReader(new FileReader(sampleCSVFile));
        // read the first header line of the input sample CSV file
        String sampleCSVLine = sampleCSVFileReader.readLine();
        // determine which column is the event number column
        int eventNumColumnIndex = -1; // the column index of the event number column
        int colCt = 0;
        StringTokenizer tokenizer = new StringTokenizer(sampleCSVLine, ",");
        while (tokenizer.hasMoreTokens()) {
            String token = tokenizer.nextToken();
            if (token.contains(FJML.EventNumberDP)) {
                eventNumColumnIndex = colCt;
            }
            colCt++;
        }
        // now determine number of columns in input plugin csv file
        BufferedReader pluginCSVFileReader = new BufferedReader(new FileReader(pluginCSVFile));
        String pluginCSVLine = pluginCSVFileReader.readLine();
        colCt = 0;
        tokenizer = new StringTokenizer(pluginCSVLine, ",");
        String noEventLine = space + noVal + ","; // the line to write when there is no derived parameter value
        while (tokenizer.hasMoreTokens()) {
            noEventLine += noVal + ",";
            tokenizer.nextToken();
            colCt++;
        }
        if (noEventLine.endsWith(",")) // get rid of trailing comma of no parameter value line
        {
            noEventLine = noEventLine.substring(0, noEventLine.length() - 1);
        }
        noEventLine += "\n";

        // Extract the hierarchical gating path from <ExternalPopNode>
        String pathString = externalPopNodeElement.getString(FJML.path);
        List<String> paths = StringUtil.stringToPath(pathString);
        paths = new ArrayList<String>(paths);
        if (paths.size() > 1) // remove last name in the path, it's the plugin node
        {
            paths.remove(paths.size() - 1);
        }
        pathString = space;
        for (String p : paths) {
            pathString += p + ".";
        }

        String sampleName = FJPluginHelper.getSampleName(fcmlElem);
        if (sampleName.endsWith(FileTypes.FCS_SUFFIX)) {
            sampleName = sampleName.substring(0, sampleName.length() - FileTypes.FCS_SUFFIX.length());
        }
        if (sampleName.endsWith(FileTypes.CSV_SUFFIX) || sampleName.endsWith(FileTypes.TXT_SUFFIX)) {
            sampleName = sampleName.substring(0, sampleName.length() - FileTypes.CSV_SUFFIX.length());
        }
        String outFileName = pathString + sampleName + ".EPA" + FileTypes.CSV_SUFFIX;

        System.out.println("outFileName generateDerivedParameterCSVFile2: " + outFileName);

        File outFile = new File(outputFolder, outFileName);
        Writer output = new BufferedWriter(new FileWriter(outFile));
        int ct = 0;
        int eventNum = 0;
        output.write(FJML.EventNumberDP + "," + pluginCSVLine + "\n");

        // reading sample and plugin CSV file in parallel
        while ((sampleCSVLine = sampleCSVFileReader.readLine()) != null && (pluginCSVLine = pluginCSVFileReader.readLine()) != null) {
            // get event number to write in first column
            tokenizer = new StringTokenizer(sampleCSVLine, ",");
            colCt = 0;
            String line = space;
            while (tokenizer.hasMoreTokens()) {
                String token = tokenizer.nextToken();
                if (colCt == eventNumColumnIndex) // get the event number as integer
                {
                    eventNum = (int) ParseUtil.getDouble(token);
                    line += eventNum + ",";
                }
                colCt++;
            }
            if (eventNum < 0) {
                break;
            }
            while (ct < eventNum - 1 && ct < numEvents) {
                output.write(noEventLine);
                ct++;
            }
            line += pluginCSVLine;
            output.write(line);
            output.write("\n");
            ct++;
        }
        while (ct < numEvents) {
            output.write(noEventLine);
            ct++;
        }
        pluginCSVFileReader.close();
        sampleCSVFileReader.close();
        output.close();
        return outFile;
    }


    private void addTableFromCSVToResults(ExternalAlgorithmResults results, File csvFile) {
        if (csvFile.exists()) {
            Scanner scan;
            try {
                int nCols = 0;
                scan = new Scanner(csvFile);
                if (scan.hasNextLine()) {
                    String line = scan.nextLine();
                    String[] values = line.split(",");
                    if (values != null) {
                        nCols = values.length;
                        for (int i = 0; i < nCols; i++)
                            values[i] = values[i].replaceAll("^\"|\"$", "").replaceAll("<", "(").replaceAll(">", ")");
                        results.setTableHeaders(values);
                    }
                }
                ArrayList<double[]> listOfLines = new ArrayList<double[]>();
                if (nCols > 0) {
                    while (scan.hasNextLine()) {
                        String line = scan.nextLine();
                        String[] values = line.split(",");
                        if (values != null && values.length == nCols) { // Only process lines with the correct number of columns
                            double dV[] = new double[nCols];
                            for (int i = 0; i < values.length; i++) {
                                dV[i] = Double.parseDouble(values[i].replaceAll("^\"|\"$", ""));
                            }
                            listOfLines.add(dV);
                        }
                    }
                    double[][] v = new double[listOfLines.size()][nCols];
                    for (int i = 0; i < listOfLines.size(); i++) {
                        for (int j = 0; j < nCols; j++) {
                            v[i][j] = listOfLines.get(i)[j];
                        }
                    }
                    results.setValuesTable(v);
                }


            } catch (FileNotFoundException e) {
            }
        }
    }

    @Override
    protected List<Component> getPromptComponents(SElement fcmlElem, SElement algorithmElement, List<String> parameterNames) {
        // we need to save the sample the plugin was applied on. this is to trigger re-run only if rerunning on diff sample and pop
        String sampleURI = FJPluginHelper.getSampleURI(fcmlElem);
        if (sampleURI != null) {
            try{
                algorithmElement.setAttribute(sampleURISlot, sampleURI);
            } catch (Exception e) {
            }
        }

        System.out.println("sampleURI: " + sampleURI);

        String samplePopNode;
        try{
            samplePopNode = FJPluginHelper.getParentPopNode(fcmlElem).getName();
        } catch (Exception e) {
            samplePopNode = "__pluginCalledOnRoot__";
        }

        //String samplePopNode = FJPluginHelper.getParentPopNode(fcmlElem).getName();
        if (samplePopNode != null) {
            try{
                algorithmElement.setAttribute(samplePopNodeSlot, samplePopNode);
            } catch (Exception e) {
            }
        }

        System.out.println("samplePopNode: " + samplePopNode);

        // We need the plugin output folder and we want to add that to the algorithmElement so that later on, we can scan that folder for any existing .RData files.
        // Unfortunately, FJPluginHelper.getPluginOutputFolder(fcmlElem, this) seems to be returning null sometimes,
        // i.e., this may be called before the plugin output folder is set. Therefore, this is a work around:
        //Sample sample = FJPluginHelper.getSample(fcmlElem);
        //if (sample != null) {
        //    Workspace ws = sample.getWorkspace();
        //  if (ws != null) {
        //      try {
        //          WSDocument wsd = ws.getDoc();
        //          wsd.save(); //save workspace before running plugin
        //          String wsDir = wsd.getWorkspaceDirectory().getAbsolutePath();
        //          String wsName = wsd.getFilename();
        //          String outputFolder = wsDir + File.separator + wsName.substring(0, wsName.lastIndexOf('.')) + File.separator + this.getName() + File.separator + millisTime;
        //          File pluginFolder = new File(outputFolder);
        //          if (pluginFolder.exists())
        //              algorithmElement.setAttribute(pluginFolderAttName, outputFolder);
        //      } catch (Exception e) {
        //      }
        //      ;
        //  }
        //}
        fsElement = fcmlElem;
        List<Component> ret = super.getPromptComponents(fcmlElem, algorithmElement, parameterNames);

        runAgain = true;
        return ret;
    }

    @SuppressWarnings("unchecked")
    @Override
    protected List<Component> getPromptComponents(SElement selement, List<String> list) {
        ArrayList<Component> componentList = new ArrayList<>();

        FJLabel fjLabel0 = new FJLabel(channelsLabelLine0);
        FJLabel fjLabel1 = new FJLabel(channelsLabelLine1);

        //FJLabel fjLabelEzExp0 = new FJLabel(channelsLabelLineEzExp0);
        //FJLabel fjLabelEzExp1 = new FJLabel(channelsLabelLineEzExp1);
        //FJLabel fjLabelEzExp2 = new FJLabel(channelsLabelLineEzExp2);

        FJLabel fjLabelCITESeq0 = new FJLabel(channelsLabelLineCITESeq0);
        FJLabel fjLabelCITESeq1 = new FJLabel(channelsLabelLineCITESeq1);

        componentList.add(fjLabel0);
        componentList.add(fjLabel1);

        componentList.add(addFlowJoParameterSelector(list));

        fsElement = selement;

        fApplyOnPrevCombo = new FJComboBox(new String[]{defaultApplyOnPrev});
        String pluginFolder = selement.getString(pluginFolderAttName);
        if (pluginFolder != null && !pluginFolder.isEmpty()) {
            File myDir = new File(pluginFolder);
            if (myDir.exists()) {
                File[] existingRDataFiles = myDir.listFiles(new FilenameFilter() {
                    public boolean accept(File dir, String name) {
                        return name.endsWith(ezDAFi.RDataFileExtension);
                    }
                });
                for (File rDataFile : existingRDataFiles) {
                    String rDataName = rDataFile.getName();
                    if (rDataName.contains(ezDAFi.RDataFileSuffix))
                        rDataName = rDataName.substring(0, rDataName.lastIndexOf(ezDAFi.RDataFileSuffix));
                    fApplyOnPrevCombo.addItem(rDataName);
                }
            }
        }

        String sampleURI = selement.getString(sampleURISlot);
        String samplePopNode = selement.getString(samplePopNodeSlot);
        String sampleFile = "";

        // Default parameter values
        fAnalysisPathSampleURI = sampleURI;
        fAnalysisPathSamplePopNode = samplePopNode;
        fAnalysisPathSampleFile = sampleFile;
        fndimx = defaultXDim;
        fndimy = defaultYDim;
        //fScale = defaultScale;
        //fPlotStats = defaultPlotStats;
        //fTrans = defaultTrans;
        fBatch = defaultBatch;
        //fEzExp = defaultEzExp;
        fNaive = defaultNaive;
        fKMeansSom = defaultKMeansSom;
        fMulti = defaultMulti;
        //fPLS = defaultPLS;
        //fMeta = defaultMeta;
        fApplyOnChildren = defaultApplyOnChildren;
        //fnEzExpSeed = defaultEzExpSeed;
        fnMinDim = defaultMinDim;
        fnMaxDim = defaultMaxDim;

        summaryLayoutBool = defaultSummaryLayout;
        fOverlay = defaultOverlay;

        // If there are option set already (e.g., from the workspace), then
        // let's retrieve those and use them instead of defaults.
        Iterator<SElement> iterator = selement.getChildren("Option").iterator();
        int savedViewOptionIndex = 0;
        int savedPlotOptionIndex = 0;

        while (iterator.hasNext()) {
            SElement option = iterator.next();

            int savedDimx = option.getInt(xDimOptionName, -1);
            if (savedDimx >= 1 && savedDimx <= 100) fndimx = savedDimx;

            int savedDimy = option.getInt(yDimOptionName, -1);
            if (savedDimy >= 1 && savedDimy <= 100) fndimy = savedDimy;

            int nApplyOnPrevComboItemsCount = fApplyOnPrevCombo.getItemCount();
            String savedApplyOnPrevOption = option.getString(applyOnPrevOptionName);
            if (savedApplyOnPrevOption != null && savedApplyOnPrevOption.length() > 5)
                for (int j = 0; j < nApplyOnPrevComboItemsCount; j++) {
                    String itemValue = (String) fApplyOnPrevCombo.getItemAt(j);
                    if (savedApplyOnPrevOption.startsWith(itemValue))
                        fApplyOnPrevCombo.setSelectedIndex(j);
                }

            String overlayString = option.getAttributeValue(fOverlayString);
            if (overlayString != null && !overlayString.isEmpty())
                fOverlay = One.equals(overlayString) || True.equals(overlayString);

            String sLayout = option.getAttributeValue(summaryLayout);
            if (sLayout != null && !sLayout.isEmpty())
                summaryLayoutBool = One.equals(sLayout) || True.equals(sLayout);

            String savedApplyOnChildren = option.getAttributeValue(applyOnChildrenOptionName);
            if (savedApplyOnChildren != null && !savedApplyOnChildren.isEmpty())
                fApplyOnChildren = One.equals(savedApplyOnChildren) || True.equals(savedApplyOnChildren);

            String savedKMeansSom = option.getAttributeValue(kMeansSomOptionName);
            if (savedKMeansSom != null && !savedKMeansSom.isEmpty())
                fKMeansSom = One.equals(savedKMeansSom) || True.equals(savedKMeansSom);

            String savedMulti = option.getAttributeValue(multiOptionName);
            if (savedMulti != null && !savedMulti.isEmpty())
                fMulti = One.equals(savedMulti) || True.equals(savedMulti);

            //String savedPLS = option.getAttributeValue(PLSOptionName);
            //if (savedPLS != null && !savedPLS.isEmpty())
            //  fPLS = One.equals(savedPLS) || True.equals(savedPLS);

            //String savedMeta = option.getAttributeValue(metaOptionName);
            //if (savedMeta != null && !savedMeta.isEmpty())
            //  fMeta = One.equals(savedMeta) || True.equals(savedMeta);

            //String savedScale = option.getAttributeValue(scaleOptionName);
            //if (savedScale != null && !savedScale.isEmpty())
//                fScale = One.equals(savedScale) || True.equals(savedScale);

            //String savedPlotStats = option.getAttributeValue(plotStatsOptionName);
            //if (savedPlotStats != null && !savedPlotStats.isEmpty())
            //  fPlotStats = One.equals(savedPlotStats) || True.equals(savedPlotStats);

            //String savedTrans = option.getAttributeValue(transOptionName);
            //if (savedTrans != null && !savedTrans.isEmpty())
            //  fTrans = One.equals(savedTrans) || True.equals(savedTrans);

            String savedBatch = option.getAttributeValue(batchOptionName);
            if (savedBatch != null && !savedBatch.isEmpty())
                fBatch = One.equals(savedBatch) || True.equals(savedBatch);

            //String savedEzExp = option.getAttributeValue(ezExpOptionName);
            //if (savedEzExp != null && !savedEzExp.isEmpty())
            //  fEzExp = One.equals(savedEzExp) || True.equals(savedEzExp);

            String savedNaive = option.getAttributeValue(naiveOptionName);
            if (savedNaive != null && !savedNaive.isEmpty())
                fNaive = One.equals(savedNaive) || True.equals(savedNaive);

            //int savedEzExpSeed = option.getInt(ezExpSeedOptionName, -1);
            //if (savedEzExpSeed >= 0 && savedEzExpSeed <= 999) fnEzExpSeed = savedEzExpSeed;

            int savedMinDim = option.getInt(minDimOptionName, -1);
            if (savedMinDim >= 1 && savedMinDim <= 999999) fnMinDim = savedMinDim;

            int savedMaxDim = option.getInt(maxDimOptionName, -1);
            if (savedMaxDim >= 1 && savedMaxDim <= 999999) fnMaxDim = savedMaxDim;

        }

        fApplyOnPrevCombo.addItemListener(new ItemListener() {
            @Override
            public void itemStateChanged(ItemEvent e) {
                refreshComponentsEnabled(fApplyOnPrevCombo);
            }
        });

        FJLabel fjLabelDimX = new FJLabel(dimXLabel);
        fDimXField = new RangedIntegerTextField(3, 100);
        fDimXField.setInt(fndimx);
        fDimXField.setToolTipText("<html><p width=\"" + fixedToolTipWidth + "\">" + dimXTooltip + "</p></html>");
        GuiFactory.setSizes(fDimXField, new Dimension(fixedFieldWidth, fixedFieldHeigth));
        GuiFactory.setSizes(fjLabelDimX, new Dimension(fixedLabelWidth, fixedLabelHeigth));

        fDimYField = new RangedIntegerTextField(3, 100);
        fDimYField.setInt(fndimy);
        fDimYField.setToolTipText("<html><p width=\"" + fixedToolTipWidth + "\">" + dimYTooltip + "</p></html>");
        GuiFactory.setSizes(fDimYField, new Dimension(fixedFieldWidth, fixedFieldHeigth));

        HBox hboxDimXY = new HBox(new Component[]{fjLabelDimX, fDimXField, /*fjLabelDimY,*/new FJLabel("x"), fDimYField});
        // UNDO COMMENT OUT BELOW TO MAKE IT POSSIBLE FOR THE USER TO CHOOSE NUMBER OF CENTROIDS
        //componentList.add(hboxDimXY);

        refreshComponentsEnabled(fApplyOnPrevCombo);

        //FJLabel LabelApplyOnPrev = new FJLabel(applyOnPrevLabel);
        //fApplyOnPrevCombo.setToolTipText("<html><p width=\"" + fixedToolTipWidth + "\">" + applyOnPrevTooltip + "</p></html>");
        //GuiFactory.setSizes(fApplyOnPrevCombo, new Dimension(fixedComboWidth, fixedFieldHeigth));
        //GuiFactory.setSizes(LabelApplyOnPrev, new Dimension(fixedLabelWidth, fixedLabelHeigth));
        //HBox hboxApplyOnPrev = new HBox(new Component[]{LabelApplyOnPrev, fApplyOnPrevCombo});
        //componentList.add(hboxApplyOnPrev);
        //componentList.add(new HBox(new Component[]{new FJLabel(orPerformezDAFiLabel)}));

        FJLabel fjLabelMinDim = new FJLabel(minDimLabel);
        fMinDimField = new RangedIntegerTextField(1, 999999);
        fMinDimField.setInt(fnMinDim);
        fMinDimField.setToolTipText("<html><p width=\"" + fixedToolTipWidth + "\">" + minDimTooltip + "</p></html>");
        GuiFactory.setSizes(fMinDimField, new Dimension(fixedFieldWidth, fixedFieldHeigth));
        GuiFactory.setSizes(fjLabelMinDim, new Dimension(fixedLabelWidth, fixedLabelHeigth));

        // NEXT LINE ADDED TO SEPARATE MAX DIM BOX FROM MIN DIM BOX
        FJLabel fjLabelMaxDim = new FJLabel(maxDimLabel);
        fMaxDimField = new RangedIntegerTextField(1, 999999);
        fMaxDimField.setInt(fnMaxDim);
        fMaxDimField.setToolTipText("<html><p width=\"" + fixedToolTipWidth + "\">" + maxDimTooltip + "</p></html>");
        GuiFactory.setSizes(fMaxDimField, new Dimension(fixedFieldWidth, fixedFieldHeigth));
        // NEXT LINE ADDED TO ADD MAX DIM BOX SEPARATELY FROM MIN DIM BOX
        GuiFactory.setSizes(fjLabelMaxDim, new Dimension(fixedLabelWidth, fixedLabelHeigth));

        HBox hboxDimMinMax = new HBox(new Component[]{fjLabelMinDim, fMinDimField, /*fjLabelMinDim,*/new FJLabel("max:"), fMaxDimField});
        // UNDO COMMENT OUT BELOW TO MAKE IT POSSIBLE FOR THE USER TO SELECT RANGE OF DIMS
        //componentList.add(hboxDimMinMax);
        //componentList.add(new HBox(new Component[]{new FJLabel(mustBeMinDimLabel)}));

        // NEXT TWO LINES ADDED TO SEPARATE MAX DIM BOX FROM MIN DIM BOX
        HBox hboDimMax = new HBox(new Component[]{fjLabelMaxDim, fMaxDimField});
        //componentList.add(hboDimMax);

        fApplyOnChildrenCheckbox = new FJCheckBox(applyOnChildrenLabel);
        fApplyOnChildrenCheckbox.setToolTipText("<html><p width=\"" + fixedToolTipWidth + "\">" + applyOnChildrenTooltip + "</p></html>");
        fApplyOnChildrenCheckbox.setSelected(fApplyOnChildren);
        // UNDO COMMENT OUT BELOW TO MAKE IT POSSIBLE FOR THE USER TO APPLY ezDAFi TO CHILDREN BUT NOT TO GRANDCHILDREN
        //componentList.add(new HBox(new Component[]{fApplyOnChildrenCheckbox}));

        fKMeansSomOptionCheckbox = new FJCheckBox(kMeansSomLabel);
        fKMeansSomOptionCheckbox.setToolTipText("<html><p width=\"" + fixedToolTipWidth + "\">" + kMeansSomTooltip + "</p></html>");
        fKMeansSomOptionCheckbox.setSelected(fKMeansSom);
        componentList.add(new HBox(new Component[]{fKMeansSomOptionCheckbox}));

        fSummaryOptionCheckbox = new FJCheckBox(summaryLabel);
        fSummaryOptionCheckbox.setToolTipText("<html><p width=\"" + fixedToolTipWidth + "\">" + summaryTooltip + "</p></html>");
        fSummaryOptionCheckbox.setSelected(summaryLayoutBool);
        componentList.add(new HBox(new Component[]{fSummaryOptionCheckbox}));

        fOverlayCheckbox = new FJCheckBox(overlayLabel);
        fOverlayCheckbox.setToolTipText("<html><p width=\"" + fixedToolTipWidth + "\">" + overlayTooltip + "</p></html>");
        fOverlayCheckbox.setSelected(fOverlay);
        componentList.add(new HBox(new Component[]{new FJLabel("     "),fOverlayCheckbox}));

        //fScaleOptionCheckbox = new FJCheckBox(scaleLabel);
        //fScaleOptionCheckbox.setToolTipText("<html><p width=\"" + fixedToolTipWidth + "\">" + scaleTooltip + "</p></html>");
        //fScaleOptionCheckbox.setSelected(fScale);
        //componentList.add(new HBox(new Component[]{fScaleOptionCheckbox}));

        //fPlotStatsOptionCheckbox = new FJCheckBox(plotStatsLabel);
        //fPlotStatsOptionCheckbox.setToolTipText("<html><p width=\"" + fixedToolTipWidth + "\">" + plotStatsTooltip + "</p></html>");
        //fPlotStatsOptionCheckbox.setSelected(fPlotStats);
        //componentList.add(new HBox(new Component[]{fPlotStatsOptionCheckbox}));

        //fPLSOptionCheckbox = new FJCheckBox(PLSLabel);
        //fPLSOptionCheckbox.setToolTipText("<html><p width=\"" + fixedToolTipWidth + "\">" + PLSTooltip + "</p></html>");
        //fPLSOptionCheckbox.setSelected(fPLS);
        //componentList.add(new HBox(new Component[]{fPLSOptionCheckbox}));

        //fMetaOptionCheckbox = new FJCheckBox(metaLabel);
        //fMetaOptionCheckbox.setToolTipText("<html><p width=\"" + fixedToolTipWidth + "\">" + metaTooltip + "</p></html>");
        //fMetaOptionCheckbox.setSelected(fMeta);
        //componentList.add(new HBox(new Component[]{fMetaOptionCheckbox}));

        fBatchOptionCheckbox = new FJCheckBox(batchLabel);
        fBatchOptionCheckbox.setToolTipText("<html><p width=\"" + fixedToolTipWidth + "\">" + batchTooltip + "</p></html>");
        fBatchOptionCheckbox.setSelected(fBatch);
        // UNDO COMMENT OUT BELOW TO MAKE IT POSSIBLE FOR THE USER TO FINISH UP ANALYSIS IN R
        //componentList.add(new HBox(new Component[]{fBatchOptionCheckbox}));

        fNaiveCheckbox = new FJCheckBox(naiveLabel);
        fNaiveCheckbox.setToolTipText("<html><p width=\"" + fixedToolTipWidth + "\">" + naiveTooltip + "</p></html>");
        fNaiveCheckbox.setSelected(fNaive);
        componentList.add(new HBox(new Component[]{fNaiveCheckbox}));

        //FJLabel hSpaceLabelEzExp = new FJLabel("");
        //GuiFactory.setSizes(hSpaceLabelEzExp, new Dimension(fixedLabelWidth, hSpaceHeigth));
        //componentList.add(hSpaceLabelEzExp);

        //componentList.add(fjLabelEzExp0);
        //componentList.add(fjLabelEzExp1);
        //componentList.add(fjLabelEzExp2);

        //fEzExpCheckbox = new FJCheckBox(ezExpLabel);
        //fEzExpCheckbox.setToolTipText("<html><p width=\"" + fixedToolTipWidth + "\">" + ezExpTooltip + "</p></html>");
        //fEzExpCheckbox.setSelected(fEzExp);
        //componentList.add(new HBox(new Component[]{fEzExpCheckbox}));

        //FJLabel fjLabelEzExpSeed = new FJLabel(ezExpSeedLabel);
        //fEzExpSeedField = new RangedIntegerTextField(0, 999);
        //fEzExpSeedField.setInt(fnEzExpSeed);
        //fEzExpSeedField.setToolTipText("<html><p width=\"" + fixedToolTipWidth + "\">" + ezExpSeedTooltip + "</p></html>");
        //GuiFactory.setSizes(fEzExpSeedField, new Dimension(fixedFieldWidth, fixedFieldHeigth));
        //GuiFactory.setSizes(fjLabelEzExpSeed, new Dimension(fixedLabelWidth, fixedLabelHeigth));

        //HBox hboxEzExpSeed = new HBox(new Component[]{fjLabelEzExpSeed, fEzExpSeedField});
        //componentList.add(hboxEzExpSeed);

        FJLabel hSpaceLabelCITESeq = new FJLabel("");
        GuiFactory.setSizes(hSpaceLabelCITESeq, new Dimension(fixedLabelWidth, hSpaceHeigth));
        componentList.add(hSpaceLabelCITESeq);

        componentList.add(fjLabelCITESeq0);
        componentList.add(fjLabelCITESeq1);

        fMultiOptionCheckbox = new FJCheckBox(multiLabel);
        fMultiOptionCheckbox.setToolTipText("<html><p width=\"" + fixedToolTipWidth + "\">" + multiTooltip + "</p></html>");
        fMultiOptionCheckbox.setSelected(fMulti);
        componentList.add(new HBox(new Component[]{fMultiOptionCheckbox}));

//fTransOptionCheckbox = new FJCheckBox(transLabel);
        //fTransOptionCheckbox.setToolTipText("<html><p width=\"" + fixedToolTipWidth + "\">" + transTooltip + "</p></html>");
        //fTransOptionCheckbox.setSelected(fTrans);
        //componentList.add(new HBox(new Component[]{fTransOptionCheckbox}));

        FJLabel hSpaceLabelCiting = new FJLabel("");
        GuiFactory.setSizes(hSpaceLabelCiting, new Dimension(fixedLabelWidth, hSpaceHeigth));
        componentList.add(hSpaceLabelCiting);

        //componentList.add(new FJLabel(pathToScriptLabelLine1));
        //componentList.add(new FJLabel(pathToScriptLabelLine2));
        //componentList.add(new FJLabel(pathToScriptLabelLine3));
        //componentList.add(new FJLabel(pathToScriptLabelLine4));

        componentList.add(new FJLabel(citingLabelLine1));
        componentList.add(new FJLabel(citingLabelLine2));
        componentList.add(new FJLabel(citingLabelLine3));
        componentList.add(new FJLabel(citingLabelLine4));

        MakeLink ml = new MakeLink();
        FJLabel citeLink = ml.makeCLink(citation);
        componentList.add(citeLink);

        componentList.add(new JSeparator());
        String helpText = makeHTMPar("For any questions about the plugin, please reach out: ", 5, 9,"left",
                false);
        FJLabel helpLabel = new FJLabel(helpText);

        String HELP = "";
        if (SeqGeq()) {
            HELP = "mailto:seqgeq@bd.com";
        } else {
            HELP = "mailto:flowjo@bd.com";
        }
        FJLabel help = ml.makeCLink(HELP);
        componentList.add(helpLabel);
        componentList.add(help);

        String supportedByText = makeHTMPar("<br>" + supportedByString, 5,8,"left", false);
        FJLabel supportedByLabel = new FJLabel(supportedByText);
        componentList.add(supportedByLabel);

        return componentList;
    }

    public JScrollPane addFlowJoParameterSelector(List<String> parameters) {

        if (fParameterNameList == null) {
            DefaultListModel dlm = new DefaultListModel();
            for (int i = 0; i < parameters.size(); i++) {
                dlm.add(i, parameters.get(i));
            }
            fParameterNameList = new FJList(dlm);
            fParameterNameList.setSelectionMode(2);
        }

        JScrollPane scrollableList = new JScrollPane(fParameterNameList);
        int[] indexes = new int[fParameterNameList.getModel().getSize()];
        for (int i = 0; i < indexes.length; i++) {
            indexes[i] = i;
        }
        fParameterNameList.setSelectedIndices(indexes);
        //return scrollableList;
        return null;
    }

    public static boolean SeqGeq() {
        if (FJApplication.getInstance() != null &&
                FJApplication.getInstance().getAppName() != null &&
                FJApplication.getInstance().getAppName().toLowerCase().contains("seqgeq"))
            return true;
        return false;
    }

    // t = text, w = width, s = font-size, a = alignment (left|right|center|justify)
    public static String makeHTMPar(String t, int w, int s, String a, boolean bold) {
        String ret = "<html><p style=\"width: " + w + "px;\" style=\"font-size:" + s + "px\" text-align:\"" + a + "\">";
        if (bold) ret += "<b>";
        ret += t;
        if (bold) ret += "</b>";
        ret += "</p></html>";
        return ret;
    }


    protected void refreshComponentsEnabled(FJComboBox applyOnPrevCombo) {
        if (applyOnPrevCombo != null) {
            if (applyOnPrevCombo.getSelectedIndex() > 0) {
                if (fDimXField != null) fDimXField.setEnabled(false);
                if (fDimYField != null) fDimYField.setEnabled(false);
                //if (fScaleOptionCheckbox != null) fScaleOptionCheckbox.setEnabled(false);
                //if (fPlotStatsOptionCheckbox != null) fPlotStatsOptionCheckbox.setEnabled(false);
                //if (fTransOptionCheckbox != null) fTransOptionCheckbox.setEnabled(false);
                if (fBatchOptionCheckbox != null) fBatchOptionCheckbox.setEnabled(false);
                //if (fEzExpCheckbox != null) fEzExpCheckbox.setEnabled(false);
                if (fNaiveCheckbox != null) fNaiveCheckbox.setEnabled(false);
                if (fKMeansSomOptionCheckbox != null) fKMeansSomOptionCheckbox.setEnabled(false);
                if (fMultiOptionCheckbox != null) fMultiOptionCheckbox.setEnabled(false);
                //if (fPLSOptionCheckbox != null) fPLSOptionCheckbox.setEnabled(false);
                //if (fMetaOptionCheckbox != null) fMetaOptionCheckbox.setEnabled(false);
                if (fApplyOnChildrenCheckbox != null) fApplyOnChildrenCheckbox.setEnabled(false);
                if (fSummaryOptionCheckbox != null) fSummaryOptionCheckbox.setEnabled(false);
                if (fOverlayCheckbox != null) fOverlayCheckbox.setEnabled(false);

                // If we are selecting the application on existing map then let's select the same parameters
                // in the parameter selector, and let's do so based on the CSV file that has those.
                String csvWithParsName = applyOnPrevCombo.getSelectedItem().toString() + ezDAFi.CSVwithParsFileSuffix;
                String pluginFolder = this.fsElement.getString(pluginFolderAttName);
                if (pluginFolder != null && !pluginFolder.isEmpty() && fParameterNameList != null) {
                    File csvWithParsFile = new File(pluginFolder + File.separator + csvWithParsName);
                    if (csvWithParsFile.exists()) {
                        ArrayList<String> parsList = new ArrayList<String>();
                        Scanner scan;
                        try {
                            scan = new Scanner(csvWithParsFile);
                            if (scan.hasNextLine())
                                scan.nextLine(); // Skip header (which is "x")
                            while (scan.hasNextLine()) {
                                String line = scan.nextLine();
                                line = line.replaceAll("^\"|\"$", "");
                                if (line != null && !line.isEmpty())
                                    parsList.add(line);
                            }

                            List<Integer> selections = new ArrayList<Integer>();

                            List<String> allParams = fParameterNames;
                            int numberParams = allParams.size();
                            for (int i = 0; i < numberParams; i++) {
                                String pName = allParams.get(i);
                                if (pName != null && !pName.isEmpty()) {
                                    for (String nameFromCSV : parsList) {
                                        if (pName.startsWith(nameFromCSV)) {
                                            selections.add(i);
                                            break;
                                        }
                                    }
                                }
                            }

                            int indices[] = new int[selections.size()];
                            for (int i = 0; i < selections.size(); i++)
                                indices[i] = selections.get(i);
                            fParameterNameList.setSelectedIndices(indices);
                            fParameterNameList.setEnabled(false);

                        } catch (FileNotFoundException e) {
                        }
                    }
                }
            } else {
                if (fDimXField != null) fDimXField.setEnabled(true);
                if (fDimYField != null) fDimYField.setEnabled(true);
                //if (fScaleOptionCheckbox != null) fScaleOptionCheckbox.setEnabled(true);
                //if (fPlotStatsOptionCheckbox != null) fPlotStatsOptionCheckbox.setEnabled(true);
                //if (fTransOptionCheckbox != null) fTransOptionCheckbox.setEnabled(true);
                //if (fBatchOptionCheckbox != null) fBatchOptionCheckbox.setEnabled(true);
                //if (fEzExpCheckbox != null) fEzExpCheckbox.setEnabled(true);
                if (fNaiveCheckbox != null) fNaiveCheckbox.setEnabled(true);
                if (fKMeansSomOptionCheckbox != null) fKMeansSomOptionCheckbox.setEnabled(true);
                if (fMultiOptionCheckbox != null) fMultiOptionCheckbox.setEnabled(true);
                //if (fPLSOptionCheckbox != null) fPLSOptionCheckbox.setEnabled(true);
                //if (fMetaOptionCheckbox != null) fMetaOptionCheckbox.setEnabled(true);
                if (fApplyOnChildrenCheckbox != null) fApplyOnChildrenCheckbox.setEnabled(true);
                if (fParameterNameList != null) fParameterNameList.setEnabled(true);
                if (fSummaryOptionCheckbox != null) fSummaryOptionCheckbox.setEnabled(true);
                if (fOverlayCheckbox != null) fOverlayCheckbox.setEnabled(true);
            }
        }
    }

    @SuppressWarnings("deprecation")
    @Override
    protected void extractPromptOptions() {
        fOptions = new HashMap<>();
        fParameterNames = new ArrayList<>();

        boolean cellIdParameterIncluded = false;

        for (Object obj : fParameterNameList.getSelectedValues()) {
            String parName = (new StringBuilder()).append("").append(obj).toString();
            // FlowJo's parameter names are often in the form of Name :: Description, we only want the Name part from that
            int parDescIndex = parName.indexOf(" :: ");
            if (parDescIndex > 0) parName = parName.substring(0, parDescIndex);
            fParameterNames.add(parName);
            if (parName.equals(ezDAFi.cellIdParName))
                cellIdParameterIncluded = true;
            cellIdParameterIncluded = true;
        }

        // TODO
        // We really need the Time parameter, so we select it even if the user doesn't.
        if (isSeqGeq() && !cellIdParameterIncluded)
            fParameterNames.add(ezDAFi.cellIdParName);

        // Save all the ezDAFi specific options
        fOptions.put(sampleURISlot, fAnalysisPathSampleURI);
        fOptions.put(samplePopNodeSlot, fAnalysisPathSamplePopNode);
        fOptions.put(sampleFileSlot, fAnalysisPathSampleFile);
        //fOptions.put(ezExpSeedOptionName, Integer.toString(fEzExpSeedField.getInt()));
        fOptions.put(minDimOptionName, Integer.toString(fMinDimField.getInt()));
        fOptions.put(maxDimOptionName, Integer.toString(fMaxDimField.getInt()));
        fOptions.put(xDimOptionName, Integer.toString(fDimXField.getInt()));
        fOptions.put(yDimOptionName, Integer.toString(fDimYField.getInt()));
        //fOptions.put(scaleOptionName, fScaleOptionCheckbox.isSelected() ? One : Zero);
        //fOptions.put(plotStatsOptionName, fPlotStatsOptionCheckbox.isSelected() ? One : Zero);
        //fOptions.put(transOptionName, fTransOptionCheckbox.isSelected() ? One : Zero);
        fOptions.put(batchOptionName, fBatchOptionCheckbox.isSelected() ? One : Zero);
        //fOptions.put(ezExpOptionName, fEzExpCheckbox.isSelected() ? One : Zero);
        fOptions.put(naiveOptionName, fNaiveCheckbox.isSelected() ? One : Zero);
        fOptions.put(kMeansSomOptionName, fKMeansSomOptionCheckbox.isSelected() ? One : Zero);
        fOptions.put(multiOptionName, fMultiOptionCheckbox.isSelected() ? One : Zero);
        //fOptions.put(PLSOptionName, fPLSOptionCheckbox.isSelected() ? One : Zero);
        //fOptions.put(metaOptionName, fMetaOptionCheckbox.isSelected() ? One : Zero);
        fOptions.put(applyOnChildrenOptionName, fApplyOnChildrenCheckbox.isSelected() ? One : Zero);
        fOptions.put(summaryLayout, fSummaryOptionCheckbox.isSelected() ? One : Zero);
        fOptions.put(fOverlayString, fOverlayCheckbox.isSelected() ? One : Zero);
        if (fApplyOnPrevCombo.getSelectedIndex() <= 0) {
            fOptions.put(applyOnPrevOptionName, fApplyOnPrevCombo.getSelectedItem().toString());
        } else {
            fOptions.put(applyOnPrevOptionName, fApplyOnPrevCombo.getSelectedItem().toString() + ezDAFi.RDataFileSuffix);
        }

    }

    // Is this SeqGeq based on the application name?
    // This is used to figure out whether we should be including CellIds in the CSV files
    public static boolean isSeqGeq() {
        if (FJApplication.getInstance() != null &&
                FJApplication.getInstance().getAppName() != null &&
                FJApplication.getInstance().getAppName().toLowerCase().contains("seqgeq"))
            return true;
        return false;
    }

    private static final double epsilon = 0.1;

    // Use FlowJo's CSV reader instead of manually and get the column where the categorical is found
    private List<Float> extractUniqueValuesForParameter(File sampleFile) {
        HashSet<Float> uniqueValues = new HashSet<>();
        try {
            CSVReader reader = new CSVReader(new FileReader(sampleFile));
            List<String[]> entries = reader.readAll();
            int categoricalColIndex = -1;
            for (String[] entry : entries) {
                for (int i = 0; i < entry.length; i++) {
                    if (categoricalColIndex == -1) {
                        if (entry[i].equalsIgnoreCase(pluginName)) {
                            categoricalColIndex = i;
                        }
                    } else if (i == categoricalColIndex) {
                        try {
                            float val = Float.parseFloat(entry[i]);
                            val = (float) (epsilon * Math.round(val / epsilon));
                            uniqueValues.add(val);
                        } catch (NumberFormatException e) {
                            System.out.println("Error in parsing " + entry[i]);
                            //We should not get any weird formats but we could see commas or related that break stuff
                        }
                    }
                }
            }
        } catch (FileNotFoundException e) {
//            e.printStackTrace();
            System.out.println();
        } catch (IOException e) {
//            e.printStackTrace();
        }


        return new ArrayList<>(uniqueValues);
    }

    //trying to open .R.txt file
    public void open(File document)
            throws IOException {
        Desktop.getDesktop().open(document);
    }

    public String tail2( File file, int lines) {
        java.io.RandomAccessFile fileHandler = null;
        try {
            fileHandler =
                    new java.io.RandomAccessFile( file, "r" );
            long fileLength = fileHandler.length() - 1;
            StringBuilder sb = new StringBuilder();
            int line = 0;

            for(long filePointer = fileLength; filePointer != -1; filePointer--){
                fileHandler.seek( filePointer );
                int readByte = fileHandler.readByte();

                if( readByte == 0xA ) {
                    if (filePointer < fileLength) {
                        line = line + 1;
                    }
                } else if( readByte == 0xD ) {
                    if (filePointer < fileLength-1) {
                        line = line + 1;
                    }
                }
                if (line >= lines) {
                    break;
                }
                sb.append( ( char ) readByte );
            }

            String lastLine = sb.reverse().toString();
            return lastLine;
        } catch( java.io.FileNotFoundException e ) {
            e.printStackTrace();
            return null;
        } catch( java.io.IOException e ) {
            e.printStackTrace();
            return null;
        }
        finally {
            if (fileHandler != null )
                try {
                    fileHandler.close();
                } catch (IOException e) {
                }
        }
    }

    public static int countLinesNew(String filename) throws IOException { //https://stackoverflow.com/a/453067
        InputStream is = new BufferedInputStream(new FileInputStream(filename));
        try {
            byte[] c = new byte[1024];

            int readChars = is.read(c);
            if (readChars == -1) {
                // bail out if nothing to read
                return 0;
            }

            // make it easy for the optimizer to tune this loop
            int count = 0;
            while (readChars == 1024) {
                for (int i=0; i<1024;) {
                    if (c[i++] == '\n') {
                        ++count;
                    }
                }
                readChars = is.read(c);
            }

            // count remaining characters
            while (readChars != -1) {
                System.out.println(readChars);
                for (int i=0; i<readChars; ++i) {
                    if (c[i] == '\n') {
                        ++count;
                    }
                }
                readChars = is.read(c);
            }

            return count == 0 ? 1 : count;
        } finally {
            is.close();
        }
    }


}
