package com.flowjo.plugin.ezDAFi.utils;

import com.treestar.flowjo.application.graphwindow.gatewindow.GateWinController;
import com.treestar.flowjo.application.leditor.LEditor;
import com.treestar.flowjo.application.leditor.Layout;
import com.treestar.flowjo.charting.FigChartAnnotation;
import com.treestar.flowjo.charting.LEChart;
import com.treestar.flowjo.charting.LEChartController;
import com.treestar.flowjo.core.*;
import com.treestar.flowjo.core.nodes.*;
import com.treestar.layout.editor.Editor;
import com.treestar.layout.presentation.Fig;
import com.treestar.layout.presentation.FigList;
import com.treestar.lib.explorer.TemplateChangeEvent;
import com.treestar.lib.fjml.types.DisplayType;
import com.treestar.lib.xml.SElement;

import javax.swing.undo.UndoManager;
import java.awt.*;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.List;
import java.util.*;

import static java.lang.Thread.sleep;

public class CustomDAFTiLayout {

    public static LinkedHashMap<AppNode, Integer> sortHashMap(HashMap<AppNode, Integer> passedMap) {

        ArrayList<Integer> mapValues = new ArrayList();
        mapValues.addAll(passedMap.values());
        ArrayList<AppNode> mapKeys = new ArrayList<>();
        mapKeys.addAll(passedMap.keySet());
        Collections.sort(mapValues);

        LinkedHashMap<AppNode, Integer> sortedMap = new LinkedHashMap<>();

        // Order by Value:
        Iterator valueIt = mapValues.iterator();
        while (valueIt.hasNext()) {
            Object val = valueIt.next();
            Iterator keyIt = mapKeys.iterator();

            while (keyIt.hasNext()) {
                Object key = keyIt.next();
                String comp1 = passedMap.get(key).toString();
                String comp2 = val.toString();

                if (comp1.equals(comp2)) {
                    passedMap.remove(key);
                    mapKeys.remove(key);
                    sortedMap.put(( AppNode ) key, ( Integer ) val);
//                    sortedMap.put((String)key, (Double)val);
                    break;
                }
            }
        }

        return sortedMap;
    }



    public static void createOverlay(HashMap<AppNode, Integer> popHash, Sample sample, //List<AppNode> appNodeList,
                                     String layoutName, int maxColumns, SElement sElement,
                                     boolean showLayout, File folder) throws MalformedURLException {

        Color niceRed = new Color(152, 2, 37);
        Color sickSlate = new Color(159, 159, 159, 255);
        boolean addRows = false;

        LEditor layoutEditor = sample.getWorkspace().getLayoutEditor();

        Layout nd = layoutEditor.doNewTemplate();
        nd.setName(layoutName);
        nd.setShowPageBreaks(false);
        nd.setShowGrid(false);

        int y = 0;
        int x = -1;
        int yOffset;
        int xOffset;

        LinkedHashMap<AppNode, Integer> sorted = sortHashMap(popHash);

        int rowCount = 0;
        for (AppNode AN : sorted.keySet()) {  // Loop over this twice, once to set the parent population, and again to overlay the actual population

            if (rowCount == 0) {
                rowCount = sorted.get(AN); // first time through, set the row count to IT
            }
            if (sorted.get(AN) > rowCount) {
                rowCount = sorted.get(AN); // if the depth increases, increase the row count
                addRows = true;
            }
            if (addRows) {
                x = 0;
                y++;
                addRows = false;
            } else {
                x++;
            }

            xOffset = ( int ) (500 * x) + 300;
            yOffset = ( int ) (600 * y) + 200;

            if ((AN != null)) {

                // Instead of using the parent of the DAFi gate (below) use the parent of the manually gated node after discovering that...
//                PopNode Parent = AN.getParentPop();
//                GraphSpec nodeSpec = Parent.getGraphSpec().clone(Parent.getSample());

                String PopName = AN.getName().replace("ezDAFi_", "");
                String xPar = "";
                String yPar = "";
                List<PopNode> lister = sample.getSampleNode().getAllGates(); // getActualChildren();
                for (PopNode node : lister) {
                    if (node.getName().equals(PopName)) {

                        // NOTE: Node is then the non-DAFi version of the gate:
                        PopNode Parent = node.getParentPop();
                        GraphSpec nodeSpec = Parent.getGraphSpec().clone(Parent.getSample());

                        List<String> pars = node.getGateParams();
                        xPar = pars.get(0);
                        yPar = pars.get(1);

                        // NOTE: without its duplicate down below this doesn't seem to work...
                        nodeSpec.setXAxisName(xPar);
                        nodeSpec.setYAxisName(yPar);

                        try {
                            sleep(100);
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }

                        nodeSpec.setType(DisplayType.DotPlot);
                        nodeSpec.setDrawLargeDots(false);
                        nodeSpec.setShowOutliers(true);
                        nodeSpec.setNumberOfDots(100);

                        // Had to duplicate this to make it work...
                        nodeSpec.setXAxisName(xPar);
                        nodeSpec.setYAxisName(yPar);

                        GateWinController gwc = new GateWinController(Parent, nodeSpec);
                        gwc.eval(true);
                        gwc.updateGraphSpec();
                        gwc.init(layoutEditor.getLocation());
                        gwc.getFrame().setVisible(true);

                        try {
                            sleep(100);
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }

                        GraphSpec nodeSpec02 = Parent.getGraphSpec().clone(Parent.getSample());
                        GateWinController gwc02 = new GateWinController(Parent, nodeSpec02);
                        GraphNode node02 = gwc02.getNode();
                        NodeList nodelist02 = new NodeList(node02);

                        if (showLayout) {
                            layoutEditor.show();
                            showLayout = false;
                        }

                        try {
                            sleep(100);
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }

                        Layout curLayout = layoutEditor.getCurLayout();
                        if (nodelist02 != null) {
                            layoutEditor.getEditor().addFigs(nodelist02, new Point(xOffset, yOffset), null);
                            layoutEditor.fireTemplateChanged(new TemplateChangeEvent(layoutEditor, curLayout));
                        }

                        gwc.getFrame().dispose();
                        try {
                            sleep(100);
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }

                        // Get the figure object and deal with it...
                        // Normally the fig should be: "ChartData"
                        FigList f = curLayout.getFigList();
                        int fize = f.size() - 3;
                        Fig figger = curLayout.getFigList().get(fize);
//                        System.out.println("What is the figger element?: ");
//                        System.out.println(figger.getElementName());

                        Editor fEditMe = figger.getParentEditor();

                        Fig aFig = null;
                        if(!AN.isSampleNode())
                            aFig = (( PopNode ) AN).makeFigForDrop(new Point(xOffset, yOffset), fEditMe);
                        else
                            aFig = (( SampleNode ) AN).makeFigForDrop(new Point(xOffset, yOffset), fEditMe);

                        try {
                            sleep(100);
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }

                        // Creates the overlay
                        Point fp = figger.getLocation();
                        figger.setWantsDrops(true);
                        FigList fl = new FigList();
                        fl.addFig(aFig);
                        figger.dropFigs(fl, fp, false);
                        figger.repaint();

                        // Customize the color
                        LEChartController LEC = figger.getController();
                        UndoManager UM = new UndoManager();
                        PopModelList PML = LEC.getModel();
                        PopModel PM = PML.getPopModel(0);   //getModel(0);
                        PM.setColor(niceRed,UM);
                        PML.getPopModel(1).setColor(sickSlate,UM);

                        try {
                            sleep(100);
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }

                        // Make sure the parameters are named nicely:
                        GraphSpec GS = LEC.getGraphSpec();

                        AxisSpec ASCx = GS.getXAxis();
                        AxisSpec ASCy = GS.getYAxis();

//                        GParameter gParX = sample.getParameter(xPar);
//                        GParameter gParY = sample.getParameter(yPar);
//                        System.out.println("What is the x par identifier?: " + gParX.getIdentifier());
//                        System.out.println("What is the y par identifier?: " + gParY.getIdentifier());
//                        ASCx.setAxisParameter(gParX.getIdentifier());
//                        ASCy.setAxisParameter(gParY.getIdentifier());

                        String parNamerX = sample.getParameter( GS.getXAxis().getAxisParameter() ).getStain();
                        if(parNamerX.isEmpty()){
                            parNamerX = GS.getXAxisName();
                        }

                        String parNamerY = sample.getParameter( GS.getYAxis().getAxisParameter() ).getStain();
                        if(parNamerY.isEmpty()){
                            parNamerY = GS.getXAxisName();
                        }

                        ASCx.setLabel(parNamerX);
                        ASCy.setLabel(parNamerY);

                        // Give figures some annotation and add the contents of geneset CSV files:
                        String csvString = grabCSVtext(folder, node);

                        LEChart chart = LEC.getLEChart();
                        chart.showAnnotation(true);

                        FigChartAnnotation fca = chart.getAnnotation();
                        fca.insertFJML(4, csvString);

                        figger.repaint();
                        layoutEditor.repaint();

                        layoutEditor.fireTemplateChanged(new TemplateChangeEvent(layoutEditor, curLayout));
                    }
                }
//                layoutEditor.fireTemplateChanged(new TemplateChangeEvent(layoutEditor, curLayout));
            } else {
                System.out.println("Could not find a population.");
            }
        }
    }

    public static void formatLegend(Sample sample) throws MalformedURLException {

        LEditor layoutEditor = sample.getWorkspace().getLayoutEditor();

        Layout curLayout = layoutEditor.getCurLayout();

        FigList figures = curLayout.getFigList();
        for (Fig figger : figures) {

            if (figger.getElementName().equals("Legend")) {
                figger.setLineColor(Color.white);
            }
        }
        layoutEditor.fireTemplateChanged(new TemplateChangeEvent(layoutEditor, curLayout));

    }

    public static String grabCSVtext(File outputFolder, AppNode appNode) {

        File inputCSVFile = null;
        String csvString = "";

        File[] files = outputFolder.listFiles();
        for(File f : files){
            String namer = f.getName().replaceAll("_Features.csv",""); //.replaceAll("\\.csv","")
            if(namer.equals(appNode.getName())){
                inputCSVFile = f;
            }
        }

        if(inputCSVFile != null & inputCSVFile.exists()) {

            try {
                BufferedReader inputCSVFileReader = new BufferedReader(new FileReader(inputCSVFile));

                // read the first header line of the input CSV sample file
                String csvLine = inputCSVFileReader.readLine();

                int colCt = 0;
                boolean startWrite = false;
                csvString = "<br>Parameter, t-Score\n";

                while (inputCSVFileReader.ready()){
                    csvLine = inputCSVFileReader.readLine();
                    StringTokenizer tokenizer = new StringTokenizer(csvLine, "\n");
                    String token = tokenizer.nextToken();

                    if(token.contains("[Genes],t-Value")) {
                        startWrite = true;
                        colCt = 0;
                    }

                    if (startWrite && colCt > 0) {
                        if(colCt == 1)
                            csvString = csvString + token;
                        else if ((colCt-1) % 3 == 0)
                            csvString = csvString + "\n" + token;
                        else
                            csvString = csvString + "&nbsp; &nbsp; &nbsp; &nbsp;" + token;
                    }
                    colCt++;
                }

            } catch (IOException e){
                System.out.println("Error" + e);
            }
        }

        return csvString;
    }
}

