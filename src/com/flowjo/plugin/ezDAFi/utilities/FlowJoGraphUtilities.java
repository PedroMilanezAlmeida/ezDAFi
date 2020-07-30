/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.flowjo.plugin.DAFi.utilities;

import com.treestar.flowjo.application.graphwindow.image.BufferedImageMaker;
import com.treestar.flowjo.core.GParameter;
import com.treestar.flowjo.core.GraphSpec;
import com.treestar.flowjo.core.Sample;
import com.treestar.flowjo.core.nodes.GraphNode;
import com.treestar.flowjo.core.nodes.PopNode;
import com.treestar.flowjo.core.nodes.SampleNode;
import com.treestar.lib.fjml.types.DisplayType;

import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;

/**
 * @author miguelv
 */
public class FlowJoGraphUtilities {


    private static final int sGraphSize = 400;

    /*
     *   This method request a buffered image from FJ using scatter parameters, if they are available.
     *   TODO: Check if there is scatter parameters, if not then let kit know!
     *   TODO: Allow kit to select their own defined gate for scatter
     */
    public static BufferedImage getScatterBufferedImage(Sample ss) {

        SampleNode node = ss.getSampleNode();

        PopNode graphPop = node.getParentPop();
        if (graphPop == null) {
            graphPop = node;
        }

        GraphSpec graphSpec = getGraphSpecScatter(graphPop, ss);

        List<GraphNode> pnr = new ArrayList<>();
        pnr.add(node);

        BufferedImageMaker mim = new BufferedImageMaker(pnr, graphSpec, 2, sGraphSize, sGraphSize);

        return mim.createBufferedImage();
    }


    /*
     * This method launches a graph of the Sample passed in
     * Defines a GraphSpec to request the data.
     */
    public static void openReportPseudocolorGW(Sample ss, String xParameter, String gate, String yParameter) {

        SampleNode node = ss.getSampleNode();
        PopNode graphPop = null;

        List<PopNode> kn = node.getAllGates();

        for (PopNode popNode : kn) {
//            System.out.println("PopNode: " + popNode.getName());
            if (popNode.getName().equalsIgnoreCase(gate)) {
                graphPop = popNode;
            }
        }

        if (graphPop == null) {
            graphPop = node.getParentPop();
            ;
        }

        GraphSpec graphSpec = getGraphSpecReporterPseudocolor(graphPop, xParameter, yParameter);
        graphPop.setGraphSpec(graphSpec);
        graphPop.update();
        graphPop.open(ss.getWorkspace());
    }

    /*
     * This method launches a graph of the Sample passed in
     * Defines a GraphSpec to request the data.
     */
    public static void openReportHistogramGW(Sample ss, String xParameter, String gate) {

        SampleNode node = ss.getSampleNode();
        PopNode graphPop = null;

        List<PopNode> kn = node.getAllGates();

        for (PopNode popNode : kn) {
            if (popNode.getName().equalsIgnoreCase(gate)) {
                graphPop = popNode;
            }
        }

        if (graphPop == null) {
            graphPop = node.getParentPop();
        }

        GraphSpec graphSpec = getGraphSpecPeaksHist(graphPop, ss, xParameter);
        graphPop.setGraphSpec(graphSpec);
        graphPop.update();
        graphPop.open(ss.getWorkspace());


    }

    /*
     * This method launches a graph of the Sample passed in
     * Defines a GraphSpec to request the data.
     */
    public static void launchScatter(Sample ss) {

        SampleNode node = ss.getSampleNode();
        PopNode graphPop = node.getParentPop();
        if (graphPop == null) {
            graphPop = node;
        }

        GraphSpec graphSpec = FlowJoGraphUtilities.getGraphSpecScatter(graphPop, ss);

        graphPop.setGraphSpec(graphSpec);
        graphPop.update();
        graphPop.open(ss.getWorkspace());
    }

    public static GraphSpec getGraphSpecScatter(PopNode graphPop, Sample ss) {
        GraphSpec graphSpec = new GraphSpec(graphPop.getGraphSpec().getElement());
        DisplayType displayType = DisplayType.Pseudocolor;

        graphSpec.setXAxisName(ss.getForwardScatter().getName());
        graphSpec.setYAxisName(ss.getSideScatter().getName());

        graphSpec.setType(displayType);
        graphSpec.setShowGates(true);

        return graphSpec;
    }

    private static GraphSpec getGraphSpecPeaksHist(PopNode graphPop, Sample ss, String xParameter) {
        GraphSpec graphSpec = new GraphSpec(graphPop.getGraphSpec().getElement());
        DisplayType displayType = DisplayType.Histogram;
        graphSpec.setXAxisName(xParameter);
        graphSpec.setShowGateNames(true);
        graphSpec.setYAxisName("Histogram");
        graphSpec.setYAuto(true);
        graphSpec.setType(displayType);
        graphSpec.setShowGates(true);
        graphSpec.setShowGateNames(true);

        return graphSpec;
    }

    private static GraphSpec getGraphSpecReporterPseudocolor(PopNode graphPop, String xParameter, String yParameter) {
        GraphSpec graphSpec = new GraphSpec(graphPop.getGraphSpec().getElement());
        DisplayType displayType = DisplayType.Pseudocolor;

        graphSpec.setXAxisName(xParameter);
        graphSpec.setYAxisName(yParameter);

        graphSpec.setShowGateNames(true);
        graphSpec.setHeatMapStatParameter(xParameter);

        graphSpec.setType(displayType);
        graphSpec.setShowGates(true);
        graphSpec.setShowGateNames(true);

        return graphSpec;
    }

    private static GraphSpec getGraphSpecReporterPseudocolorCluster(PopNode graphPop, String xParameter, String yParameter) {
        GraphSpec graphSpec = new GraphSpec(graphPop.getGraphSpec().getElement());
        DisplayType displayType = DisplayType.Pseudocolor;

        graphSpec.setXAxisName(xParameter);
        graphSpec.setYAxisName(yParameter);

        graphSpec.setShowGateNames(true);

        graphSpec.setType(displayType);
        graphSpec.setShowGates(true);
        graphSpec.setShowGateNames(true);

        return graphSpec;
    }


    private static final int GRAPH_HEIGHT = 400;
    private static final int GRAPH_WIDTH = 400;

    /*
     *   This method request a buffered image from FJ using scatter parameters, if they are available.
     *   TODO: Check if there is scatter parameters, if not then let kit know!
     *   TODO: Allow kit to select their own defined gate for scatter
     */
    public static BufferedImage getUnivariateHistogramImage(GParameter xParameter, PopNode popNode) {
        if (xParameter == null || popNode == null) {
            return null;
        }

        GraphSpec graphSpec = getGraphSpecPeaksHist(popNode, xParameter.getSample(), xParameter.getName());

        List<GraphNode> graphNodes = new ArrayList<>();
        graphNodes.add(popNode);

        BufferedImageMaker bufferedImageMaker = new BufferedImageMaker(graphNodes, graphSpec, 3, GRAPH_WIDTH, GRAPH_HEIGHT);

        return bufferedImageMaker.createBufferedImage();
    }


    /*
     *   This method request a buffered image from FJ using scatter parameters, if they are available.
     *   TODO: Check if there is scatter parameters, if not then let kit know!
     *   TODO: Allow kit to select their own defined gate for scatter
     */
    public static BufferedImage getBivariateHistogramImage(GParameter xParameter, GParameter yParameter, PopNode popNode) {

        if (xParameter == null || yParameter == null || popNode == null) {
            return null;
        }

        GraphSpec graphSpec = getGraphSpecReporterPseudocolorCluster(popNode, xParameter.getName(), yParameter.getName());

        List<GraphNode> graphNodes = new ArrayList<>();
        graphNodes.add(popNode);

        BufferedImageMaker bufferedImageMaker = new BufferedImageMaker(graphNodes, graphSpec, 3, GRAPH_WIDTH, GRAPH_HEIGHT);

        return bufferedImageMaker.createBufferedImage();
    }

}
