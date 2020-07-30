package com.flowjo.plugin.ezDAFi.utilities;

import com.treestar.flowjo.application.workspace.Workspace;
import com.treestar.flowjo.core.GParameter;
import com.treestar.flowjo.core.GraphSpec;
import com.treestar.flowjo.core.Sample;
import com.treestar.flowjo.core.gates.PolyGate;
import com.treestar.flowjo.core.gates.RangeGate;
import com.treestar.flowjo.core.gates.ScalePolygon;
import com.treestar.flowjo.core.nodes.PopNode;
import com.treestar.lib.data.Range;
import com.treestar.lib.fjml.types.DisplayType;
import com.treestar.lib.fjml.types.ScalePoint;

public class PopulationUtilities {

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
     * Creates a rectangle gate using the (x1, y1), (x1, y2), (x2, y2), (x2, y1) scale value points
     *
     * @param parentNode         parent PopNode on which to hang the new population
     * @param gateName           name of new gate
     * @param xAxisParameterName x-axis parameter name, must be in parentNode's sample parameters
     * @param yAxisParameterName y-axis parameter name, must be in parentNode's sample parameters
     * @param x1                 scale value of x1
     * @param x2                 scale value of x2
     * @param y1                 scale value of y1
     * @param y2                 scale value of y2
     * @return PopNode for rectangle gate
     */
    public static PopNode createRectangleGate(PopNode parentNode, String gateName, String xAxisParameterName, String yAxisParameterName, double x1, double x2, double y1, double y2) {
        try {
            if (parentNode == null) return null;
            if (xAxisParameterName.isEmpty() || yAxisParameterName.isEmpty()) return null;

            Sample sample = parentNode.getSample();
            if (sample == null) return null;

            GParameter xAxisGParameter = sample.getParameter(xAxisParameterName);
            GParameter yAxisGParameter = sample.getParameter(yAxisParameterName);

            if (xAxisGParameter == null && yAxisGParameter == null) {
                return null;
            }

            ScalePolygon scalePolygon = new ScalePolygon();
            scalePolygon.addpoint(new ScalePoint(x1, y1));
            scalePolygon.addpoint(new ScalePoint(x1, y2));
            scalePolygon.addpoint(new ScalePoint(x2, y2));
            scalePolygon.addpoint(new ScalePoint(y2, x1));

            PolyGate polyGate = new PolyGate(parentNode, xAxisGParameter, yAxisGParameter, scalePolygon);
            if (polyGate == null) return null;
            polyGate.setUserDefined(false);

            GraphSpec spec = new GraphSpec(xAxisParameterName, yAxisParameterName, DisplayType.Pseudocolor);
            if (spec == null) return null;

            PopNode polygonGateNode = new PopNode(gateName, polyGate, spec, parentNode.getWorkspace(), parentNode.getSample());
            if (polygonGateNode == null) return null;

            parentNode.addAnalysisNode(polygonGateNode);
            parentNode.setDirtyWithAllDependents();
            parentNode.updateIfDirty();

            return polygonGateNode;
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }
        return null;
    }

    /**
     * Creates a RangeGate on a parentNode PopNode using parameter with population name
     *
     * @param parentNode     parentNode to hang RangeGate on
     * @param parameter      Parameter name to get GParameter from Sample
     * @param populationName String population name
     * @return boolean if rangeGate added true or else false
     */
    public static PopNode rangeGatePopulation(PopNode parentNode, GParameter parameter, String populationName, double xMinimum, double xMaximum) {

        try {
            double offset = 0.3;

            String parameterName = parameter.getName();
            Range range = new Range(xMinimum, xMaximum);
            RangeGate rangeGate = new RangeGate(parameterName, range, offset);
            rangeGate.setUserDefined(false);
            GraphSpec spec = new GraphSpec(parameterName);
            Workspace ws = parentNode.getWorkspace();
            Sample sample = parentNode.getSample();

            if (ParameterUtils.checkParameterExists(sample, parameterName)) {
                PopNode subrangeNode = new PopNode(populationName, rangeGate, spec, ws, sample);

                parentNode.addAnalysisNode(subrangeNode);
                subrangeNode.setDirty();
                subrangeNode.setParent(parentNode);
                subrangeNode.updateIfDirty();

                return subrangeNode;
            } else {
                return null;
            }

        } catch (Exception e) {
            return null;

        }
    }

    public static GraphSpec getUnivariateHistogramGraphSpec(PopNode graphPop, GParameter xParameter) {
        GraphSpec graphSpec = new GraphSpec(graphPop.getGraphSpec().getElement());
        DisplayType displayType = DisplayType.Histogram;
        graphSpec.setXAxisName(xParameter.getName());
        graphSpec.setShowGateNames(true);
        graphSpec.setYAxisName("Histogram");
        graphSpec.setYAuto(true);
        graphSpec.setType(displayType);
        graphSpec.setShowGates(true);
        graphSpec.setShowGateNames(true);

        return graphSpec;
    }


    public static GraphSpec getPseudocolorGraphSpec(PopNode graphPop, GParameter xParameter, GParameter yParameter) {
        GraphSpec graphSpec = new GraphSpec(graphPop.getGraphSpec().getElement());
        DisplayType displayType = DisplayType.Pseudocolor;

        graphSpec.setXAxisName(xParameter.getName());
        graphSpec.setYAxisName(yParameter.getName());

        graphSpec.setShowGateNames(true);

        graphSpec.setType(displayType);
        graphSpec.setShowGates(true);
        graphSpec.setShowGateNames(true);

        return graphSpec;
    }
}
