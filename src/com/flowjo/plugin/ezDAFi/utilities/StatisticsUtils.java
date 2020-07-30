package com.flowjo.plugin.ezDAFi.utilities;

import com.treestar.flowjo.core.GParameter;
import com.treestar.flowjo.core.nodes.PopNode;
import com.treestar.flowjo.core.nodes.StatisticNode;
import com.treestar.lib.i18n.StatKey;

public class StatisticsUtils {

    /**
     * Create a StatisticNode and add to ParentNode on parameterName with type StatKey
     *
     * @param parentNode    PopNode on which to attach StatisticNode
     * @param parameter GParameter to use for
     * @param statKey
     * @return
     */
    public static StatisticNode createStatistic(PopNode parentNode, GParameter parameter, StatKey statKey) {
        switch (statKey) {
            case CV:
                StatisticNode statisticNodeCV = new StatisticNode(StatKey.CV, parameter.getName(), 50, "",
                        parentNode.getWorkspace(), parentNode);
                addStatisticNode(parentNode, statisticNodeCV);
                return statisticNodeCV;
            case Median:
                StatisticNode statisticNodeMedian = new StatisticNode(StatKey.Median, parameter.getName(), 50, "",
                        parentNode.getWorkspace(), parentNode);
                return statisticNodeMedian;

            default:
                return null;
        }

    }

    /**
     * Add a StatisticNode to a parent PopNode, set dirty flag and request update.
     * @param node PopNode parent to attach StatisticNode
     * @param stat StatisticNode to attach to PopNode parent
     */
    public static void addStatisticNode(PopNode node, StatisticNode stat) {
        node.addAnalysisNode(stat);
        stat.setDirty();
        stat.setParent(node);
        stat.updateIfDirty();

        node.setDirty();
        node.setParent(node.getParent());
        node.updateIfDirty();
        node.forceUpdate();
    }

    /**
     * Utility for finding a StatKey from Enums
     *
     * @param statKeyType StringOf StatKey
     * @return StatKey if valid StatKey
     */
    public static StatKey getStatKeyType(String statKeyType) {
        StatKey statKey = StatKey.find(statKeyType);
        if (statKey == null) return statKey;
        return statKey;
    }
}
