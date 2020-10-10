package com.flowjo.plugin.ezDAFi.utils;

import com.treestar.flowjo.core.nodes.AppNode;
import com.treestar.flowjo.core.nodes.PopNode;
import com.treestar.lib.xml.SElement;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Queue;

public class PopTraverser {

//    public static void scaryTraversal(PopNode theNode, HashMap<AppNode,SElement> popHash, SElement element)
    public static HashMap<AppNode,Integer> scaryTraversal(PopNode theNode, HashMap<AppNode, Integer> popHash, SElement element)
    {
        String sampID = null;
        boolean sampleFlag = false;
        int m = 0;
        Queue<AppNode> q = new LinkedList<>();
        q.add(theNode);
        while (!q.isEmpty())
        {
            int l = q.size();

            // If this node has children
            while (l > 0) {
                AppNode p = q.peek();
                q.remove();

                // Check if the node is a DP
                if(!p.isDPNode()) {
                    // Check if the node has any cells
                    if(p.getCount() > 0) {
                        // Check if this is an ezDAFi node
                        String nodeName = p.getName();
                        if (nodeName.contains("_ezDAFi_")) {
                            // regEx out the part of the string we want to remove
                            String newName = nodeName.replaceAll("(^.+)?(ezDAFi_)", "ezDAFi_");
                            p.setName(newName);

                            boolean thisIsTheSample = p.isSampleNode();
                            if (!thisIsTheSample) {
                                // add the populations to a list for use in Layout
                                popHash.put(p, p.getDepth());
                            }
                        }
                    }
                }
                m++;

                // Put all children of the node in a list:
                for (int i = 0; i < p.getChildren().size(); i++)
                    q.add(p.getChild(i));
                l--;
            }
        }
        return popHash;
    }
}
