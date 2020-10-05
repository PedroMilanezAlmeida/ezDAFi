package com.flowjo.plugin.ezDAFi.utils;

import com.treestar.flowjo.core.nodes.AppNode;
import com.treestar.flowjo.core.nodes.PopNode;
import com.treestar.lib.xml.SElement;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Queue;

public class PopTraverser {

    public static void scaryTraversal(PopNode theNode, HashMap<AppNode,SElement> popHash, SElement element)
//    public static HashMap<AppNode, SElement> scaryTraversal(PopNode theNode, HashMap<AppNode,SElement> popHash, SElement element)
    {
//        if (theNode == null)
//            return null;
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

                String nodeName = p.getName();

                // Check if this is an ezDAFi node
                if(nodeName.contains("_ezDAFi_")){
                    // regEx out the part of the string we want to remove
                    String newName = nodeName.replaceAll("(^.+)?(ezDAFi_)","DAFi_");
                    p.setName(newName);
                }

                // We don't need any of the following things for DAFi:

//                boolean thisIsTheSample = p.isSampleNode();

//                System.out.println(p.getName());
//                System.out.println(p.getCount());
//                System.out.println(p.getElement());         // This contains a gate id and parent gate id
//                SElement popElement = p.getElement();
//                SElement popGateElement = null;

//                if(!thisIsTheSample) {      // This is not the sample
//
//                    if (p.getParent() != null) {
//
////                    System.out.println("What is the element for pops?: ");
////                    System.out.println(popElement);
//
//                        popGateElement = popElement.getChild("Gate");
//                        if (popGateElement != null) {//   getAttribute("gating:id") != null) {          // This avoids null pointer for AND gates but we still need an id
//                            String id = popGateElement.getAttribute("gating:id");
//
//                            String pId = popGateElement.getAttribute("gating:parent_id");
//
//                            if(pId == null && sampleFlag){
//                                pId = sampID;
//                                popGateElement.setAttribute("gating:parent_id", pId);
//                            }
//                            popHash.put(p, popGateElement);
//                        }
//                    } else {
//                        popGateElement = popElement;
////                        popHash.put(p, popGateElement);
//                    }
//                } else {        // We are dealing with a sample pop!
//                    popGateElement = popElement;
//                    if (popGateElement != null) {//   getAttribute("gating:id") != null) {          // This avoids null pointer for AND gates but we still need an id
//
//                        String id = popGateElement.getAttribute("sampleID");
//                        popGateElement.setAttribute("gating:id",id);
//                        popGateElement.setAttribute("gating:parent_id","S"+id+"_"+p.getCount());
//
//                        sampleFlag = true;
//                        sampID = id;
//
//                        popHash.put(p, popGateElement);
//                    }
//                }

                m++;

                // Put all children of the node in a list:
                for (int i = 0; i < p.getChildren().size(); i++)
                    q.add(p.getChild(i));
                l--;
            }
//            System.out.println();
        }
//        return popHash;
    }
}
