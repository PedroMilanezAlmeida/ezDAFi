package com.flowjo.plugin.DAFi.utilities;

import com.treestar.lib.data.Range;

public class SimpleRangeGate {

    private String gateName = null;
    private double min = 0;
    private  double max = 0;

    public SimpleRangeGate(String gateName, double min, double max){
        this.gateName = gateName;
        this.min = min;
        this.max = max;
    }

    public double getMax() {
        return max;
    }

    public double getMin() {
        return min;
    }

    public String getGateName() {
        return gateName;
    }

    public void setGateName(String gateName) {
        this.gateName = gateName;
    }

    public void setMax(double max) {
        this.max = max;
    }

    public void setMin(double min) {
        this.min = min;
    }

    public Range getRange(){
        return new Range(min, max);
    }
}

