////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017 Josef Spidlen, Ph.D., FlowJo, LLC
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

package com.flowjo.plugin.ezDAFi.utils;

import com.treestar.lib.gui.numberfields.FJNumberFormatter;

/**
 * I seem to need a class that implements the FJNumberFormatter interface 
 * in order to be able to use FlowJo's RangedDoubleTextField.
 * I feel like FlowJo must have a class that is implementing this already, but 
 * I cannot find it. If I had their source codes, maybe it would have helped.
 * Anyway, this interface seems pretty simple, so I just decided to write
 * a simple class that implements it.
 * 
 * @author Josef Spidlen
 */
public class DoubleNumberFormatter implements FJNumberFormatter {

	public String formattedDouble(Double value)
	{
		if(value == null) return "";
        else return String.valueOf(value.doubleValue());
	}

	public String formattedPercentDouble(double value) 
	{
		return String.valueOf(value * 100D);
	}

	public double parseDouble(String value) 
	{
		double ret = 0;
		try {
			ret = Double.parseDouble(value);
		} catch (Exception e) { }
		return ret; 
	}
	
	public DoubleNumberFormatter() 
	{ 
	}
}

