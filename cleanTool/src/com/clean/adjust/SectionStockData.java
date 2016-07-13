package com.clean.adjust;

import java.util.ArrayList;

import com.clean.mo.HistoryItems;

public class SectionStockData
{
    ArrayList<HistoryItems> segmentData = null;
    ArrayList<Rule> ruleInSequence = new ArrayList<Rule>();


    public ArrayList<HistoryItems> getSegmentData()
    {
        return segmentData;
    }


    public void setSegmentData(ArrayList<HistoryItems> segmentData)
    {
        this.segmentData = segmentData;
    }


    public ArrayList<Rule> getRuleInSequence()
    {
        return ruleInSequence;
    }


    public void setRuleInSequence(ArrayList<Rule> ruleInSequence)
    {
        this.ruleInSequence = ruleInSequence;
    }

}
