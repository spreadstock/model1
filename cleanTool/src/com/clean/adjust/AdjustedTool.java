package com.clean.adjust;

import java.util.ArrayList;

import com.clean.mo.FileStock;
import com.clean.mo.HistoryItems;

public class AdjustedTool
{

    public void AdjustedData(ArrayList<FileStock> files)
    {
        for (FileStock file : files)
        {
            System.out.println("AdjustedData file:" + file.getFileName());
        }
    }


    public ArrayList<SectionStockData> getRangeData(FileStock file, ArrayList<Rule> ruleList)
    {
        ArrayList<SectionStockData> result = new ArrayList<SectionStockData>();
        int endIndex = 0;
        int startIndex = 0;
        for (Rule rule : ruleList)
        {
            for (int i = startIndex; i < file.getContent().size(); i++)
            {
                HistoryItems item = file.getContent().get(i);
                if (item.getDate().equals(rule.getDate()))
                {
                    endIndex = i;
                    break;
                }
            }
            ArrayList<HistoryItems> section = new ArrayList<HistoryItems>();
            section.addAll(file.getContent().subList(startIndex, endIndex));
            SectionStockData stockdata = new SectionStockData();
            stockdata.setSegmentData(section);
            result.add(stockdata);
            startIndex = endIndex;
            for(SectionStockData data : result)
            {
              //Former complex Rights
                data.getRuleInSequence().add(0,rule);
            }
        }
        
        if(startIndex <= file.getContent().size() -1)
        {
            ArrayList<HistoryItems> section = new ArrayList<HistoryItems>();
            section.addAll(file.getContent().subList(startIndex, file.getContent().size()));
            SectionStockData stockdata = new SectionStockData();
            stockdata.setSegmentData(section);
            result.add(stockdata);
        }
        return result;
    }

}
