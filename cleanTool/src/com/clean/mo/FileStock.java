/**
 * 
 */
package com.clean.mo;

import java.io.File;
import java.util.ArrayList;

/**
 * @author exubixu
 */
public class FileStock
{
    private String stockId;
    private String fileName;
    private String title;
    private File file;
    private ArrayList<HistoryItems> content = new ArrayList<HistoryItems>();


    public String getStockId()
    {
        return stockId;
    }


    public void setStockId(String stockId)
    {
        this.stockId = stockId;
    }


    public String getFileName()
    {
        return fileName;
    }

    public String getNewFileName()
    {
        return fileName.replace("#", "");
    }

    public void setFileName(String fileName)
    {
        this.fileName = fileName;
    }


    public String getTitle()
    {
        return title;
    }


    public void setTitle(String tile)
    {
        this.title = String.format("Date, %s.Open, %s.High, %s.Low %s.Close, %s.Volume, %s.Adjusted, %s.Average\n", tile, tile,
                                   tile, tile, tile, tile, tile);
    }


    public ArrayList<HistoryItems> getContent()
    {
        return content;
    }


    public void setContent(ArrayList<HistoryItems> content)
    {
        this.content = content;
    }


    public File getFile()
    {
        return file;
    }


    public void setFile(File file)
    {
        this.file = file;
    }


    public String toString()
    {
        return fileName + ":" + stockId + ":" + title;

    }


    public void addItem(HistoryItems item)
    {
        content.add(item);
    }


    public void fulfillPrevious(HistoryItems item)
    {
        if (!content.isEmpty())
        {
            HistoryItems previous = content.get(content.size() - 1);
            item.setPreviousDay(previous);
        }
    }

}
