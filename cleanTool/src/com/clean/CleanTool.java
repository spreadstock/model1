/**
 * 
 */
package com.clean;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.clean.mo.FileStock;
import com.clean.mo.HistoryItems;

public class CleanTool
{
    private static final String CONF_CONF_PROPERTIES = "conf/conf.properties";
    static String path;
    static String newPath;
    static int COL_DATE = 0;
    static int COL_OPEN = 1;
    static int COL_HIGH = 2;
    static int COL_LOW = 3;
    static int COL_CLOSE = 4;
    static int COL_VOLUME = 5;
    static int COL_ADJUSTED = 6;


    public static void main(String[] args)
    {
        System.out.println("begin to cleanup data");
        try
        {
            init();
        }
        catch (FileNotFoundException e)
        {
            LogError.getLogger().error("file not found", e);
            e.printStackTrace();
        }
        ArrayList<FileStock> files = getAllFiles();
        for (FileStock file : files)
        {
            System.out.println("train file:" + file.getFileName());
            readFileByLines(file);
            writeToFile(file);
        }
        System.out.println("finish cleanup data");
    }


    public static void init() throws FileNotFoundException
    {
        Properties prop = new Properties();
        InputStream in = new BufferedInputStream(new FileInputStream(CONF_CONF_PROPERTIES));
        try
        {
            prop.load(in);
            path = prop.getProperty("path").trim();
            newPath = prop.getProperty("newPath").trim();
            COL_DATE = prop.getProperty("COL_DATE") == null ? 0 : Integer.parseInt(prop.getProperty("COL_DATE").trim());
            COL_OPEN = prop.getProperty("COL_OPEN") == null ? 1 : Integer.parseInt(prop.getProperty("COL_OPEN").trim());
            COL_HIGH = prop.getProperty("COL_HIGH") == null ? 2 : Integer.parseInt(prop.getProperty("COL_HIGH").trim());
            COL_LOW = prop.getProperty("COL_LOW") == null ? 3 : Integer.parseInt(prop.getProperty("COL_LOW").trim());
            COL_CLOSE = prop.getProperty("COL_CLOSE") == null ? 4 : Integer.parseInt(prop.getProperty("COL_CLOSE")
                            .trim());
            COL_VOLUME = prop.getProperty("COL_VOLUME") == null ? 5 : Integer.parseInt(prop.getProperty("COL_VOLUME")
                            .trim());
            COL_ADJUSTED = prop.getProperty("COL_ADJUSTED") == null ? 6 : Integer.parseInt(prop
                            .getProperty("COL_ADJUSTED").trim());

        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
    }


    public static void readFileByLines(FileStock file)
    {
        BufferedReader reader = null;
        try
        {
            reader = new BufferedReader(new FileReader(file.getFile()));
            String tempString = null;
            while ((tempString = reader.readLine()) != null)
            {
                trainLine(file, tempString);
            }
            reader.close();
        }
        catch (Exception e)
        {
            LogError.getLogger().error("read error", e);
            e.printStackTrace();
        }
        finally
        {
            if (reader != null)
            {
                try
                {
                    reader.close();
                }
                catch (IOException e1)
                {
                }
            }
        }
    }


    private static ArrayList<FileStock> getAllFiles()
    {
        ArrayList<FileStock> files = new ArrayList<FileStock>();
        File file = new File(path);
        File[] tempList = file.listFiles();
        for (int i = 0; i < tempList.length; i++)
        {
            if (tempList[i].isFile())
            {
                files.add(trainStockFile(tempList[i]));
            }
        }
        return files;
    }


    private static FileStock trainStockFile(File file)
    {
        FileStock filestock = new FileStock();
        filestock.setFileName(file.getName());
        String stockId = file.getName().split("#")[1].split("\\.")[0];
        filestock.setStockId(stockId);
        filestock.setTitle(stockId);
        filestock.setFile(file);
        return filestock;
    }


    private static void writeToFile(FileStock file)
    {
        FileWriter fw = null;
        try
        {
            File dest = new File(newPath);
            dest.deleteOnExit();
            dest.mkdir();

            fw = new FileWriter(newPath + "/" + file.getFileName());

            long begin3 = System.currentTimeMillis();

            fw.write(file.getTitle());
            for (HistoryItems line : file.getContent())
            {
                fw.write(line.toString());
            }
            long end3 = System.currentTimeMillis();

            System.out.println("FileWriter time:" + (end3 - begin3) + " ms");
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }

        finally
        {
            try
            {
                fw.close();
            }
            catch (Exception e)
            {
                e.printStackTrace();
            }

        }
    }


    private static boolean isChinese(char c)
    {
        Character.UnicodeBlock ub = Character.UnicodeBlock.of(c);
        if (ub == Character.UnicodeBlock.CJK_UNIFIED_IDEOGRAPHS
                        || ub == Character.UnicodeBlock.CJK_COMPATIBILITY_IDEOGRAPHS
                        || ub == Character.UnicodeBlock.CJK_UNIFIED_IDEOGRAPHS_EXTENSION_A
                        || ub == Character.UnicodeBlock.CJK_UNIFIED_IDEOGRAPHS_EXTENSION_B
                        || ub == Character.UnicodeBlock.CJK_SYMBOLS_AND_PUNCTUATION
                        || ub == Character.UnicodeBlock.HALFWIDTH_AND_FULLWIDTH_FORMS
                        || ub == Character.UnicodeBlock.GENERAL_PUNCTUATION)
        {
            return true;
        }
        return false;
    }


    private static boolean isContainChinese(String strName)
    {
        char[] ch = strName.toCharArray();
        for (int i = 0; i < ch.length; i++)
        {
            char c = ch[i];
            if (isChinese(c))
            {
                return true;
            }
        }
        return false;
    }


    private static void trainLine(FileStock file, String tempString)
    {
        try
        {
            if (!validate(tempString))
            {
                LogError.getLogger().debug(file.getFileName() + ":   " + tempString);
                return;
            }
            generateHistoryItems(file, tempString);
        }
        catch (Exception e)
        {
            LogError.getLogger().error("trainLine error:"+ file.getFileName() +"  "+ tempString , e);
            e.printStackTrace();
        }
    }


    private static void generateHistoryItems(FileStock file, String tempString)
    {
        HistoryItems item = new HistoryItems();
        String[] tmp = tempString.split("\\s+");
        item.setDate(tmp[COL_DATE]);
        item.setOpen(new Double(tmp[COL_OPEN]));
        item.setHigh(new Double(tmp[COL_HIGH]));
        item.setLow(new Double(tmp[COL_LOW]));
        item.setClose(new Double(tmp[COL_CLOSE]));
        item.setVolume(new Long(tmp[COL_VOLUME]));
        item.setAdjusted(new Double(tmp[COL_ADJUSTED]));
        
        if(isValidDataValue(item))
        {
            item.calculateAverage();
            file.fulfillPrevious(item);
            file.addItem(item);  
        }
        else
        {
            LogError.getLogger().debug("Filtered "+file.getFileName() + ":   " + tempString);
        }
    }


    private static boolean isValidDataValue(HistoryItems item)
    {
        if(item.getVolume() == 0 || item.getAdjusted() == 0)
        {
            return false;
        }
        return true;
    }


    private static boolean validate(String tempString)
    {
        if (isContainChinese(tempString))
        {
            return false;
        }
        if (!isValidDataLine(tempString))
        {
            return false;
        }
        return true;
    }


    private static boolean isValidDataLine(String tempString)
    {
        Pattern pattern = Pattern.compile("[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}(\\s+\\-{0,1}\\d+(\\.\\d+)?){6}");
        Matcher matcher = pattern.matcher(tempString);
        if (!matcher.matches())
        {
            return false;
        }
        if (tempString.split("\\s+").length != 7)
        {
            return false;
        }
        return true;
    }
}
