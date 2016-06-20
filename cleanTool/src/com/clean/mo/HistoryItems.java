/**
 * 
 */
package com.clean.mo;

import java.math.BigDecimal;

import com.clean.LogError;

/**
 * @author exubixu
 */
public class HistoryItems
{
    private String date;
    private double open;
    private double high;
    private double low;
    private double close;
    private long volume;
    private double adjusted;
    private double average;
    private HistoryItems previousDay;


    public String getDate()
    {
        return date;
    }


    public void setDate(String date)
    {
        this.date = date;
    }


    public double getOpen()
    {
        return open;
    }


    public void setOpen(double open)
    {
        this.open = open;
    }


    public double getHigh()
    {
        return high;
    }


    public void setHigh(double high)
    {
        this.high = high;
    }


    public double getLow()
    {
        return low;
    }


    public void setLow(double low)
    {
        this.low = low;
    }


    public double getClose()
    {
        return close;
    }


    public void setClose(double close)
    {
        this.close = close;
    }


    public long getVolume()
    {
        return volume;
    }


    public void setVolume(long volume)
    {
        this.volume = volume;
    }


    public double getAdjusted()
    {
        return adjusted;
    }


    public void setAdjusted(double adjusted)
    {
        this.adjusted = adjusted;
    }


    public void calculateAverage()
    {
        try
        {
            average = new BigDecimal(high).add(new BigDecimal(low))
                            .divide(new BigDecimal("2"), 2, BigDecimal.ROUND_HALF_UP).doubleValue();
            double closeTmp = previousDay == null ? close : previousDay.getClose();
            if (volume == 0 || closeTmp == 0)
            {
                return;
            }

            double avg = new BigDecimal(adjusted).divide(new BigDecimal(volume), 4, BigDecimal.ROUND_HALF_UP)
                            .doubleValue();
            double diff = new BigDecimal(avg).subtract(new BigDecimal(close)).abs().doubleValue();
            double perc = new BigDecimal(diff).divide(new BigDecimal(closeTmp), 4, BigDecimal.ROUND_HALF_UP)
                            .doubleValue();
            if (perc > 0.1)
            {
                return;
            }
            else
            {
                average = new BigDecimal(avg).doubleValue();
            }
        }
        catch (Exception e)
        {
            LogError.getLogger().error("calculateAverage error:"+this.toString(), e);
            e.printStackTrace();
        }
    }


    public double getAverage()
    {
        return average;
    }


    public void setAverage(double average)
    {
        this.average = average;
    }


    public String toString()
    {
        return date + "," + new BigDecimal(open).setScale(2,BigDecimal.ROUND_HALF_UP).toString() + "," + new BigDecimal(high).setScale(2,BigDecimal.ROUND_HALF_UP).toString() + "," + new BigDecimal(low).setScale(2,BigDecimal.ROUND_HALF_UP).toString() + "," + new BigDecimal(close).setScale(2,BigDecimal.ROUND_HALF_UP).toString() + "," + volume + "," + new BigDecimal(adjusted).setScale(2,BigDecimal.ROUND_HALF_UP).toString() + "," + average
                        + "\n";
    }


    public HistoryItems getPreviousDay()
    {
        return previousDay;
    }


    public void setPreviousDay(HistoryItems previousDay)
    {
        this.previousDay = previousDay;
    }

}
