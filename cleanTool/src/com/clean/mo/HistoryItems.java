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
            LogError.getLogger().error("calculateAverage error:" + this.toString(), e);
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
//        return date + "," + new BigDecimal(open).setScale(2, BigDecimal.ROUND_HALF_UP).toString() + ","
//                        + new BigDecimal(high).setScale(2, BigDecimal.ROUND_HALF_UP).toString() + ","
//                        + new BigDecimal(low).setScale(2, BigDecimal.ROUND_HALF_UP).toString() + ","
//                        + new BigDecimal(close).setScale(2, BigDecimal.ROUND_HALF_UP).toString() + "," + volume + ","
//                        + new BigDecimal(adjusted).setScale(2, BigDecimal.ROUND_HALF_UP).toString() + "," + average
//                        + "\n";
        return date + "," + new BigDecimal(open).setScale(2, BigDecimal.ROUND_HALF_UP).toString() + ","
        + new BigDecimal(high).setScale(2, BigDecimal.ROUND_HALF_UP).toString() + ","
        + new BigDecimal(low).setScale(2, BigDecimal.ROUND_HALF_UP).toString() + ","
        + new BigDecimal(close).setScale(2, BigDecimal.ROUND_HALF_UP).toString() + "," + volume + ","
        + new BigDecimal(adjusted).setScale(2, BigDecimal.ROUND_HALF_UP).toString() + "\n";
    }


    @Override
    public int hashCode()
    {
        final int prime = 31;
        int result = 1;
        long temp;
        temp = Double.doubleToLongBits(adjusted);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        temp = Double.doubleToLongBits(average);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        temp = Double.doubleToLongBits(close);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        result = prime * result + ((date == null) ? 0 : date.hashCode());
        temp = Double.doubleToLongBits(high);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        temp = Double.doubleToLongBits(low);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        temp = Double.doubleToLongBits(open);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        result = prime * result + ((previousDay == null) ? 0 : previousDay.hashCode());
        result = prime * result + (int) (volume ^ (volume >>> 32));
        return result;
    }


    @Override
    public boolean equals(Object obj)
    {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        HistoryItems other = (HistoryItems) obj;
        if (Double.doubleToLongBits(adjusted) != Double.doubleToLongBits(other.adjusted))
            return false;
        if (Double.doubleToLongBits(average) != Double.doubleToLongBits(other.average))
            return false;
        if (Double.doubleToLongBits(close) != Double.doubleToLongBits(other.close))
            return false;
        if (date == null)
        {
            if (other.date != null)
                return false;
        }
        else if (!date.equals(other.date))
            return false;
        if (Double.doubleToLongBits(high) != Double.doubleToLongBits(other.high))
            return false;
        if (Double.doubleToLongBits(low) != Double.doubleToLongBits(other.low))
            return false;
        if (Double.doubleToLongBits(open) != Double.doubleToLongBits(other.open))
            return false;
        if (previousDay == null)
        {
            if (other.previousDay != null)
                return false;
        }
        else if (!previousDay.equals(other.previousDay))
            return false;
        if (volume != other.volume)
            return false;
        return true;
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
