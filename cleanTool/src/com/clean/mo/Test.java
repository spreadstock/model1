package com.clean.mo;

import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

public class Test
{
    private static double high = 15.55;
    private static double low = 15.50;
    private static double close = 15.20;
    private static long volume = 1701100;
    private static BigDecimal adjusted = new BigDecimal(25977860.00);
    private static double average;


    public static void main(String[] args)
    {
//        double avg = adjusted.divide(new BigDecimal(volume), 4, BigDecimal.ROUND_HALF_UP).doubleValue();
//        double diff = new BigDecimal(avg).subtract(new BigDecimal(close)).abs().doubleValue();
//        System.out.println(avg);
//        System.out.println(diff);
//        double perc = new BigDecimal(diff).divide(new BigDecimal(close), 4, BigDecimal.ROUND_HALF_UP).doubleValue();
//        System.out.println(perc);
//
//        if (perc > 0.0001)
//        {
//            average = new BigDecimal(high).add(new BigDecimal(low)).divide(new BigDecimal("2"), 4, BigDecimal.ROUND_HALF_UP).doubleValue();
//        }
//        else
//        {
//            average = new BigDecimal(avg).doubleValue();
//        }
        System.out.println(adjusted.setScale(2,BigDecimal.ROUND_HALF_UP).toString());

//        String day1 = "2002/12/31";
//        String day2 = "2003/01/01";
//        
//        SimpleDateFormat format = new SimpleDateFormat("yyyy/MM/dd");
//        try
//        {
//            String[]  parsedDay = day1.split("/");
//            
//            Calendar calendar = new GregorianCalendar(Integer.parseInt(parsedDay[0]), Integer.parseInt(parsedDay[1])-1, Integer.parseInt(parsedDay[2]),0,0,0);   
//            calendar.add(Calendar.DAY_OF_MONTH, +1);
//            Date aaa = calendar.getTime();   
//            System.out.println("result is:"+format.format(aaa));
//            System.out.println("now is:" + format.format(now));
//            System.out.println("date is:" + format.format(date));
            
//        }
//        catch (Exception e)
//        {
//            e.printStackTrace();
//        }

    }
}
