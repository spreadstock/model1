package util;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;

public class Utils {

	public static double getMedian (String[] a) {
		ArrayList<Double> arr=new ArrayList<Double>();
	    for(int i=0;i<a.length;i++){
	        arr.add(Double.valueOf(a[i]));
	         
	    }
	    Collections.sort(arr);
	    double j = arr.get(a.length/2);
	    if(a.length%2==0){
	        j=(arr.get(a.length/2 - 1)  +arr.get(a.length/2))/2;
	    }else{
	        j=arr.get(a.length/2) ;
	    }
	    
	    BigDecimal b = new BigDecimal(j);  
	    
	    return b.setScale(3, BigDecimal.ROUND_HALF_UP).doubleValue();
	}
	
	public static double getAverage (String[] a) {
		ArrayList<Double> arr=new ArrayList<Double>();
		
		Double sum = Double.valueOf(0);
		for (int i = 0; i < a.length; i++) {
			sum += Double.valueOf(a[i]);
		}
		BigDecimal b = new BigDecimal(sum / a.length);  
		
		return b.setScale(2, BigDecimal.ROUND_HALF_UP).doubleValue();
		
	}
	
	public static List<String> getFileList(String path) {  
		  
		File file = new File(path);
        List<String> result = new ArrayList<String>();  
  
        if (!file.isDirectory()) {  
            System.out.println(file.getAbsolutePath());  
            result.add(file.getAbsolutePath());  
        } else {  
            File[] directoryList = file.listFiles();  
            for (int i = 0; i < directoryList.length; i++) {  
                result.add(directoryList[i].getPath());  
            }  
        }  
  
        return result;  
    }

	public static boolean isEmpty(String str) {  
		if (str == null || "".equals(str)) {
			return true;
		} else {
			return false;
		}
    }
	
	/**
	 * class名：ArrayManagement
	 * class说明：此类用于对整数数组的一系列处理
	 * 备注：我是新手 所以不要太难的（要用到methods）求一个程序：
	 * 要求使用者输入随机数的范围 和随机数的个数求总和、最值、平均数、众数和中位数
	 * @author Jr
	 *
	 */
	public static class ArrayManagement {
		/**
		 * method名：randomArray
		 * method说明：此方法用于随机生成一个整数数组
		 * @param <limit> <数组的最大值，即数组范围>
		 * @param <amount> <数组的大小> 
		 * @return <array> <返回一个数组>
		 */
		private static int[] randomArray(int limit, int amount) {
			int[] array = new int[amount];
			for (int i = 0; i < array.length; i++) {
				array[i] = new Random().nextInt(limit);
			}
			return array;
		}
		
		/**
		 * method名：arraySum
		 * method说明：此方法用于计算总和
		 * @param <array> <一个整数数组>
		 * @return <sum> <返回数组之和>
		 */
		private static int arraySum(int[] array) {
			int sum = 0;
			for (int i = 0; i < array.length; i++) {
				sum += array[i];
			}
			return sum;
		}
		
		/**
		 * method名：maxOfArray
		 * method说明：此方法用于找出数组最大值
		 * @param <array> <一个整数数组>
		 * @return <max> <数组中的最大数>
		 */
		private static int maxOfArray(int[] array) {
			int max = 0;
			for (int i = 0; i < array.length; i++) {
				if (max < array[i]) {
					max = array[i];
				}
			}
			return max;
		}
		
		/**
		 * method名：miniOfArray
		 * method说明：此方法用于找出数组最小值
		 * @param <array> <一个整数数组>
		 * @return <mini> <数组中的最小数>
		 */
		private static int miniOfArray(int[] array) {
			int mini = 0;
			for (int i = 0; i < array.length; i++) {
				if (mini > array[i]) {
					mini = array[i];
				}
			}
			return mini;
		}
		
		/**
		 * method名：averageOfArray
		 * method说明：此方法用于求出数组平均数
		 * @param <array> <一个整数数组>
		 * @return <average> <数组的平均数>
		 */
		private static int averageOfArray(int[] array) {
			int sum = arraySum(array);	// 调用之前写过的数组求和方法
			int average = sum / array.length;
			return average;
		}
		

	}

	public static void createCSVFile(List exportData, LinkedHashMap rowMapper,
            String outPutPath, String filename) {

        File csvFile = null;
        BufferedWriter csvFileOutputStream = null;
        try {
            csvFile = new File(outPutPath + filename + ".txt");
            // csvFile.getParentFile().mkdir();
            File parent = csvFile.getParentFile();
            if (parent != null && !parent.exists()) {
                parent.mkdirs();
            }
            csvFile.createNewFile();

            // GB2312使正确读取分隔符","
            csvFileOutputStream = new BufferedWriter(new OutputStreamWriter(
                    new FileOutputStream(csvFile), "GB2312"), 1024);
            // 写入文件头部
            for (Iterator propertyIterator = rowMapper.entrySet().iterator(); propertyIterator
                    .hasNext();) {
                java.util.Map.Entry propertyEntry = (java.util.Map.Entry) propertyIterator
                        .next();
                csvFileOutputStream.write(propertyEntry.getValue().toString());
                if (propertyIterator.hasNext()) {
                    csvFileOutputStream.write(",");
                }
            }
            csvFileOutputStream.newLine();

           


            // 写入文件内容
            for (Iterator iterator = exportData.iterator(); iterator.hasNext();) { 
            	Map row = (Map) iterator.next(); 
                for (Iterator propertyIterator = row.entrySet().iterator(); propertyIterator.hasNext();) { 
                    Entry propertyEntry = (Entry) propertyIterator.next(); 
                    csvFileOutputStream.write(propertyEntry.getValue().toString()); 
                   if (propertyIterator.hasNext()) { 
                       csvFileOutputStream.write(","); 
                    } 
               } 
                if (iterator.hasNext()) { 
                   csvFileOutputStream.newLine(); 
                } 
           } 
            csvFileOutputStream.flush(); 
        } catch (Exception e) { 
           e.printStackTrace(); 
        } finally { 
           try { 
                csvFileOutputStream.close(); 
            } catch (IOException e) { 
               e.printStackTrace();
           } 
       } 
//        return csvFile;
    
		// TODO Auto-generated method stub
		
	}
	


}
	

