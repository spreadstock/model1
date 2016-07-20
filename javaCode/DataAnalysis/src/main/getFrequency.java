package main;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import bean.CompareBean;
import bean.MonthlyBean;

import util.Utils;


public class getFrequency {
	
	public static void main(String[] args) {
		System.out.println("hello world");
		String path = "C:\\Users\\esunnen\\Documents\\model1\\StockDatas\\test";
		List<String> list = Utils.getFileList(path);
		
//		List<String> nameList = new ArrayList<String>();
		Map<String, Integer> nameMap = new HashMap<String, Integer>();
		
		for (String l : list) {
			try {
				BufferedReader br = new BufferedReader(new FileReader(new File(l)));
				String name = l.replace(path + "\\", "").replace(".txt", "");
				
				for (String line = br.readLine(); line != null; line = br.readLine()) {
	        		if (!line.contains("Date")) {
	        			String[] str = line.split(",");
	        			
	        			for (int j = 0; j < str.length; j++) {
	        				String nameStr = str[j];
	        				
	        				if (nameMap.get(nameStr) == null){
	        					nameMap.put(nameStr, 0);
//	        					System.out.println("111"+nameStr);
	        				} else {
	        					nameMap.put(nameStr, nameMap.get(nameStr) + 1);
//	        					System.out.println("222"+nameStr);
	        				}
	        			}
				}

				}
				
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}  
        	
		}
		
		
		
		 // output
		 Iterator it = nameMap.entrySet().iterator();

		   while (it.hasNext()) {
		    Map.Entry entry = (Map.Entry) it.next();
		    Object key = entry.getKey();
		    Object value = entry.getValue();
	
//	    		System.out.println(key.toString().length());
	    			System.out.println("key=" + key + "\t " + value);
	    		

		    	
		    	
//		    	System.out.println(value);
		    

		   }
		   System.out.println("Finished!!");
		
		
	}
}
