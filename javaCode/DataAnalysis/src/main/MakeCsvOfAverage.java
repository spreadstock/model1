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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import bean.CompareBean;
import bean.MonthlyBean;

import util.Utils;


public class MakeCsvOfAverage {
	
	public static void main(String[] args) {
		System.out.println("hello world");
		String path = "C:\\Users\\esunnen\\Documents\\model1\\StockDatas\\Bluechips";
		String startDate0 = "2001/01/01";
		String endDate0 = "2016/10/01";
		List<String> list = Utils.getFileList(path);
		
		SimpleDateFormat sdf = new SimpleDateFormat("yyyy/MM/dd");
		Date startDate = null;
		Date endDate = null;
		try {
			startDate = sdf.parse(startDate0);
			endDate = sdf.parse(endDate0);
		} catch (ParseException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		
		List<MonthlyBean> dataList = new ArrayList<MonthlyBean>();
		for (String l : list) {
			try {
				BufferedReader br = new BufferedReader(new FileReader(new File(l)));
				String name = l.replace(path + "\\", "").replace(".txt", "");
				MonthlyBean monthlyBean = new MonthlyBean();
				
				int i = 0;
				for (String line = br.readLine(); line != null; line = br.readLine()) {
	        		if (!line.contains("Date")) {
	        			String[] str = line.split(",");
	        			if (i == 0 && endDate.before(sdf.parse(str[0]))) {
	        				break;
	        			}
	        			i++;
	        			if (!startDate.after(sdf.parse(str[0])) && !endDate.before(sdf.parse(str[0]))) {
	        				String month = str[0].substring(0, 7);
		        			if (Utils.isEmpty(monthlyBean.getMonth()) ||
		        					!monthlyBean.getMonth().equals(month)) {
		        				if (!Utils.isEmpty(monthlyBean.getMonth())) {
		        					dataList.add(monthlyBean);
		        				}
		        				monthlyBean = new MonthlyBean();
		        				
		        				monthlyBean.setName(name);
		        				monthlyBean.setMonth(month);
		        				monthlyBean.setFirstDay(str[0]);
		        				// close value
		        				monthlyBean.getCloseDataList().add(str[4]);
		        			} else {
		        				// close value
		        				monthlyBean.getCloseDataList().add(str[4]);
		        			}
	        			}
	        		}
				}
				if (!Utils.isEmpty(monthlyBean.getName())) {
					dataList.add(monthlyBean);
				}
				
				
				
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}  
        	
		}
		
		
		// set average value
		for (int i = 0; i < dataList.size(); i++) {
			MonthlyBean bean = dataList.get(i);
			
			List<String> dailyDataList = bean.getCloseDataList();
			String[] str = (String[])dailyDataList.toArray(new String[dailyDataList.size()]);
			bean.setAverage(Utils.getAverage(str));
			
		}
		
		
		// make csv
		String tmpName = "";
		List<Map<String, String>> exportData = new ArrayList<Map<String, String>>();
		for (int i = 0; i < dataList.size(); i++) {
			MonthlyBean monthlyBean = dataList.get(i);
			if (Utils.isEmpty(tmpName)) {
				tmpName = monthlyBean.getName();
			}
			if (!tmpName.equals(monthlyBean.getName())) {
				
				LinkedHashMap map = new LinkedHashMap();
		        map.put("1", "Date");
		        map.put("2", tmpName + ".Close");
				
		        Utils.createCSVFile(exportData, map, "C:\\Users\\esunnen\\Documents\\model1\\StockDatas\\Average\\", tmpName);
		        
		        exportData  = new ArrayList<Map<String, String>>();
				tmpName = monthlyBean.getName();
				Map<String, String> row = new LinkedHashMap<String, String>();
				row.put("1", monthlyBean.getFirstDay());
				row.put("2", String.valueOf(monthlyBean.getAverage()));
				exportData.add(row);
			} else {
				Map<String, String> row = new LinkedHashMap<String, String>();
				row.put("1", monthlyBean.getFirstDay());
				row.put("2", String.valueOf(monthlyBean.getAverage()));
				exportData.add(row);
			}
			
		}
		
		LinkedHashMap map = new LinkedHashMap();
        map.put("1", "Date");
        map.put("2", tmpName + ".Close");
        Utils.createCSVFile(exportData, map, "C:\\Users\\esunnen\\Documents\\model1\\StockDatas\\Average\\", tmpName);
        
        System.out.println("Finished!!");
		
		
		
		
	}
}
