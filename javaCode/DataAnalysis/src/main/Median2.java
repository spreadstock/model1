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
import java.util.List;
import java.util.Map;

import bean.MonthlyBean;

import util.Utils;


public class Median2 {
	
	public static void main(String[] args) {
		System.out.println("hello world");
		String path = "C:\\Users\\esunnen\\Documents\\model1\\StockDatas\\Bluechips2";
		String startDate0 = "";
		String endDate0 = "";
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
		
//		Map<String, Map<Date, String>> totalDataMap = new HashMap<String, Map<Date, String>>();
		List<MonthlyBean> dataList = new ArrayList<MonthlyBean>();
		for (String l : list) {  
			System.out.println(l);
			try {
				BufferedReader br = new BufferedReader(new FileReader(new File(l)));
				String name = l.replace(path + "\\", "").replace(".txt", "");
//				Map<Date, String> dailyDataMap = null;
//				for (String line = br.readLine(); line != null; line = br.readLine()) {
//					dailyDataMap = new HashMap<Date, String>();
//	        		if (!line.contains("Date")) {
//	        			String[] str = line.split(",");
//	        			SimpleDateFormat sdf = new SimpleDateFormat("yyyy/MM/dd");
//	        			Date date = sdf.parse(str[0]); 
//	        			String value = str[4];
//	        			dailyDataMap.put(date, value);
//	        			System.out.println(name + "--------" + date + "--------" + value);     
//	        		}
//				}
//				totalDataMap.put(name, dailyDataMap);
				MonthlyBean monthlyBean = new MonthlyBean();
				
				for (String line = br.readLine(); line != null; line = br.readLine()) {
	        		if (!line.contains("Date")) {
	        			
	        			String[] str = line.split(",");
	        			if (!startDate.before(sdf.parse(str[0])) && !endDate.after(sdf.parse(str[0]))) {
	        				String month = str[0].substring(0, 7);
		        			if (Utils.isEmpty(monthlyBean.getMonth()) ||
		        					!monthlyBean.getMonth().equals(month)) {
		        				if (!Utils.isEmpty(monthlyBean.getMonth())) {
		        					dataList.add(monthlyBean);
		        				}
		        				monthlyBean = new MonthlyBean();
		        				
		        				monthlyBean.setName(name);
		        				monthlyBean.setMonth(month);
		        				// close value
		        				monthlyBean.getCloseDataList().add(str[4]);
		        			} else {
		        				// close value
		        				monthlyBean.getCloseDataList().add(str[4]);
		        			}
	        			}
	        		}
				}
				dataList.add(monthlyBean);
				
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}  
        	
		}
	}
}
