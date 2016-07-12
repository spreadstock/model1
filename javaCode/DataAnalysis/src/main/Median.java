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


public class Median {
	
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
		
		
			
		String tmpName = "";
		Double tmpMedian = new Double(0);
		List<CompareBean> compareBeanList = new ArrayList<CompareBean>();
		CompareBean compareBean = new CompareBean();
		// set Median value
		for (int i = 0; i < dataList.size(); i++) {
			MonthlyBean bean = dataList.get(i);
			
			List<String> dailyDataList = bean.getCloseDataList();
			String[] str = (String[])dailyDataList.toArray(new String[dailyDataList.size()]);
			
//			String[] str2 = new String[]{"1", "5",  "2", "3", "4", "1"};		
//			System.out.println(Utils.getMedian2(str2));
//			System.out.println(bean.getName() + " " + bean.getMonth());
			bean.setMedian(Utils.getMedian(str));
			
			if (Utils.isEmpty(tmpName) || !tmpName.equals(bean.getName())) {
				if (!Utils.isEmpty(tmpName)) {
					compareBeanList.add(compareBean);
				}
				
				compareBean = new CompareBean();
				
				compareBean.setName(bean.getName());
				
				if (bean.getMedian() > tmpMedian) {
					compareBean.setResult(compareBean.getResult() + "1");
				} else {
					compareBean.setResult(compareBean.getResult() + "0");
				}
				tmpMedian = bean.getMedian();
				tmpName = bean.getName();
			} else {
				if (bean.getMedian() > tmpMedian) {
					compareBean.setResult(compareBean.getResult() + "1");
				} else {
					compareBean.setResult(compareBean.getResult() + "0");
				}
				tmpMedian = bean.getMedian();
			}
			
		}
		compareBeanList.add(compareBean);
		
		// result
		Map<String, String> resultMap = new HashMap<String, String>();
		for (int i = 0; i < compareBeanList.size(); i++) {
			CompareBean tmpCompareBean = compareBeanList.get(i);
			
			if (Utils.isEmpty(resultMap.get(tmpCompareBean.getResult()))) {
				resultMap.put(tmpCompareBean.getResult(), tmpCompareBean.getName());
			} else {
				resultMap.put(tmpCompareBean.getResult(), resultMap.get(tmpCompareBean.getResult()) + ", " +tmpCompareBean.getName());
			}
		}
		
		
		
		
//		 // output
//		 Iterator it = resultMap.entrySet().iterator();
//
//		   while (it.hasNext()) {
//		    Map.Entry entry = (Map.Entry) it.next();
//		    Object key = entry.getKey();
//		    Object value = entry.getValue();
//		    if (value.toString().contains(",")) {
//		    	System.out.println("key=" + key + " value=" + value);
//		    }
//		    
//
//		   }
		
		
	}
}
