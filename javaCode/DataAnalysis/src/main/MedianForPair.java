package main;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import bean.CompareBean;
import bean.CompareBean2;
import bean.MonthlyBean;
import bean.PairBean;

import util.Utils;


public class MedianForPair {
	
	public static void main(String[] args) {
		System.out.println("hello world");
		String path = "C:\\Users\\esunnen\\Documents\\model1\\StockDatas\\Bluechips";
		String path2 = "C:\\Users\\esunnen\\Documents\\model1\\StockDatas\\input";
		String startDate0 = "2003/12/31";
		String endDate0 = "2005/01/01";
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
		Map<String, MonthlyBean> totalData = new HashMap<String, MonthlyBean>();
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
	        			
	        			if (sdf.parse(str[0]).after(startDate) && sdf.parse(str[0]).before(endDate)) {
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
			totalData.put(bean.getName(), bean);
			
			List<String> dailyDataList = bean.getCloseDataList();
			String[] str = (String[])dailyDataList.toArray(new String[dailyDataList.size()]);
			bean.setMedian(Utils.getMedian(str));
			
			if (Utils.isEmpty(tmpName) || !tmpName.equals(bean.getName())) {
				if (!Utils.isEmpty(tmpName)) {
					compareBeanList.add(compareBean);
				}
				tmpMedian = new Double(0);
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
			
//			if (Utils.isEmpty(resultMap.get(tmpCompareBean.getResult()))) {
//				resultMap.put(tmpCompareBean.getResult(), tmpCompareBean.getName());
//			} else {
//				resultMap.put(tmpCompareBean.getResult(), resultMap.get(tmpCompareBean.getResult()) + ", " +tmpCompareBean.getName());
//			}
			resultMap.put(tmpCompareBean.getName(), tmpCompareBean.getResult());
			
		}
		
		List<String> keyList = new ArrayList<String>();
		
		
		
		
		List<String> list2 = Utils.getFileList(path2);
		
		
		for (String l : list2) {
			try {
				BufferedReader br = new BufferedReader(new FileReader(new File(l)));
				String name = l.replace(path + "\\", "").replace(".txt", "");
				MonthlyBean monthlyBean = new MonthlyBean();
				
				int i = 0;
				for (String line = br.readLine(); line != null; line = br.readLine()) {
	        		if (line.contains("SZ") || line.contains("SH")) {
	        			String[] str = line.split(",");
	        			
	        			String str1 = str[0].trim();
	        			String str2 = str[1].trim();
	        			
	        			int tmp = 0;
	 				    for(int z = 0; z < resultMap.get(str1).length(); z++) {
	 					    if (resultMap.get(str1).substring(z, z +1).equals(resultMap.get(str2).substring(z, z +1))) {
	 						    tmp++;
	 					    }
	 				    }
	 				   
	 				    BigDecimal a = new BigDecimal(tmp);
	 				    BigDecimal b = new BigDecimal(resultMap.get(str1).length());
	 				    
	 				    
	        			System.out.println(str1 + ":"+ resultMap.get(str1) + "------" +str2 + ":"+ resultMap.get(str2) + "------" + a.divide(b, 4, BigDecimal.ROUND_HALF_UP));
//	        			System.out.println(resultMap.get(str2));
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
		
		
//		
//		 // output
//		 Iterator it = resultMap.entrySet().iterator();
//
//		   while (it.hasNext()) {
//		   	Map.Entry entry = (Map.Entry) it.next();
//		    Object key = entry.getKey();
//		    Object value = entry.getValue();
////		    System.out.println(key.toString().length());
//		    if (key.toString().length() == 120) {
//		    	keyList.add(key.toString());
//		    } 
//
//		   }
//		   
//		   System.out.println(keyList.size());
//		   
//		   List<CompareBean2> compareResultFor10YearsList = new ArrayList<CompareBean2>();
//		   for (int i = 0; i < keyList.size(); i++) {
//			   String str = keyList.get(i);
//			   
//			   CompareBean2 bean2 = new CompareBean2();
//			   // key
//			   bean2.setName(str);
//			   for (int j = 0; j < keyList.size(); j++) {
//				   String str2 = keyList.get(j);
//				   int tmp = 0;
//				   for(int z = 0; z < 120; z++) {
//					   if (str.substring(z, z +1).equals(str2.substring(z, z +1))) {
//						   tmp++;
//					   }
//				   }
//				   
//				   if (tmp > 100 && tmp != 120) {
//					   bean2.getResultList().put(str, tmp);
//					   bean2.setName2(str2);
//				   }
//				   
//			   }
//			   
//			   compareResultFor10YearsList.add(bean2);
////			   System.out.println(str);
//		   }
//		   
//		   List<PairBean> pairList = new ArrayList<PairBean>();
//		   for (int i = 0; i < compareResultFor10YearsList.size(); i++) {
//			   CompareBean2 bean2 = compareResultFor10YearsList.get(i);
//			   
//			   Map<String, Integer> map = bean2.getResultList();
//			   
//			   Iterator it2 = map.entrySet().iterator();
//			   PairBean bean = new PairBean();
//			   while (it2.hasNext()) {
//			   	Map.Entry entry = (Map.Entry) it2.next();
//			    Object key = entry.getKey();
//			    Object value = entry.getValue();
////			    System.out.println(key.toString().length());
//			    bean.setStr1(resultMap.get(bean2.getName2()));
//			    bean.setStr2(resultMap.get(resultMap.get(key)));
//			    System.out.println(resultMap.get(bean2.getName2()) + "  " + resultMap.get(key) + "  " + value);
////			    System.out.println(bean2.getResult());
//
//			   }
//			   pairList.add(bean);
//		   }
//		   
		   
		   
		   
		   
		   System.out.println("Finished!!");
		
		
	}
}
