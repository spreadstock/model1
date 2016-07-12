package bean;

import java.util.ArrayList;
import java.util.List;

public class MonthlyBean {
	
	private String name;
	private String month;
	private String firstDay;
	private List<String> closeDataList = new ArrayList<String>();
	private double median;
	private double average;

	public void setName(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public void setMonth(String month) {
		this.month = month;
	}

	public String getMonth() {
		return month;
	}

	public void setCloseDataList(List<String> closeDataList) {
		this.closeDataList = closeDataList;
	}

	public List<String> getCloseDataList() {
		return closeDataList;
	}

	public void setMedian(double median) {
		this.median = median;
	}

	public double getMedian() {
		return median;
	}

	public void setFirstDay(String firstDay) {
		this.firstDay = firstDay;
	}

	public String getFirstDay() {
		return firstDay;
	}

	public void setAverage(double average) {
		this.average = average;
	}

	public double getAverage() {
		return average;
	}
}
