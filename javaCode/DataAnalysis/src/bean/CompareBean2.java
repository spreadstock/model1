package bean;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class CompareBean2 {
	
	private String name;
	private String name2;
	private int result = 0;
	private Map<String, Integer> resultList = new HashMap<String, Integer>();
	
	public void setName(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public void setName2(String name2) {
		this.name2 = name2;
	}

	public String getName2() {
		return name2;
	}

	public void setResult(int result) {
		this.result = result;
	}

	public int getResult() {
		return result;
	}

	public void setResultList(Map<String, Integer> resultList) {
		this.resultList = resultList;
	}

	public Map<String, Integer> getResultList() {
		return resultList;
	}

}
