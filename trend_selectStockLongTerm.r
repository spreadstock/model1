library(quantstrat)
#################################################################
#短期均线（5，10，20）在长期均线之上（60，250）
isShorternMAOverLongTermMARule<- function(stock, latestDay=1)
{
  if(nrow(stock) < 251)
  {
    return (FALSE)
  }
  MA<-Cl(stock)
  MA$SMA250 <- SMA(Cl(stock),250)
  MA$SMA5 <- SMA(Cl(stock),5)
  MA$SMA10 <- SMA(Cl(stock),10)
  MA$SMA20 <- SMA(Cl(stock),20)
  MA$SMA60 <- SMA(Cl(stock),60)
	
  result <- na.omit(last(MA,latestDay)) 
  result <- as.matrix(result)
  for (i in 1:nrow(result)) 
  {
    ma250 <- as.numeric(result[i,2])
    ma5 <- as.numeric(result[i,3])
    ma10 <- as.numeric(result[i,4])
    ma20 <- as.numeric(result[i,5])
	ma60 <- as.numeric(result[i,6])
    if((ma5 < ma250) || (ma10 < ma250) || (ma20 < ma250) || (ma5 < ma60) || (ma10 < ma60) || (ma20 < ma60)) { return (FALSE) }
  }
  return (TRUE)
}
#长期均线是向上发散（最近的3天每天都比前一天高）
matchTrendGrow <- function(x, targetGrowDay=3) 
{ 
  if(nrow(stock) < 251)
  {
    return (FALSE)
  }
  MA<-Cl(stock)
  MA$SMA250 <- SMA(Cl(stock),250)
  MA$SMA60 <- SMA(Cl(stock),60)
	
  result <- na.omit(last(MA,targetGrowDay)) 
  result <- as.matrix(result)

  for (i in 1:(nrow(result)-1))
  {
    if(result[i,2] > result[i+1,2])
    {
      return (FALSE)
    }
	if(result[i,3] > result[i+1,3])
    {
      return (FALSE)
    }
  }
  return (TRUE)
}
#20个交易日内有一次Cross（5，10，20天向上突破60天线）
crossHappened <- function(stock, backDate=20)
{
  if(nrow(stock) < 61)
  {
    return (FALSE)
  }
  MA<-Cl(stock)
  MA$SMA5 <- SMA(Cl(stock),5)
  MA$SMA10 <- SMA(Cl(stock),10)
  MA$SMA20 <- SMA(Cl(stock),20)
  MA$SMA60 <- SMA(Cl(stock),60)
  result <- na.omit(last(MA,backDate)) 
  result <- as.matrix(result)
  for (i in 1:nrow(result)) 
  {
    ma5 <- as.numeric(result[i,2])
    ma10 <- as.numeric(result[i,3])
    ma20 <- as.numeric(result[i,4])
	ma60 <- as.numeric(result[i,5])
    if()
  }  
  
}

#################################################################
#成交量阶段性温和放量，最近20天的交易量比前一个20天放大30%以上；
matchVolumGrowRule<- function(stock, backDate=20, pct=0.3)
{
  lastbackDate1 <- last(Vo(stock),backDate)
  lastbackDate2 <- Vo(stock[(nrow(stock)-2*backDate+1):(nrow(stock)-backDate)])
  
  VoSumbackDate1 <- sum(lastbackDate1)
  VoSumbackDate2 <- sum(lastbackDate2)
  #print(VoSumbackDate1)
  #print(VoSumbackDate2)
  if(VoSumbackDate1 > (VoSumbackDate2*(1+pct))) { return (TRUE) } else { return (FALSE) }
  
}

#股票价格在60个交易日内上涨小于25%, 60日前的交易价格小于60日高点
matchGrowRule<- function(stock, pct=0.25, backDate=60)
{
  lastbackDate <- last(Cl(stock),backDate)
  startPrice <- lastbackDate[1]
  maxPrice <- max(lastbackDate)
  #print(startPrice)
  #print(maxPrice)
  if(startPrice*(1+pct) > maxPrice) { return (TRUE) } else { return (FALSE) }
}

#################################################################
#最近10个交易日内的最高点是60天内的最高点；
matchHighPointRule<- function(stock, highCurrent=10, highLong=60)
{
  lasthighCurrent <- last(Cl(stock),highCurrent)
  lasthighLong <- last(Cl(stock),highLong)
  #print(max(lasthighCurrent))
  #print(max(lasthighLong)))
  if(max(lasthighCurrent) == max(lasthighLong)) { return (TRUE) } else { return (FALSE) }
  
}
#################################################################
#最近20个交易日的低点比前面出现的低点高（第一，二个20个交易日的低点）。
isLowestPointRule<- function(stock, backDate=20)
{
  lastbackDate1 <- last(Cl(stock),backDate)
  lastbackDate2 <- Cl(stock[(nrow(stock)-2*backDate+1):(nrow(stock)-backDate)])
  lastbackDate3 <- Cl(stock[(nrow(stock)-3*backDate+1):(nrow(stock)-2*backDate)])
  #print(min(lastbackDate1))
  #print(min(lastbackDate2))
  #print(min(lastbackDate3))
  if( (min(lastbackDate1) > min(lastbackDate2)) & (min(lastbackDate1) > min(lastbackDate3))) { return (TRUE) } else { return (FALSE) }
  
}


startDate <- '2001-01-01'
endDate <- '2016-9-31'

getTrendMatchStocks <- function(stock.folder){
stock_symbols  <- listStocksFromDir(stock.folder)

result <- list(NA)
for(symbol in stock_symbols) 
{ 
  print(symbol)
  stock <-  loadStock(stock.folder, symbol, operation.name="all")

  if(nrow(stock) > 60){
    stock <- subsetByDateRange(stock,startDate,endDate)
    if(nrow(stock) > 60){
    if(isLowestPointRule(stock) &
       matchHighPointRule(stock)) {
      result <- c(result, symbol)
    }}
  }

  rm(stock) 
}
print("done!")
  return (result[-1])  
}

source.folder <- 'C:/Users/exubixu/Desktop/Imp/git_new/model1/'
source(paste0(source.folder,"commonPackages.r"))
stock.folder <- 'C:/Users/exubixu/Desktop/Imp/git_new/model1/StockDatas/2016-08-09-Later_Rehabilitation_Cleaned/'
mySelected <- getTrendMatchStocks(stock.folder)
#write.csv(x=mySelected, file="C:/Users/exubixu/Desktop/new1/test.csv", row.names = TRUE)

