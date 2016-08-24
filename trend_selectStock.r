library(quantstrat)

#最近一段时间，涨的天数比跌的天数多
isGainRateOverSlip <- function(stock, percentage=0.5)
{
  b <- Cl(stock)  
  result <- as.matrix(diff(b))
  result <- na.omit(result) 
  up <- 0
  down <- 0
  for (i in 1:nrow(result)) 
  {
    zz <- as.numeric(result[i,1])
    if((zz > 0)) up=up+1 else down=down+1
  }
  if(up/nrow(stock) >= percentage) { return (TRUE) } else { return (FALSE) }
}

#最近一天是涨的，并且最近的10天里涨的天数多余跌的天数
isGainRuleLastDays<- function(stock, latestDay=10)
{
  last <- last(Cl(stock),latestDay)
  result <- as.matrix(diff(last))
  result <- na.omit(result) 
  
  #last day is up, in 10 days, up is greater than down
  if(result[nrow(result),1] > 0 && isGainRateOverSlip(last,0.5)) { return (TRUE) } else { return (FALSE) }
}

#################################################################
#最近三天交易量都比前一天的大并且总交易量比前三天的总交易量高1.2/1.5倍以上
isGainolumeRuleLastDays<- function(stock, latestDay=3, multipler=1.8)
{
  #最近三天交易量都比前一天的大
  last <- last(Vo(stock),latestDay+1)
  result <- as.matrix(diff(last))
  result <- na.omit(result) 
  for (i in 1:nrow(result)) 
  {
    zz <- as.numeric(result[i,1])
    if((zz < 0)) { return (FALSE) }
  }
  #总交易量比前三天的总交易量高一倍以上
  last3 <- last(Vo(stock),latestDay)
  last6 = stock[(nrow(stock)-2*latestDay+1):(nrow(stock)-latestDay)]
  if(sum(last3)/sum(last6) < multipler) { return (FALSE) }
  return (TRUE)
}

#################################################################
#短期均线（5,10,20）在年线以上250.
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
  
  result <- na.omit(last(MA,latestDay)) 
  result <- as.matrix(result)
  for (i in 1:nrow(result)) 
  {
    ma250 <- as.numeric(result[i,2])
    ma5 <- as.numeric(result[i,3])
    ma10 <- as.numeric(result[i,4])
    ma20 <- as.numeric(result[i,5])
    if((ma5 < ma250) || (ma10 < ma250) || (ma20 < ma250)) { return (FALSE) }
  }
  return (TRUE)
}

#################################################################
#最近的一个低点（5天以内）比上一次的低点要高。
isLowestPointRule<- function(stock, backDate=5)
{
  last5 <- last(Cl(stock),backDate)
  last <- stock[(nrow(stock)-2*backDate+1):(nrow(stock)-backDate)]
  
  if(min(last5) > min(last)) { return (TRUE) } else { return (FALSE) }
  
}


getTrendMatchStocks <- function(stock.folder){
stock_symbols  <- listStocksFromDir(stock.folder)
result <- list(NA)
for(symbol in stock_symbols) 
{ 
  print(symbol)
  stock <-  loadStock(stock.folder, symbol, operation.name="all")
  if( nrow(stock) > 10 ) {  
    if(isGainRuleLastDays(stock)
       && isGainolumeRuleLastDays(stock)
       && isShorternMAOverLongTermMARule(stock)
       && isLowestPointRule(stock)) { 
      result <- c(result, symbol)
    }    
  rm(stock) }
}
  return (result[-1])  
}


getPrintMatchStocks <- function(stock.folder){
  stock_symbols  <- listStocksFromDir(stock.folder)
  result <- list(NA)
  printResult <- list(NA)
  for(symbol in stock_symbols) 
  { 
    stock <-  loadStock(stock.folder, symbol, operation.name="all")
    if( nrow(stock) > 10 ) {  
      isGainRule <- isGainRuleLastDays(stock)
      isGainolume <- isGainolumeRuleLastDays(stock)
      isShorternMA <- isShorternMAOverLongTermMARule(stock)
      isLowestPointRule <- isLowestPointRule(stock)
      printResult <- c(printResult, list(paste(symbol, isGainRule, isGainolume, isShorternMA, isLowestPointRule, "\n", sep=",")))
      if(isGainRule
         && isGainolume
         && isShorternMA
         && isLowestPointRule) { 
        result <- list(result, symbol)
      }    
      rm(stock) }
  }
  return (printResult[-1])  
} 

source.folder <- 'C:/Users/exubixu/Desktop/Imp/git_new/model1/'
source(paste0(source.folder,"commonPackages.r"))
#stock.folder <- 'C:/Users/exubixu/Desktop/new1/'
stock.folder <- 'C:/Users/exubixu/Desktop/Imp/git_new/model1/StockDatas/2016-08-09-Later_Rehabilitation_Cleaned/'
#write.csv(x=mySelected, file="C:/Users/exubixu/Desktop/new1/test.csv", row.names = TRUE)
mySelected <- getTrendMatchStocks(stock.folder)

