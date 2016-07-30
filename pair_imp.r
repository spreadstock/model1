calculate_spread <- function(x) {
  

  aLm <- lm(y ~ x - 1, data=as.data.frame(x))
  betas <- coef(aLm)[1]
  # yValue <- coredata(x$y)
  # xValue <- coredata(x$x)
  # spreadValue <- yValue - betas * xValue
  
  
#  spreadUpper <- mean(spreadValue, na.rm = TRUE) + sd(spreadValue, na.rm = TRUE)
#  spreadLower <- mean(spreadValue, na.rm = TRUE) - sd(spreadValue, na.rm = TRUE)
#  spreadResult <- c(median(spreadValue), betas)

  
  return (betas)
}

loadDailyClose <- function (start_date, end_date, symbList = NULL) {
  stock.folder.daily <- 'C:/important/ideas/stock/projects/model1/StockDatas/Bluechips/'
  
  if (length(symbList) == 0) {
    #symbList <- c("SH600000","SH600037","SH600039","SH600053","SH600054","SH600056", "SH600090", "SH600094", "SH600074","SH601872","SH601908")
    symbList <- listStocksFromDir(stock.folder.daily)
    
  }
  
  entStocks <- loadMultipleStock(stock.folder.daily, symbList)
  
  x<-subsetByDateRange(entStocks, start_date, end_date)
  
  return (x)
}

rollingV2_1 <- function(x, width, FUN, PREFUN) {
  
  xIn <- x
  timeList <- index(x)
  
  
  rollingResultList <- matrix(NA,nrow=(width - 1), ncol= 1);
  
  for (aNext in 1:(length(timeList)-width + 1)) {
    startPoint <- timeList[aNext]
    endPoint <- timeList[aNext + width - 1]
    startDateOfWindow <- as.Date(startPoint)
    endDateOfWindow <- as.Date(endPoint)
    firstWindow <- window(x,start=startDateOfWindow,end=endDateOfWindow)
    
    #clean up data, remove stocks which miss data
    #firstWindow <- firstWindow[,colSums(is.na(firstWindow)) < width]
    #firstWindow <- na.locf(firstWindow, fromLast = FALSE)
    #firstWindow <- na.locf(firstWindow, fromLast = TRUE)
    firstWindow <- PREFUN (firstWindow)
    #clean up data, remove stocks which no change in window
    #firstWindow <- firstWindow[,colSums(firstWindow == 0) < nrow(firstWindow)]
    
    #cannot proceed, if only one column or nothing in window
    if ((ncol(firstWindow) < 2) | (nrow(firstWindow) == 0)) {
      rollingResult <- NA
    } else {
      #call function first window
      colnames(firstWindow) <- c("x", "y")
      rollingResult <- FUN(firstWindow)

      rollingResultList <- rbind(rollingResultList, rollingResult)

    }
    
  }
  
  
  #add time line
  lookBack <- 60
  
  rollingResultTimed <- xts(x=rollingResultList, order.by=timeList)
  stockData <- calcuateSimpleReturn(x)
  spread <- stockData[,2] - lag(rollingResultTimed[,1], 1) * stockData[,1]
  movingAvg = calcuateSMA(spread,lookBack) #Moving average
  movingStd = runSD(spread,lookBack, sample=FALSE) #Moving standard deviation / bollinger bands
  spreadUpper <- movingAvg + 1.2 * movingStd
  spreadLower <- movingAvg - 1.2 * movingStd
  rollingResultTimed <- cbind(spread, rollingResultTimed/10, spreadUpper, spreadLower)

  colnames(rollingResultTimed) <- c("Spread","Beta" ,"Upper","Lower")
  
  #mktdataTimed <- xts (order.by = index(xIn))
  #rollingResultTimed <- merge(mktdataTimed, rollingResultTimed, all=FALSE, join = 'left')
  
  return (rollingResultTimed)
  #return (rollingResultList)
}

################# Main 
stock.output <- 'C:/important/ideas/stock/projects/model1/testResult/testRel/'

# 1st level processing
start_date2 <- "2014-01-01"
end_date2 <- "2014-12-31"

stock_daily <- loadDailyClose(start_date=start_date2, end_date=end_date2, symbList = c("SH600391" ,"SZ000738"))
yy <-rollingV2_1(x=stock_daily, width=10, FUN=calculate_spread, PREFUN=calcuateSimpleReturn)


#betaZZ <- cumsum(na.omit(yy$Beta))
#betaZZ <- yy$Beta / betaZZ
#yy <- cbind (yy, betaZZ)
zz <- cumsum(na.omit(yy$Spread * (lag(yy$Beta,1))))
yy<-cbind(yy,zz)
returns <- calcuateSimpleReturn(stock_daily)
colnames(yy) <- c("Spread","Beta" ,"Upper","Lower","AccSpread")
