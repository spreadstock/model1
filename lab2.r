calculate_spreadwithBeta <- function(x, beta) {
  
  return (x$y - beta * x$x)
}

buildDistance <- function (x) {
  corr <- timeweighted_corr(x, 0.5)
  corrValue <- 1 - corr$cor
  resultValue <- corrValue[1,2]

  return (resultValue)
}

rollingV1_1 <- function(x, width, FUN, PREFUN, stockSymbols) {
  
  xIn <- x
  timeList <- index(x)
  
  
  rollingResultList <- rep(NA,width-1);

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
      rollingResult <- FUN(firstWindow)

      rollingResultList <- c(rollingResultList, rollingResult)
      
    }
    
    
    
  }
  
  
  #add time line
  rollingResultTimed <- xts(x=rollingResultList, order.by=timeList)
  colnames(rollingResultTimed) <- paste(stockSymbols[2], "_Relation", sep="")
  
  #mktdataTimed <- xts (order.by = index(xIn))
  #rollingResultTimed <- merge(mktdataTimed, rollingResultTimed, all=FALSE, join = 'left')
  
  return (rollingResultTimed)
  #return (rollingResultList)
}

stock.folder <- 'C:/important/ideas/stock/projects/model1/StockDatas/Bluechips/'
stock.output <- 'C:/important/ideas/stock/projects/model1/testResult/'

SH600131 <- loadStock(stock.folder=stock.folder, stock.name="SH600131")
SH600336 <- loadStock(stock.folder=stock.folder, stock.name="SH600336")


start_date <- "2013-01-01"
end_date <- "2015-07-01"

entStocks<-subsetByDateRange(SH600131, start_date, end_date)
entStocks<-cbind(entStocks,subsetByDateRange(SH600336, start_date, end_date)) 

#result <- entStocks

result<-calcuateLogReturn(entStocks)

y <-rollingV1_1(x=entStocks, width=30, FUN=buildDistance, PREFUN=calcuateLogReturn, stockSymbols=c("SH600000.Close","SZ399001.Close")) 

result <- cbind(result, y)

yy <-rollingV2_1(x=entStocks, width=5, FUN=calculate_spread, PREFUN=calcuateLogReturn)

result<-cbind(result,yy)

startDataList <- c("2013-01-01", "2013-07-01", "2014-02-01", "2015-03-01")
endDataList <- c("2013-01-15", "2013-07-15", "2014-02-15", "2015-03-15")
testData <- result[,c(1,2)]
colnames(testData) <- c("x", "y")

par(mfrow = c (2,2))
  
for (i in 1:4) {
  range <- paste(startDataList[i], "::", endDataList[i], sep = "")
  
  time_series <- round(coredata(testData[range]),3)
  time_series <- na.omit(time_series)
  #pred <- predict(lm(x ~ y - 1, data=as.data.frame(time_series)))
  #predTimed <- xts(x=pred, order.by=index(time_series))
  #time_series <- coredata(cbind(time_series, predTimed))
  #colnames(time_series) <- c("SH600131", "SH600336", "Pred")
  plot(x=time_series[,1], y=time_series[,2],pch=19, ylim=c(-0.04, 0.04), xlim=c(-0.05, 0.05), xaxt="n")
  at <- seq(from = -0.05, to = 0.05, by = 0.002)
  axis(side = 1, at = at)
  #lines(x=time_series[,1],y=time_series[,3])
  #alm1 <-lm(x ~ y, data=as.data.frame(time_series)) 
  #abline(alm1, lwd=2)
  #lines(x=time_series[,2], y=alm1$residuals, col="black", type="p", pch="*")
  alm2 <- lm(y ~ x, data=as.data.frame(time_series)) 
  abline(alm2, lwd=2, lty=2, col="red")
  lines(x=time_series[,1], y=alm2$residuals, col="red", type="p", pch="*")
  #r<- prcomp( ~ x + y, data=as.data.frame(time_series))
  #slop <- r$rotation[2,1] / r$rotation[1,1]
  #intercept <- r$center[2] - slop * r$center[1]
  #abline(a=0,b=slop, lty=3, lwd=2, col="green")
  grid()
  
  
  #on time
  # time_series <- testData[range]
  # aLm <- lm(y ~ x, data=as.data.frame(time_series))
  # pred <- time_series$x * aLm$coefficients[2] + aLm$coefficients[1]
  # #pred <- predict(lm(y ~ x - 1, data=as.data.frame(time_series)))
  # predTimed <- xts(x=pred, order.by=index(time_series))
  # time_series <- cbind(time_series, predTimed)
  # time_series <- cbind(time_series, xts(x=aLm$residuals, order.by=index(time_series)))
  # plot(time_series[,1])
  # lines(time_series[,2], col="blue")
  # lines(time_series[,3], col="red", lty=2, lwd=2)
  # lines(time_series[,4], col="orange", lty=2, lwd=2)
  #plot(time_series[,4])
  # r<- prcomp( ~ x + y, data=as.data.frame(time_series))
  # slop <- r$rotation[2,1] / r$rotation[1,1]
  # intercept <- r$center[2] - slop * r$center[1]
  # pred2Timed <- time_series[,1] * slop + intercept
  # time_series <- cbind(time_series, pred2Timed)
  # lines(time_series[,4], col="green", lty=4)

  # grid()


}
par(mfrow=c(1,1))







#g<-drawLine(ldata=result[,-which(colnames(result) == "Beta")],title="SH600131-SH600336", majorBreaks="1 month", minorBreaks = "1 day")
#print(g)


# out of sample
# start_date <- "2015-01-01"
# end_date <- "2015-06-01"
# 
# entStocks1<-subsetByDateRange(SH600131, start_date, end_date)
# entStocks1<-cbind(entStocks1,subsetByDateRange(SH600336, start_date, end_date))
# 
# result2<-calcuateLogReturn(entStocks1)
# 
# colnames(result2) <- c("x", "y")
# 
# y2 <- calculate_spreadwithBeta(result2, 0.6565394)
# 
# plot(y2)
# abline(h=result$Upper)
# abline(h=result$Lower)