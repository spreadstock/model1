preProcess <- function(x) {
  return (calcuateLogReturn(x))
}

rollingV1_1 <- function(x, width, FUN, PREFUN, returnColumn) {
  
  xIn <- x
  timeList <- index(x)
  
  rollingResultList <- matrix(NA,nrow=width-1,ncol=length(returnColumn))
  colnames(rollingResultList) <- names(returnColumn)
  
  for (aNext in 1:(length(timeList)-width + 1)) {
    startPoint <- timeList[aNext]
    endPoint <- timeList[aNext + width - 1]
    startDateOfWindow <- as.Date(startPoint)
    endDateOfWindow <- as.Date(endPoint)
    firstWindow <- window(x,start=startDateOfWindow,end=endDateOfWindow)
    
    #clean up data, remove stocks which miss data
    firstWindow <- firstWindow[,colSums(is.na(firstWindow)) < width]
    #firstWindow <- removeNA(firstWindow)
    firstWindow <- na.locf(firstWindow, fromLast = FALSE)
    firstWindow <- na.locf(firstWindow, fromLast = TRUE)
    firstWindow <- PREFUN (firstWindow)
    #clean up data, remove stocks which no change in window
    firstWindow <- firstWindow[,colSums(firstWindow == 0) < nrow(firstWindow)]
    
    #cannot proceed, if only one column or nothing in window
    if ((ncol(firstWindow) < 2) | (nrow(firstWindow) == 0)) {
      rollingResult <- rep(NA, length(returnColumn))
    } else {
      #call function first window
      rollingResult <- FUN(firstWindow, returnColumn)
      
    }
    rollingResultList <- rbind(rollingResultList, rollingResult) 
    
  }
  
  
  
  #add time line
  rollingResultTimed <- xts(x=rollingResultList, order.by=timeList)
  
  #mktdataTimed <- xts (order.by = index(xIn))
  #rollingResultTimed <- merge(mktdataTimed, rollingResultTimed, all=FALSE, join = 'left')
  
  return (rollingResultTimed)
  #return (rollingResultList)
}


identifyPair <- function (x, xPos, yPos) {
  
  result <- vector()
  for (aPos in 1:length(yPos)) {
    nowPos <- yPos[aPos]
    stockItem <- x[xPos, nowPos]
    if (sign(stockItem) == -1) {
      result1 <- c(-stockItem)
    } else {
      stocks <- identifyPair(x,stockItem, c(1,2))
      result1 <- c(paste(stocks, collapse ="$"))
    }
    result <-c(result,result1)
  }
  return (result)
}

getPairList <- function(x, dist) {
  result <- vector()
  for (stockItem in 1:5) {
    stock1Out <- identifyPair(x, stockItem, c(1))
    stock2Out <- identifyPair(x, stockItem, c(2))
    stockDist <- c(dist[stockItem])
    
    names(stock1Out) <- paste("Top",stockItem,".1",sep = "")
    names(stock2Out) <- paste("Top",stockItem,".2",sep = "")
    names(stockDist) <- paste("Top",stockItem,".distance", sep = "")
    
    result <- c(result,stock1Out,stock2Out,stockDist)
    
  }
  
  return (result)
  
}

buildClusting <- function (x, returnColumn) {
  corr <- timeweighted_corr(x)
  corrValue <- 1 - corr$cor
  distance <- as.dist(corrValue)
  stockcluster <- hclust(distance,"ave")
  
  #build pair list
  rollingResult <- getPairList(stockcluster$merge, stockcluster$height)
  
  return (rollingResult)
}

buildDistance <- function (x, returnColumn) {
  resultValue <- rep(NA, length(returnColumn))
  names(resultValue) <- returnColumn
  corr <- timeweighted_corr(x)
  corrValue <- 1 - corr$cor
  for (n in colnames(corrValue)[-1]) {
    resultValue[n] <- corrValue[1,n]
  }

  return (resultValue)
}




start_date <- "2012-01-01"
end_date <- "2015-01-01"
stock.folder <- 'C:/important/ideas/stock/projects/model1/StockDatas/Bluechips/'
stock_symbols <- listStocksFromDir(stock.folder)

entStocks <- loadMultipleStock(stock.folder, stock_symbols)

x<-subsetByDateRange(entStocks, start_date, end_date)

y <-rollingV1_1(x=x, width=30, FUN=buildClusting, PREFUN=calcuateLogReturn, returnColumn=c("Top1.1","Top1.2","Top1.distance","Top2.1","Top2.2","Top2.distance","Top3.1","Top3.2","Top3.distance","Top4.1","Top4.2","Top4.distance","Top5.1","Top5.2","Top5.distance"))


topList <- na.omit(coredata(y))
topList1 <- topList[,c("Top1.1", "Top1.2")]
names(topList1) <- c("Top.1", "Top.2")
topList2 <- topList[,c("Top2.1", "Top2.2")]
names(topList2) <- c("Top.1", "Top.2")
topList3 <- topList[,c("Top3.1", "Top3.2")]
names(topList3) <- c("Top.1", "Top.2")
topList4 <- topList[,c("Top4.1", "Top4.2")]
names(topList4) <- c("Top.1", "Top.2")
topList5 <- topList[,c("Top5.1", "Top5.2")]
names(topList5) <- c("Top.1", "Top.2")
topList <- topList1
topList <- rbind(topList, topList2)
topList <- rbind(topList, topList3)
topList <- rbind(topList, topList4)
topList <- rbind(topList, topList5)

topListCounted <- count(topList)
#topListCounted <- topListCounted[order(topListCounted$x.Top1.1,-topListCounted$freq,topListCounted$x.Top1.2),]
topListCounted <- topListCounted[order(-topListCounted$freq),]



# # check pair 1 to 10 & 13, 7, 14
# stockMixed <- x[,1]
# stockMixed <- cbind (stockMixed, x[,10])
# stockMixed <- cbind (stockMixed, x[,13])
# stockMixed <- cbind (stockMixed, x[,7])
# stockMixed <- cbind (stockMixed, x[,14])
# 
# yy <-rollingV1_1(x=stockMixed, width=30, FUN=buildDistance, PREFUN=calcuateLogReturn, returnColumn=c("Close.9", "Close.12", "Close.6", "Close.13"))
# g<-drawLine(ldata = yy, title="Relation with SH600551", ylab = "Distance")
# print(g)







