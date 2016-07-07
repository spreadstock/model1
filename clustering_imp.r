convertStockNames <- function (x, stockSymbols) {
  return (paste(stockSymbols[x[1]], " & ", stockSymbols[x[2]], sep = ""))
}

preProcess <- function(x) {
  return (calcuateLogReturn(x))
}

rollingV1_1 <- function(x, width, FUN, PREFUN, stockSymbols) {
  
  xIn <- x
  timeList <- index(x)
  
  rollingResultList <- list()

  for (aNext in 1:(length(timeList)-width + 1)) {
    startPoint <- timeList[aNext]
    endPoint <- timeList[aNext + width - 1]
    startDateOfWindow <- as.Date(startPoint)
    endDateOfWindow <- as.Date(endPoint)
    firstWindow <- window(x,start=startDateOfWindow,end=endDateOfWindow)
    
    #clean up data, remove stocks which miss data
    firstWindow <- firstWindow[,colSums(is.na(firstWindow)) < width]
    firstWindow <- na.locf(firstWindow, fromLast = FALSE)
    firstWindow <- na.locf(firstWindow, fromLast = TRUE)
    firstWindow <- PREFUN (firstWindow)
    #clean up data, remove stocks which no change in window
    firstWindow <- firstWindow[,colSums(firstWindow == 0) < nrow(firstWindow)]
    
    #cannot proceed, if only one column or nothing in window
    if ((ncol(firstWindow) < 2) | (nrow(firstWindow) == 0)) {

          } else {
      #call function first window
      rollingResult1 <- FUN(firstWindow)
      rollingResult <- as.matrix(rollingResult1$corr)

      # put back missed columns
      resultNames <- colnames(rollingResult)
      if (length(resultNames) != length(stockSymbols)) {
        prevItem <- ""
        processedN <- 0
        for (aItem in stockSymbols) {
          if (aItem %in% resultNames) {
            prevItem <- aItem 
          } else {
            #insert a column between prev and current
            aStockItem <-rep(9, length(resultNames)+processedN)
            aStockItem1 <-rep(9, length(resultNames)+processedN+1)
            if (prevItem == "") {
              rollingResult <- cbind (aStockItem, rollingResult)
              rollingResult <- rbind (aStockItem1, rollingResult)
            } else {
              if (prevItem == resultNames[length(resultNames)]) {
                rollingResult <- cbind (rollingResult, aStockItem)
                rollingResult <- rbind (rollingResult, aStockItem1)
              } else {
                curPos <- which (resultNames == prevItem)  
                rollingResult <- cbind(rollingResult, aStockItem)
                rollingResult<- rollingResult[, c(1:curPos,ncol(rollingResult),(curPos+1):(ncol(rollingResult)-1))]
                rollingResult <- rbind(rollingResult, aStockItem1)
                rollingResult<- rollingResult[c(1:curPos,ncol(rollingResult),(curPos+1):(ncol(rollingResult)-1)),]
              }
              
            }
            processedN <- processedN + 1
          }
        }
        colnames(rollingResult) <- stockSymbols
        rownames(rollingResult) <- stockSymbols
        rollingResult1$corr <- rollingResult
      }

      rollingResultList <- c(rollingResultList, rollingResult1)

    }
    
     
    
  }
  

  #add time line
  #rollingResultTimed <- xts(x=rollingResultList, order.by=timeList)
  
  #mktdataTimed <- xts (order.by = index(xIn))
  #rollingResultTimed <- merge(mktdataTimed, rollingResultTimed, all=FALSE, join = 'left')
  
  #return (rollingResultTimed)
  return (rollingResultList)
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

buildCorr <- function (x) {
  corr <- timeweighted_corr(x, 0.98)
  corrValue <- 1 - corr$cor
  corrValueList <- list(corr=corrValue)

  return (corrValueList)
}

buildCov <- function (x) {
  cov <- calcuate_cov(x)
  covValue <- 1 - cov
  covValueList <- list(corr=covValue)
  
  return (covValueList)
}

buildClusting <- function (x) {
  distance <- as.dist(x)
  stockcluster <- hclust(distance,"ave")
  
  return (stockcluster)
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

buildClustingFull <- function (x) {
  corr <- timeweighted_corr(x)
  corrValue <- 1 - corr$cor
  distance <- as.dist(corrValue)
  stockcluster <- hclust(distance,"ave")
  
  return (stockcluster)
}


stock.folder <- 'C:/important/ideas/stock/projects/model1/StockDatas/Bluechips/'
stock.output <- 'C:/important/ideas/stock/projects/model1/testResult/'

stock_symbols <- listStocksFromDir(stock.folder)
#stock_symbols <- c("SH600000","SH600037","SH600039","SH600053","SH600054","SH600056", "SH600090", "SH600094", "SH600074","SH601872","SH601908")
#stock_symbols <- c("SH601872","SH601908","SH601933","SH603006","SH603009","SH603010", "SH603017","SH603019","SH603020")

entStocks <- loadMultipleStock(stock.folder, stock_symbols)


start_date <- "2014-01-01"
end_date <- "2015-01-01"


x<-subsetByDateRange(entStocks, start_date, end_date)
#remove stocks, if having less than 120 days data in the window
x<-subset(x, select=(colSums(is.na(x)) <= (nrow(x) - 120) ))
stockList <- colnames(x)

stock_month <- Cl(getRangeSummary(stock=x[,eval(quote(stockList[1]))]))

for(n in stockList[-1]) {
  stock_month <- cbind(stock_month, Cl(getRangeSummary(stock=x[,eval(quote(n))])))
}
colnames(stock_month) <- stockList

#daily rolling
monthY <-rollingV1_1(x=stock_month, width=3, FUN=buildCov, PREFUN=calcuateLogReturn, stockSymbols=stockList)
monthObsResult <- Reduce("+", monthY)

monthResultList <- which(monthObsResult == 0, arr.ind = T)
monthResultList <- monthResultList[which(monthResultList[,1]!=cc[,2]),]

monthResultNameList <- apply(monthResultList, MARGIN=1, FUN=convertStockNames,stockSymbols=stockList)


# #build stock header
# stock_symbols2 <- colnames(x)
# 
# 
# #daily rolling
# #y <-rollingV1_1(x=x, width=30, FUN=buildCorr, PREFUN=calcuateLogReturn, stockSymbols=stock_symbols2)
# #yy <- lapply(y, testFun, threshold=0.8)
# 
# 
# 
# #sum up
# #obsResult <- Reduce("+", yy)
# # 
# #writeStock(x=obsResult, stock.folder=stock.output, ouput.name="topList")
# 
# 
# #build clustering
# obsDistances <- apply(obsResult,MARGIN=1, FUN=standFun)
# obsCluster <- buildClusting(obsDistances)



# 
# topList <- na.omit(coredata(y))
# topList1 <- topList[,c("Top1.1", "Top1.2")]
# names(topList1) <- c("Top.1", "Top.2")
# topList2 <- topList[,c("Top2.1", "Top2.2")]
# names(topList2) <- c("Top.1", "Top.2")
# topList3 <- topList[,c("Top3.1", "Top3.2")]
# names(topList3) <- c("Top.1", "Top.2")
# topList4 <- topList[,c("Top4.1", "Top4.2")]
# names(topList4) <- c("Top.1", "Top.2")
# topList5 <- topList[,c("Top5.1", "Top5.2")]
# names(topList5) <- c("Top.1", "Top.2")
# topList <- topList1
# topList <- rbind(topList, topList2)
# topList <- rbind(topList, topList3)
# topList <- rbind(topList, topList4)
# topList <- rbind(topList, topList5)
# # 
# topListCounted <- count(topList)
# #topListCounted <- topListCounted[order(topListCounted$x.Top1.1,-topListCounted$freq,topListCounted$x.Top1.2),]
# topListCounted <- topListCounted[order(-topListCounted$freq),]



# # check pair 1 to 10 & 13, 7, 14
# stockMixed <- x[,286]
# stockMixed <- cbind (stockMixed, x[,272])
# stockMixed <- cbind (stockMixed, x[,285])
# #stockMixed <- cbind (stockMixed, x[,7])
# #stockMixed <- cbind (stockMixed, x[,14])
# 
# yy <-rollingV1_1(x=stockMixed, width=30, FUN=buildDistance, PREFUN=calcuateLogReturn, returnColumn=c("X002139.Close", "X002093.Close", "X002138.Close"))
# g<-drawLine(ldata = yy, title="Relation with SH600551", ylab = "Distance")
# print(g)


# par(mfrow = c (2,2))
# 
# start_date1 <- "2014-01-01"
# end_date1 <- "2014-04-01"
# xx<-subsetByDateRange(entStocks, start_date1, end_date1)
# xx <- xx[,colSums(is.na(xx)) < nrow(xx)]
# yy <- preProcess(xx)
# yy <- yy[,colSums(yy == 0) < nrow(yy)]
# yyClust <- buildClustingFull(yy)
# plot(yyClust, main=start_date1)
# 
# 
# start_date1 <- "2014-04-01"
# end_date1 <- "2014-07-01"
# xx<-subsetByDateRange(entStocks, start_date1, end_date1)
# xx <- xx[,colSums(is.na(xx)) < nrow(xx)]
# yy <- preProcess(xx)
# yy <- yy[,colSums(yy == 0) < nrow(yy)]
# yyClust <- buildClustingFull(yy)
# plot(yyClust, main=start_date1)
# 
# 
# start_date1 <- "2014-07-01"
# end_date1 <- "2014-10-01"
# xx<-subsetByDateRange(entStocks, start_date1, end_date1)
# xx <- xx[,colSums(is.na(xx)) < nrow(xx)]
# yy <- preProcess(xx)
# yy <- yy[,colSums(yy == 0) < nrow(yy)]
# yyClust <- buildClustingFull(yy)
# plot(yyClust, main=start_date1)
# 
# start_date1 <- "2014-10-01"
# end_date1 <- "2015-01-01"
# xx<-subsetByDateRange(entStocks, start_date1, end_date1)
# xx <- xx[,colSums(is.na(xx)) < nrow(xx)]
# yy <- preProcess(xx)
# yy <- yy[,colSums(yy == 0) < nrow(yy)]
# yyClust <- buildClustingFull(yy)
# plot(yyClust, main=start_date1)
# 
# par(mfrow=c(1,1))





