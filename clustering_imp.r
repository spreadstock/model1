
buildCorr <- function (x) {
  corr <- timeweighted_corr(x, 0.98)
  corrValue <- round(1 - corr$cor, 3)

  return (corrValue)
}

buildClusting <- function (x) {
  distance <- as.dist(x)
  stockcluster <- hclust(distance,"ave")
  
  return (stockcluster)
}


calOwnDirectionWithValue <- function (X) {
  theResult <- diff(X)
  #theResult <- na.fill(theResult[-1,], 0)
  return (theResult)
  
}

calRelDirectionForEach <- function (X, baseStock) {
  theResult <- ifelse(sign (baseStock * X) == 1, 1, 0)
  theResult <- na.fill(theResult, 0)
  colnames(theResult) <- colnames(X)
  
  return (theResult)
  
}

calRelDirection <- function (X, stockData) {
  stockList <- colnames(stockData)
  baseStock <- stockData[,X[1]]
  otherStock <- stockData[,X[2]]
  
  theResult <- calRelDirectionForEach(X=otherStock, baseStock=baseStock)
  
  theMatch <- sum(theResult) / (nrow(theResult)-1)
  if (theMatch >= 0.15) {
    aRecord <- c(stockList[X[1]], stockList[X[2]], theMatch)
  } else 
    aRecord <- c(NA, NA, NA)
  
  
  
  return(aRecord)

  
}

loadAvg <- function () {
  stock.folder.med <- 'C:/important/ideas/stock/projects/model1/javaCode/Data/Average/'
  
  #stock_symbols <- c("SH600000","SH600037","SH600039","SH600053","SH600054","SH600056", "SH600090", "SH600094", "SH600074","SH601872","SH601908")
  stock_symbols <- listStocksFromDir(stock.folder.med)
  
  entStocks <- loadMultipleStock(stock.folder.med, stock_symbols, operation.name ="Others")
  
  x<-subsetByDateRange(entStocks, start_date, end_date)
  #remove stocks, if having less than 24 data in the window
  x<-subset(x, select=(colSums(is.na(x)) <= (nrow(x) - 24) ))
  
  return (x)
}

loadMedian <- function (start_date, end_date, symbList = NULL) {
  stock.folder.med <- 'C:/important/ideas/stock/projects/model1/javaCode/Data/Median2/'
  #stock.folder.med <- 'C:/important/ideas/stock/projects/model1/javaCode/Data/Median2/MedianAfter/'
  #stock.folder.med <- 'C:/important/ideas/stock/projects/model1/StockDatas/MedianBefore 20160823/'
  
  symbList <- c("SH601588","SZ000667","SZ002179","SH600391")
  if (length(symbList) == 0) {
    symbList <- listStocksFromDir(stock.folder.med)
  }
  
  entStocks <- loadStock(stock.folder.med, symbList[1])
  entStocks<-subsetByDateRange(entStocks, start_date, end_date)
  colnames(entStocks) <- symbList[1]
  for(n in symbList[-1]) {
    aStock <- loadStock(stock.folder.med, n)
    aStock<-subsetByDateRange(aStock, start_date, end_date)
    colnames(aStock) <- n
    entStocks <- cbind(entStocks, aStock)
  }

  #remove stocks, if having less than 120 data in the window
  entStocks<-subset(entStocks, select=(colSums(is.na(entStocks)) < 1))
  
  return (entStocks)
}

loadDailyClose <- function (start_date, end_date, symbList = NULL) {
  #stock.folder.daily <- 'C:/important/ideas/stock/projects/model1/StockDatas/Bluechips/'
  #stock.folder.daily <- 'C:/important/ideas/stock/projects/model1/javaCode/Data/Median2/bank/after/'
  stock.folder.daily <- 'C:/important/ideas/stock/projects/model1/StockDatas/2016-08-09-Former_Rehabilitation_leaned/'
  
  if (length(symbList) == 0) {
    #symbList <- c("SH600000","SH600037","SH600039","SH600053","SH600054","SH600056", "SH600090", "SH600094", "SH600074","SH601872","SH601908")
    symbList <- listStocksFromDir(stock.folder.daily)
  
  }

  entStocks <- loadMultipleStock(stock.folder.daily, symbList)
  
  x<-subsetByDateRange(entStocks, start_date, end_date)

  return (x)
}

loadMonthClose <- function () {
  #stock_symbols <- c("SH600000","SH600037","SH600039","SH600053","SH600054","SH600056", "SH600090", "SH600094", "SH600074","SH601872","SH601908")
  #stock_symbols <- c("SH601872","SH601908","SH601933","SH603006","SH603009","SH603010", "SH603017","SH603019","SH603020")
  
  stock.folder <- 'C:/important/ideas/stock/projects/model1/StockDatas/Bluechips/'
  stock_symbols <- listStocksFromDir(stock.folder)
  entStocks <- loadMultipleStock(stock.folder, stock_symbols)
  
  x<-subsetByDateRange(entStocks, start_date, end_date)
  #remove stocks, if having less than 120 days data in the window
  x<-subset(x, select=(colSums(is.na(x)) <= (nrow(x) - 120) ))
  stockList <- colnames(x)
  
  stock_month <- Cl(getRangeSummary(stock=x[,eval(quote(stockList[1]))]))

  for(n in stockList[-1]) {
    stock_month <- cbind(stock_month, Cl(getRangeSummary(stock=x[,eval(quote(n))])))
  }
  
  return (stock_month)
}

processFirst <- function (start_date, end_date, outputFolder, outputFileName) {
  stock_month <- calOwnDirectionWithValue(loadMedian(start_date=start_date, end_date=end_date))
  stockList <- colnames(stock_month)

  finalResult_1st <- apply(combn(ncol(stock_month), 2), 2, FUN=calRelDirection, stockData=stock_month)
  finalResult_1st<- t(finalResult_1st[,colSums(is.na(finalResult_1st))<nrow(finalResult_1st)])
  
  colnames(finalResult_1st)<- c("Stock1","Stock2","MatchPercentage")
  #writeStock(x=finalResult_1st, stock.folder=outputFolder, ouput.name=outputFileName, isZoo = FALSE)
  return (finalResult_1st)

}

process <- function (firstResultFolder, firstResultFileName, start_date, end_date) {
  
  finalResult_1st <- read.csv(paste(firstResultFolder, firstResultFileName, '.csv', sep=''), sep=',', check.names=FALSE)
  finalResult_1st <- finalResult_1st[,-1]
  refinedStockList <- unique(as.vector(as.matrix(finalResult_1st[,-3])))
  stock_daily <- loadDailyClose(start_date=start_date2, end_date=end_date2, symbList = refinedStockList)
  
  finalResult_2nd <- vector()
  for (aStockPair in 1:nrow(finalResult_1st)) {
    aStock1 <- paste(finalResult_1st[aStockPair,1],"Close", sep=".")
    aStock2 <- paste(finalResult_1st[aStockPair,2],"Close", sep=".")
    aStockData <- stock_daily[,aStock1]
    aStockData <- cbind(aStockData, stock_daily[,aStock2])
    aStockData <- calcuateSimpleReturn(aStockData)
    #exclude different direction
    aStockData <- aStockData[which(sign(aStockData[,1]) == sign(aStockData[,2]))]
    finalResult_2nd <- c(finalResult_2nd, cor(aStockData)[1,2])
  }
  
  finalResult_1st <- cbind(finalResult_1st, finalResult_2nd)
  
  #3rd level process, cointeg
  finalResult_3rd <- vector()
  stock_daily1 <- na.omit(stock_daily)
  for (aStockPair in 1:nrow(finalResult_1st)) {
    aStock1 <- paste(finalResult_1st[aStockPair,1],"Close", sep=".")
    aStock2 <- paste(finalResult_1st[aStockPair,2],"Close", sep=".")
    aStockData <- stock_daily1[,aStock1]
    aStockData <- cbind(aStockData, stock_daily1[,aStock2])
    colnames(aStockData) <- c("stockX", "stockY")
    finalResult_3rd <- c(finalResult_3rd, cointegrationTest(aStockData))
  }
  finalResult_1st <- cbind(finalResult_1st, finalResult_3rd)
  
  colnames(finalResult_1st)<- c("Stock1","Stock2","MatchPercentage","Cor","Cointegration")
  outputFile <- "clusteringResult"
  writeStock(x=finalResult_1st, stock.folder=stock.output, ouput.name=outputFile, isZoo = FALSE)
  
}

################# Main 
stock.output <- 'C:/important/ideas/stock/projects/model1/testResult/testRel/'

# 1st level processing
start_date <- "2012-01-01"
end_date <- "2014-12-31"
# 
outputFile <- "clusteringResult_1st"
finalResult_1st <- processFirst(start_date, end_date,stock.output,outputFile)


#outputFile <- "clusteringResult_1st_bk"

# 2nd level processing, cor
start_date2 <- "2013-01-01"
end_date2 <- "2014-12-31"

# finalResult_2nd <- buildClusting(stock_daily_dist)
# finalResult_2nd_groups <- cutree(finalResult_2nd, k=6)








