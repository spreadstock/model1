library(quantmod)
library(plyr)
library(ggplot2)
library(scales)
library(stringr)
library(quantstrat)
library(amap)
library(tseries)
library(devtools)
library(IKTrading)
library(PerformanceAnalytics)

#writeStock write processed result into a csv file
#Example, 
#stock.folder <- 'C:/important/ideas/stock/projects/model1/testResult/'
#writeStock(stock.output, "SH601098")
writeStock <- function(x, stock.folder="", ouput.name, isZoo=FALSE)
{
  if (!isZoo) {
    write.csv(x=x, file=paste(stock.folder, ouput.name, ".csv",sep=""), row.names = TRUE)
  } else {
    write.zoo(x=x, file=paste(stock.folder, ouput.name, ".csv",sep=""),sep = ",")
  }
  
}

#loadStock loads single stock a
#Example, 
#stock.folder <- 'C:/important/ideas/stock/stockdata/'
#SH601098<- loadStock(stock.folder, "SH601098","Op")
loadStock <- function(stock.folder, stock.name, operation.name = "Cl")
{
  tmp <- read.csv(paste(stock.folder, stock.name, '.txt', sep=''), sep=',', check.names=FALSE)
  tmpCol <- colnames(tmp)
  stock1  <- xts(tmp[,-1],as.Date(tmp[,1],"%Y/%m/%d"))
  colnames(stock1) <- tmpCol[-1]
  if (operation.name == "Op") {
    return (Op(stock1))
  } else if (operation.name == "Hi") {
    return (Hi(stock1))
  } else if (operation.name == "Lo") {
    return (Lo(stock1))
  } else if (operation.name == "Cl") {
    return (Cl(stock1))
  } else if (operation.name == "Vo") {
    return (Vo(stock1))
  } else if (operation.name == "Ad") {
    return (Ad(stock1))
  } else if (operation.name == "OpCl") {
    return (OpCl(stock1))
  } else if (operation.name == "ClCl") {
    return (ClCl(stock1))
  } else if (operation.name == "HiCl") {
    return (HiCl(stock1))
  } else if (operation.name == "LoCl") {
    return (LoCl(stock1))
  } else if (operation.name == "LoHi") {
    return (LoHi(stock1))
  } else if (operation.name == "OpHi") {
    return (OpHi(stock1))
  } else if (operation.name == "OpLo") {
    return (OpLo(stock1))
  } else if (operation.name == "OpOp") {
    return (OpOp(stock1))
  } else if (operation.name == "HLC") {
    return (HLC(stock1))
  } else if (operation.name == "OHLC") {
    return (OHLC(stock1))
  } else if (operation.name == "OHLCV") {
    return (OHLCV(stock1))
  } 
  return (stock1)
  
}
#loadStock loads multiple stocks
#Example, 
#stock.folder <- 'C:/important/ideas/stock/stockdata/'
#stock_symbols <- c("SH600037","SH600088", "SH600136", "SH600229", "SH600373", "SH600576", "SH600633", "SH600637", "SH600715","SH600757", "SH600825", "SH600831", "SH600959","SH601098", "SH601801", "SH601900", "SH601928", "SH601929","SH601999","SH603598","SH603999")
#entStocks<-loadMultipleStock(stock.folder,stock_symbols)
loadMultipleStock <- function(stock.folder, stock.name.list, operation.name = "Cl")
{
  entStocks <- loadStock(stock.folder, stock.name.list[1], operation.name)
  for(n in stock.name.list[-1]) {
    entStocks <- cbind(entStocks, loadStock(stock.folder, n, operation.name))
  } 
  return (entStocks)
}

loadMultipleStockList <- function(stock.folder, stock.name.list, operation.name = "Cl")
{
  entStocks <- list(loadStock(stock.folder, stock.name.list[1], operation.name))
  
  for(n in stock.name.list[-1]) {
    entStocks <- cbind(entStocks, list(loadStock(stock.folder, n, operation.name)))
  } 
  return (entStocks)
}

listStocksFromDir <- function(stock.folder, pattern="*.txt")
{
  stockList <- list.files(path=stock.folder, pattern=pattern)
  return (word(stockList,sep=fixed(".")))
}

calcuateLogReturn <- function(x)
{
  dx <- na.omit(x)
  dx<-diff(log(x), trim=TRUE)
  dx <- na.omit(dx)
  #dx <- dx[is.finite(dx)]
  return (dx)
}

calcuateStandarize <- function(x)
{
  dx <- na.omit(x)
  return ( apply(X=dx, MARGIN=2, FUN=divFun))
}


calcuateAbsPrice <- function(x)
{
  dx <- na.omit(x)
  return (dx)
}

calcuateSimpleReturn <- function(x)
{
  dx <- na.omit(x)
  dx<-diff(x) / lag(x)
  #dx <- na.omit(dx)

  return (dx)
}

calcuateSMA <- function(x, n)
{
  dx <- na.locf(x, na.rm = TRUE)
  dx<-SMA(dx,n)
  return (dx)
}

#calcuate time weighted correlation
#x, multiple stocks on time serials. x must contain more than one stocks
#timeWeighted, weighted value. Weight reduced as timeWeighted^n, where n is number of days
timeweighted_corr <- function(x, timeWeighted=0.98)
{
  numberOfDays <- nrow (x) - 1
  lam = timeWeighted
  i = 0:numberOfDays
  ewma.wt = lam^i
  ewma.wt = ewma.wt/sum(ewma.wt)
  cov.ewma = cov.wt(x, wt=rev(ewma.wt), cor=TRUE)
  cov.ewma
  
}

#calcuate cov
#x, multiple stocks on time serials. x must contain more than one stocks
calcuate_cov <- function(x)
{
  covResult = sign(cov(x))
  covResult
  
}

#subset of stock data by date range
#x, multiple stocks on time serials. x must contain at least one stocks data
#start_date, start of range. when not presented, the first date will be used
#end_date, end of range. when not presented, the last date will be used
subsetByDateRange <- function(x, start_date=start(x), end_date=end(x))
{
  range <- paste(start_date, "::", end_date, sep = "")
  return(x[range,])
  
}

#draw stock data
#ldata, stock matrix on time serials. The first column is the main data.
#title, subject of drawing
#ylab, subject of Y axis
#sDate, start date
#eDate, end date
#major breaks, the major breaks on X axis
#minor breaks, the minor breaks on X axis
#out, if save to output file. 
drawLine<-function(ldata,title="Stock_MA",ylab="Value",sDate=min(index(ldata)),eDate=max(index(ldata)),majorBreaks="1 week", minorBreaks="1 day", out=FALSE){
  g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
  g<-g+geom_line()
  g<-g+geom_line(aes(colour=Series),data=fortify(ldata[,-1],melt=TRUE))
  g<-g+scale_x_date(labels=date_format("%d/%m/%y"),breaks=date_breaks(majorBreaks),minor_breaks=date_breaks(minorBreaks), limits = c(sDate,eDate))
  g<-g+xlab("") + ylab(ylab)+ggtitle(title)
  
  if(out) ggsave(g,file=paste(titie,".png",sep=""))
  return (g)
}




testFun <-function(x, threshold, yesValue=1, noValue=0) {
  return (ifelse(x <= threshold, yesValue, noValue))
}

standFun <-function(x) {
  itsMax <- max(x)
  return (1 - x / itsMax)
}



divFun <- function (x) {
  return (x / x[1])
}

#getRangeSummary(stock1,start_date, end_date, "year")
getRangeSummary<- function(stock, start_date=start(stock), end_date=end(stock), kType="month")
{
  range <- paste(start_date, "::", end_date, sep = "")
  rangeStock <-  stock[range,]

  if (kType == "year") {
    return (to.yearly(rangeStock,drop.time=FALSE))
  }
  else if (kType == "month") {
    return (to.monthly(rangeStock,indexAt = 'endof',drop.time=FALSE))
  }
  else if (kType == "week") {
    return (to.weekly(rangeStock,drop.time=FALSE))
  }
  else {
    return (to.daily(rangeStock,drop.time=FALSE))
  }
}


#Convert stock id to stock name
#Attention, this function assumes the order of stockId is aligned with stock symbols
covertStockId2Name <- function(stockId, stockSymbols)
{
  return (stockSymbols[stockId])
}

#cointegration test
cointegrationTest <- function(stockData)
{
  m <- lm(stockY ~ stockX + 0, data=stockData)
  beta <- coef(m)[1]
  sprd <- stockData$stockY - beta * stockData$stockX
  ht <- adf.test(sprd, alternative = "stationary", k=0)
  return (ht$p.value)
}

takeTranxFee <- function(TxnQty, TxnPrice, Symbol,...) {
  
  aFee <- abs(TxnQty) * TxnPrice * -0.005
  return (aFee)
}

