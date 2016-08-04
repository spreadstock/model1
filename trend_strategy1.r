library(quantstrat)
startDate <- '2010-01-01'
endDate <- Sys.Date()
Sys.setenv(TZ="UTC")
multi.trend <- "multi.trend"
qs.strategy <- "trend1"
shortSMA <- 5
middleSMA <- 13
stock.folder <- 'C:/Users/exubixu/Desktop/new1/'
initDate <- '2001-08-08'
initEq <- 1e6
currency('USD')


symbols <- listStocksFromDir(stock.folder)
for(symbol in symbols) 
{ 
  a  <-  loadStock(stock.folder, symbol, operation.name="all") 
  a$SMAShort <- SMA(Cl(a),shortSMA)
  a$SMAMid <- SMA(Cl(a),middleSMA)
  a$SMADiff <- a$SMAShort - a$SMAMid
  a$osc <- MACD(Cl(a))
  a$atr <- ATR(a)$atr
  assign(symbol,a)
  rm(a)
}


stock(symbols, currency='USD',multiplier=1)
rm.strat(qs.strategy)
rm.strat(multi.trend)
initPortf(multi.trend, symbols, initDate = initDate)
initAcct(
  multi.trend,
  portfolios = multi.trend,
  initDate = initDate,
  initEq = initEq
)
initOrders(portfolio = multi.trend, initDate = initDate)

strategy(qs.strategy, store =TRUE)

#c.	以上买点还必须符号最近10天里上涨时的平均交易量大于下跌时的平均交易量。
isVolumeTrendUp <- function(x)
{
  result <- diff(Cl(x))
  result <- na.omit(result)
  UpSum <- list(NA)
  DownSum <- list(NA)
  for (i in 1:nrow(result))
  {
    if(result[i] > 0) { UpSum <- cbind(UpSum,list(Vo(x)[i+1]))} else { DownSum <- cbind(DownSum,list(Vo(x)[i+1]))}
  }
  UpSum <- UpSum[-1]
  DownSum <- DownSum[-1]
  if(sum(as.numeric(UpSum)) == 0 || sum(as.numeric(DownSum)) == 0) { return (FALSE)}
  if(mean(as.numeric(UpSum)) > mean(as.numeric(DownSum)) ) { return (TRUE)} else { return (FALSE)}
}

isvolumeUp <- function(x, flashBackDay=10) 
{ 
  a <- matrix(FALSE,nrow=nrow(x),ncol=1)
  startIndex <- flashBackDay+1;
  
  for (i in startIndex:nrow(x))
  {
    if(isVolumeTrendUp(x[(i-flashBackDay):i]))
    {
      a[i] <- TRUE
    }
  }
  return (a) 
}

add.indicator(
  strategy = qs.strategy,
  name = "isvolumeUp",
  arguments = list(
    x = quote(mktdata)
  ),
  label = "isvolumeUp"
)

add.signal(
  qs.strategy,
  name = "sigThreshold",
  arguments = list(column = "isvolumeUp", relationship = "gt",threshold=0,cross=TRUE),
  label = "signal.isvolumeUp")

# 0.MACD越过第N次
trainGtOsc <- function(x, numberofBreak = 3)
{
  gtCount <- 0
  a <- matrix(FALSE,nrow=nrow(x),ncol=1)
  for (i in 1:nrow(x))
  {
    if(gtCount<numberofBreak && !is.na(x[i]) && x[i] > 0)
    {
      gtCount=gtCount+1;
    }
    else if(gtCount >= numberofBreak && !is.na(x[i]) && x[i] > 0)
    {
      a[i] <- TRUE
      gtCount <- 0
    }
  }
  return (a)  
  
}
  
add.indicator(
  strategy = qs.strategy,
  name = "trainGtOsc",
  arguments = list(
    x = quote(mktdata$osc)
  ),
  label = "trained_osc"
)

add.signal(
  qs.strategy,
  name = "sigThreshold",
  arguments = list(column = "trained_osc", relationship = "gt",threshold=0,cross=TRUE),
  label = "signal.gt.osc")
  
#a.	当差值是负数但是持续变大超过3或5天时，标识为买点
isTrendUp <- function(matrixList)
{
  for (i in 1:(nrow(matrixList)-1))
  {
    if(matrixList[i] > matrixList[i+1])
    {
      return (FALSE)
    }
  }
  return (TRUE)
}

treat_trendGrowMinus <- function(x, targetDiffGrowDay=3) 
{ 
  a <- matrix(FALSE,nrow=nrow(x),ncol=1)
  startIndex <- middleSMA+targetDiffGrowDay
  for (i in startIndex:nrow(x))
  {
    if(isTrendUp(as.matrix(x[(i-targetDiffGrowDay):i])) && x[i] < 0)
    {
      a[i] <- TRUE
    }
  }
  return (a) 
}

add.indicator(
  strategy = qs.strategy,
  name = "treat_trendGrowMinus",
  arguments = list(
    x = quote(mktdata$SMADiff)
  ),
  label = "trendGrowMinus"
)

add.signal(
  qs.strategy,
  name = "sigThreshold",
  arguments = list(column = "trendGrowMinus", relationship = "gt",threshold=0,cross=TRUE),
  label = "signal.gt.trendGrowMinus")


#b.	当差值是负数并持续变大但不到3天立即又变小，可以用最近变大的天数减去变小的天数，
#然后跟最近变大的周期天数相加，如果大于5也可以标识为买点
treat_trendGrowPlus <- function(x, targetShortGrowDay=2, targetDiffGrowDay=3, targetDiffDownDay=2) 
{ 
  a <- matrix(FALSE,nrow=nrow(x),ncol=1)
  startIndex <- middleSMA+targetDiffGrowDay+targetDiffDownDay+1
  for (i in startIndex:nrow(x))
  {
    if(isTrendOneUpCamel(x[(i-targetDiffGrowDay-targetDiffDownDay):i]))
    {
      a[i] <- TRUE
    }
  }
  return (a) 
}

isTrendOneUpCamel <- function(x ,targetShortGrowDay=2, targetDiffGrowDay=3, targetDiffDownDay=2)
{
  topIndex <- 1
  topCount <- 0
  lowIndex <- nrow(x)
  lowCount <- 0
  for (i in 1:nrow(x))
  {
	 if((i != 1) 
	    &&(i != nrow(x))
	    && (as.numeric(x[i]) > as.numeric(x[i-1])) 
	    && (as.numeric(x[i]) > as.numeric(x[i+1])))
	 { 
	      topIndex <- i 
		  topCount <- topCount +1
	 }
	 if((i != 1) 
	    &&(i != nrow(x)) 
	    && (as.numeric(x[i]) < as.numeric(x[i-1])) 
	    && (as.numeric(x[i]) < as.numeric(x[i+1])))
	 { 
	    lowIndex <- i 
		  lowCount <- lowCount +1
	 }
  }
  if(topCount ==1
     &&lowCount==1
	 &&x[topIndex] < 0
	 &&lowIndex > topIndex
	 &&lowIndex - topIndex <= targetDiffDownDay
	 &&nrow(x) - (lowIndex - topIndex) >= targetDiffGrowDay
	 &&topIndex-1 >= targetShortGrowDay) { return (TRUE) } else { return (FALSE) }
 
}

add.indicator(
  strategy = qs.strategy,
  name = "treat_trendGrowPlus",
  arguments = list(
    x = quote(mktdata$SMADiff)
  ),
  label = "treat_trendGrowPlus"
)

add.signal(
  qs.strategy,
  name = "sigThreshold",
  arguments = list(column = "treat_trendGrowPlus", relationship = "gt",threshold=0,cross=TRUE),
  label = "signal.gt.trendGrowPlus")


add.signal(
  qs.strategy,
  name = "sigFormula",
  arguments = list(
    columns = c(
      "signal.isvolumeUp",
      "signal.gt.osc",
      "signal.gt.trendGrowMinus",
      "signal.gt.trendGrowPlus"
    ),
    formula = "(signal.isvolumeUp == 1) & ((signal.gt.osc == 1) | (signal.gt.trendGrowMinus == 1) | (signal.gt.trendGrowPlus == 1))",
    cross = FALSE
  ),
  label = "longEntry"
)  

enterAtr<-0
tradeSize <- initEq/100
osFixedMoneyFirstEntry <- function(timestamp, orderqty, portfolio, symbol, ruletype, ...)
{
  Atr <- as.numeric(mktdata[timestamp,]$atr)
  enterAtr <- Atr
  orderqty <- round(tradeSize/Atr,-2)
  return (orderqty)
}
  

#加仓，当上涨0.5个ATR并且还持有股票的时候，就增加ATR follow的买入信号
growCertainATRIndex <- function(x, index, ATRRate=0.5) 
{	
	closeTnxPrice <- Cl(x[index])
	atrTnxPrice <- x[index]$atr
	for (i in index+1:nrow(x))
    {
	   if((as.numeric(Cl(x[index])) - as.numeric(closeTnxPrice)) >= (ATRRate * as.numeric(atrTnxPrice)))
	   {
	        year <- .indexyear(x[i]) + 1900
            mon <- .indexmon(x[i]) +1
            day <- .indexmday(x[i])
			timestamp <- paste(year, mon, day,sep="-")
			print(paste("xubin result", timestamp,sep=":"))
			symbol <- strsplit(colnames(x)[1],"[.]")
			pos <- getPosQty(multi.trend, symbol, timestamp)
			if(pos > 0)
			  return (i)
	   }
    }
	return (0)
}

findGrowATRSig <- function(x, index, ATRRate=0.5) 
{
#如果符合趋势，这index+1是交易点
     if(x[index]$X1.isvolumeUp == 1 & (x[index]$X1.treat_trendGrowPlus == 1 | x[index]$X1.trendGrowMinus ==1 | x[index]$X1.trained_osc==1 ))  
	 #if(x[index]$longEntry == 1) 
	 {
	     tnxIndex <- index+1
		 return (growCertainATRIndex(x,tnxIndex))
	 }
	 else
	 {
	    return (0)
	 }
}

addGrowATRSig <- function(x) 
{
  a <- matrix(FALSE,nrow=nrow(x),ncol=1)
  for (i in 2:nrow(x))
  {
    index <- findGrowATRSig(x,i)
	if(index != 0)
    {
      a[index] <- TRUE
    }
  }
  return (a) 
}


add.indicator(
  strategy = qs.strategy,
  name = "addGrowATRSig",
  arguments = list(
    x = quote(mktdata)
  ),
  label = "atrTrendFollow"
)

add.signal(
  qs.strategy,
  name = "sigThreshold",
  arguments = list(column = "atrTrendFollow", relationship = "gt",threshold=0,cross=TRUE),
  label = "signal.gt.atrTrendFollow")	   

#加仓，每次剩余资金的10%
osPercentEquity <- function(timestamp, orderqty, portfolio,symbol, ruletype,trade.percent = 0.1,...)
{
    trading.pl <-sum(getTxns(Portfolio = portfolio, Symbol = symbol)$Txn.Value)
    total.equity <- initEq - trading.pl
    tradeSize <- total.equity * trade.percent
    ClosePrice <- as.numeric(Cl(mktdata[timestamp, ]))
    orderqty <- round(tradeSize / ClosePrice,-2)
    return(orderqty)
}
  

	
######################################################

#d.	当差值是正数但是持续变小超过3或5天，或者变负数时标识为卖点。
isTrendDown <- function(matrixList)
{
  for (i in 1:(nrow(matrixList)-1))
  {
    if(matrixList[i] < matrixList[i+1])
    {
      return (FALSE)
    }
  }
  return (TRUE)
}

treat_trendDownMinus <- function(x, targetDiffDownDay=3) 
{ 
  a <- matrix(FALSE,nrow=nrow(x),ncol=1)
  startIndex <- middleSMA+targetDiffDownDay
  for (i in startIndex:nrow(x))
  {
    if(isTrendDown(as.matrix(x[(i-targetDiffDownDay):i])) && x[i] > 0)
    {
      a[i] <- TRUE
    }
      if(x[i] < 0 && x[i-1] > 0)
      {
        a[i] <- TRUE
      }
  }
  return (a) 
}
add.indicator(
  strategy = qs.strategy,
  name = "treat_trendDownMinus",
  arguments = list(
    x = quote(mktdata$SMADiff)
  ),
  label = "trendDownMinus"
)

add.signal(
  qs.strategy,
  name = "sigThreshold",
  arguments = list(column = "trendDownMinus", relationship = "lt",threshold=0,cross=TRUE),
  label = "signal.gt.trendDownMinus")

#e.	当差值是正数并持续变小但不到3天立即又变大，可以用最近变小的天数减去变大的天数，然后跟最近变小的周期天数相加，如果大于5也可以标识为卖点
treat_trendDownPlus <- function(x, targetShortGrowDay=2, targetDiffGrowDay=3, targetDiffDownDay=2) 
{ 
  a <- matrix(FALSE,nrow=nrow(x),ncol=1)
  startIndex <- middleSMA+targetDiffGrowDay+targetDiffDownDay+1
  for (i in startIndex:nrow(x))
  {
    if(isTrendOneDwonCamel(x[(i-targetDiffGrowDay-targetDiffDownDay):i]))
    {
      a[i] <- TRUE
    }
  }
  return (a) 
}

isTrendOneDwonCamel <- function(x ,targetShortDownDay=2, targetDiffDownDay=3, targetDiffUpDay=2)
{
  topIndex <- 1
  topCount <- 0
  lowIndex <- nrow(x)
  lowCount <- 0
  for (i in 1:nrow(x))
  {
	 if((i != 1) 
	    &&(i != nrow(x))
	    && (as.numeric(x[i]) > as.numeric(x[i-1])) 
	    && (as.numeric(x[i]) > as.numeric(x[i+1])))
	 { 
	      topIndex <- i 
		  topCount <- topCount +1
	 }
	 if((i != 1) 
	    &&(i != nrow(x)) 
	    && (as.numeric(x[i]) < as.numeric(x[i-1])) 
	    && (as.numeric(x[i]) < as.numeric(x[i+1])))
	 { 
	    lowIndex <- i 
		  lowCount <- lowCount +1
	 }
  }
  if(topCount ==1
     &&lowCount==1
	 &&x[lowIndex] > 0
	 &&lowIndex < topIndex
	 &&topIndex - lowIndex <= targetDiffUpDay
	 &&nrow(x) - (lowIndex - topIndex) >= targetDiffDownDay
	 &&lowIndex-1 >= targetShortDownDay) { return (TRUE) } else { return (FALSE) }
 
}

add.indicator(
  strategy = qs.strategy,
  name = "treat_trendDownPlus",
  arguments = list(
    x = quote(mktdata)
  ),
  label = "treat_trendDownPlus"
)

add.signal(
  qs.strategy,
  name = "sigThreshold",
  arguments = list(column = "treat_trendDownPlus", relationship = "gt",threshold=0,cross=TRUE),
  label = "signal.gt.trendDownPlus")

add.signal(
  qs.strategy,
  name = "sigFormula",
  arguments = list(
    columns = c(
      "signal.gt.trendDownMinus",
      "signal.gt.trendDownPlus"
    ),
    formula = "(signal.gt.trendDownMinus == 1) | (signal.gt.trendDownPlus == 1)",
    cross = FALSE
  ),
  label = "longExit"
) 

#########################################
# last(getPrice(mktdata)[paste('::',as.character(curIndex),sep='')][,1]) * 0.00003
# normal enter
.txnFees=-0.1
#applyRules(portfolio=multi.trend, symbol='SH600000',strategy=qs.strategy, mktdata=mktdata)  
add.rule(
  qs.strategy,
  name = 'ruleSignal',
  arguments = list(
    sigcol = "longEntry",
    sigval = TRUE,
	replace=FALSE,
	TxnFees=.txnFees,
    orderqty = 900,
    ordertype = 'market',
	prefer='High',
    orderside = 'long',
    osFUN='osFixedMoneyFirstEntry',
	orderset='ocolong'),
  type = 'enter',
  label='FirstEnter'
)

#ATR tradeFollow enter
add.rule(
  qs.strategy,
  name = 'ruleSignal',
  arguments = list(
    sigcol = "signal.gt.atrTrendFollow",
    sigval = TRUE,
	replace=FALSE,
	TxnFees=.txnFees,
    orderqty = 900,
    ordertype = 'market',
	prefer='High',
    orderside = 'long',
    osFUN='osPercentEquity',
	orderset='ocolong'),
  type = 'enter',
  label='ATRFollowEnter'
)

#normal exit 
add.rule(
  qs.strategy,
  name = 'ruleSignal',
  arguments = list(
    sigcol = "longExit",
    sigval = TRUE,
	replace=TRUE,
	prefer='Low',
	TxnFees=.txnFees,
    orderqty = 'all',
    ordertype = 'market',
    orderside = 'long',
	orderset='ocolong'),
  type = 'exit',
  label='ExitLONG'
)

#stop loss
stopLossPercent <- 0.01
add.rule(
  qs.strategy,
  name = 'ruleSignal',
  arguments = list(
    sigcol = "longEntry",
    sigval = TRUE,
	replace=FALSE,
	TxnFees=.txnFees,
	orderside = 'long',
    ordertype = 'stoplimit',
    tmult=TRUE,
	threshold=quote( stopLossPercent ),
	orderqty = 'all',
	orderset='ocolong'),
    type = 'chain',
	parent="FirstEnter",
	label='StopLossLong',
	enabled=FALSE) 

.stoptrailing=0.01	
add.rule(
      qs.strategy, 
      name = 'ruleSignal',
	  arguments=list(
		sigcol='longEntry', 
		sigval=TRUE,
		replace=FALSE,
		orderside='long',
		ordertype='stoptrailing', 
		tmult=TRUE, 
		threshold=quote(.stoptrailing),
		TxnFees=.txnFees,
		orderqty='all',
		orderset='ocolong'
	),
	type='chain', parent='EnterLONG',
	label='StopTrailingLONG',
	enabled=FALSE
)


add.rule(
      qs.strategy, 
      name = 'ruleSignal',
	  arguments=list(
		sigcol='signal.gt.atrTrendFollow', 
		sigval=TRUE,
		replace=FALSE,
		orderside='long',
		ordertype='stoptrailing', 
		tmult=TRUE, 
		threshold=quote(.stoptrailing),
		TxnFees=.txnFees,
		orderqty='all',
		orderset='ocolong'
	),
	type='chain', parent='ATRFollowEnter',
	label='StopTrailingATR',
	enabled=FALSE
)


enable.rule(qs.strategy, type="chain", label="StopLossLong")
enable.rule(qs.strategy, type="chain", label="StopTrailingLONG")
	
#########################################

#applyIndicators(qs.strategy, SH600000)
applyStrategy(strategy=qs.strategy, portfolios=multi.trend)

updatePortf(multi.trend)
updateAcct(multi.trend)
updateEndEq(multi.trend)	

#getTxns(Portfolio=multi.trend, Symbol="SH600000", "2002-01-07")
getTxns(Portfolio=multi.trend, Symbol="SH600000")







