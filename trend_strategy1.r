library(quantstrat)
startDate <- '2010-01-01'
endDate <- Sys.Date()
Sys.setenv(TZ="UTC")
multi.trend <- "multi.trend"
qs.strategy <- "trend1"
shortSMA <- 5
middleSMA <- 13

stock.folder <- 'C:/Users/exubixu/Desktop/new1/'
symbols <- listStocksFromDir(stock.folder)
for(symbol in symbols) 
{ 
  a  <-  loadStock(stock.folder, symbol, operation.name="all") 
  a$SMA5 <- SMA(Cl(a),shortSMA)
  a$SMA13 <- SMA(Cl(a),middleSMA)
  a$SMADiff <- a$SMA5 - a$SMA13
  assign(symbol,a)
  rm(a)
}

initDate <- '2001-08-08'
initEq <- 1e6
currency('USD')
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
#############################################
add.indicator(strategy=qs.strategy,name="MACD",arguments=list(x=quote(Cl(mktdata))),label="osc")


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


add.rule(
  qs.strategy,
  name = 'ruleSignal',
  arguments = list(
    sigcol = "signal.gt.trendGrowPlus",
    sigval = TRUE,
    orderqty = 900,
    ordertype = 'market',
    orderside = 'long'),
  type = 'enter'
)

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
  arguments = list(column = "trendGrowMinus", relationship = "gt",threshold=0,cross=TRUE),
  label = "signal.gt.trendGrowMinus")


add.rule(
  qs.strategy,
  name = 'ruleSignal',
  arguments = list(
    sigcol = "signal.gt.trendGrowMinus",
    sigval = TRUE,
    orderqty = 900,
    ordertype = 'market',
    orderside = 'long'),
  type = 'enter'
)

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


add.rule(
  qs.strategy,
  name = 'ruleSignal',
  arguments = list(
    sigcol = "signal.gt.trendDownMinus",
    sigval = TRUE,
    orderqty = -900,
    ordertype = 'market',
    orderside = 'long'),
  type = 'enter'
)
#applyIndicators(qs.strategy, SH600000)

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


add.rule(
  qs.strategy,
  name = 'ruleSignal',
  arguments = list(
    sigcol = "signal.gt.trendDownPlus",
    sigval = TRUE,
    orderqty = -900,
    ordertype = 'market',
    orderside = 'long'),
  type = 'enter'
)

applyStrategy(strategy=qs.strategy, portfolios=multi.trend)

#########################################

updatePortf(multi.trend)
updateAcct(multi.trend)
updateEndEq(multi.trend)	

getTxns(Portfolio=multi.trend, Symbol="SH600000")






