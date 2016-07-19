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


initPortf(multi.trend, symbols, initDate = initDate)
initAcct(
  multi.trend,
  portfolios = multi.trend,
  initDate = initDate,
  initEq = initEq
)
initOrders(portfolio = multi.trend, initDate = initDate)
rm.strat(qs.strategy)
strategy(qs.strategy, store =TRUE)
#############################################
add.indicator(strategy=qs.strategy,name="MACD",arguments=list(x=quote(Cl(mktdata))),label="osc")

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
#a.	当差值是负数但是持续变大超过3或5天时，标识为买点
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
applyIndicators(qs.strategy, SH600000)

applyStrategy(strategy=qs.strategy, portfolios=multi.trend)

updatePortf(multi.trend)
updateAcct(multi.trend)
updateEndEq(multi.trend)	

getTxns(Portfolio=multi.trend, Symbol="SH600000")




