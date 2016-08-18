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
source.folder <- 'C:/Users/exubixu/Desktop/Imp/git_new/model1/'
source(paste0(source.folder,"trend_strategy_funciton1.r"))
source(paste0(source.folder,"commonPackages.r"))



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







