library(quantstrat)

startDate <- '2011-01-01'
endDate <- '2016-09-02'
stock.folder <- 'C:/Users/exubixu/Desktop/Imp/git_new/model1/StockDatas/2016-08-09-Former_Rehabilitation_leaned/'
source.folder <- 'C:/Users/exubixu/Desktop/Imp/git_new/model1/'
result.folder <- 'C:/Users/exubixu/Desktop/result/'
clustering.folder <- "C:/Users/exubixu/Desktop/Imp/git_new/model1/testResult/testRel/"
clustering.name <- "clusteringResult_bk"
data.folder <- 'C:/Users/exubixu/Desktop/Imp/git_new/model1/StockDatas/2016-08-09-Later_Rehabilitation_Cleaned/'


shortSMA <- 5
middleSMA <- 13
initEq <- 50000 * length(symbols)
tradeSize <- 45000
pct <- 0.5
minTradeSize <- tradeSize * pct
MaxPos <- 35000  #max position in stockA; 
# max position in stock B will be max * ratio, i.e. no hard position limit in 
# Stock B
lvls <- 3 #how many times to fade; Each order's qty will = MaxPos/lvls

#clean up
if (!exists('.blotter')) .blotter <- new.env()
if (!exists('.strategy')) .strategy <- new.env() 
suppressWarnings(rm(list = ls(envir = .blotter), envir = .blotter))
suppressWarnings(rm(list = ls(envir = .strategy), envir = .strategy))
rm.strat(qs.strategy)
rm.strat(multi.trend)

initDate <- '2001-08-08'
currency('USD')
Sys.setenv(TZ="UTC")
multi.trend <- "multi.trend"
qs.strategy <- "trend1"
source(paste0(source.folder,"trend_strategy_funciton1.r"))
source(paste0(source.folder,"commonPackages.r"))
source(paste0(source.folder,"pair_spreadImp.r"))
source(paste0(source.folder,"pairForTrend.r"))
source(paste0(source.folder,"trend_selectStock.r"))

#symbList <- getTrendMatchStocks(data.folder)
symbList <- c("SH600097","SH600183","SH600303","SH600697","SH601007","SZ000029","SZ000040","SZ000043","SZ000505","SZ000538","SZ002409")
#symbList <- c("SH600097")
pairList <- matchPairs(clustering.folder,clustering.name,symbList)
newSymbList <- unique(c(symbList,as.vector(na.omit(pairList[-1,]))))
#need remove NO
symbols <- newSymbList[newSymbList!="NO"]



for(symbol in symbols) 
{ 
  stock(symbol, currency='USD',multiplier=1)
  a  <-  loadStock(stock.folder, symbol, operation.name="all") 
  a <- subsetByDateRange(a,startDate,endDate)
  a$SMAShort <- SMA(Cl(a),shortSMA)
  a$SMAMid <- SMA(Cl(a),middleSMA)
  a$SMADiff <- a$SMAShort - a$SMAMid
  a$atr <- ATR(a)$atr
  assign(symbol,a)
  rm(a)
}

#prepare stock data besides market data
stockData <- Cl(get(symbols[1]))
for (aStockName in symbols[-1]) {
  stockData <- cbind(stockData, Cl(get(aStockName)))
}


initPortf(multi.trend, symbols, initDate = initDate)
initAcct(
  multi.trend,
  portfolios = multi.trend,
  initDate = initDate,
  initEq = initEq
)
initOrders(portfolio = multi.trend, initDate = initDate)

strategy(qs.strategy, store =TRUE)

setupPairsGlobals(multi.trend, na.omit(pairList),lvls)

#cc.最近三天的收盘价都在10天均线之上
add.indicator(
  strategy = qs.strategy,
  name = "isOverSMA10LastDays",
  arguments = list(
    x = quote(Cl(mktdata))
  ),
  label = "isOverSMA10Recently"
)

add.signal(
  qs.strategy,
  name = "sigThreshold",
  arguments = list(column = "isOverSMA10Recently", relationship = "gt",threshold=0,cross=FALSE),
  label = "signal.isOverSMA10Recently")

#c.	以上买点还必须符号最近10天里上涨时的平均交易量大于下跌时的平均交易量。
add.indicator(
  strategy = qs.strategy,
  name = "isvolumeUp",
  arguments = list(
    x = quote(mktdata)
  ),
  label = "isvolumeUp"
)
#cross=FALSE 每一个点都会标记为操作点
#cross=TRUE 连续的点会被忽略，直到下一次cross
add.signal(
  qs.strategy,
  name = "sigThreshold",
  arguments = list(column = "isvolumeUp", relationship = "gt",threshold=0,cross=FALSE),
  label = "signal.isvolumeUp")

  
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
  arguments = list(column = "trendGrowMinus", relationship = "gt",threshold=0,cross=FALSE),
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
  arguments = list(column = "treat_trendGrowPlus", relationship = "gt",threshold=0,cross=FALSE),
  label = "signal.gt.trendGrowPlus")


add.signal(
  qs.strategy,
  name = "sigFormula",
  arguments = list(
    columns = c(
      "signal.isvolumeUp",
      "signal.isOverSMA10Recently",
      "signal.gt.trendGrowMinus",
      "signal.gt.trendGrowPlus"
    ),
    formula = "(signal.isvolumeUp == 1) & (signal.isOverSMA10Recently == 1) & ((signal.gt.trendGrowMinus == 1) | (signal.gt.trendGrowPlus == 1))",
    cross = TRUE
  ),
  label = "longEntry"
)  



#如果剩余的钱已经小于minTradeSize，全部用完，不然就用minTradeSize
osFixedMoneyEntry <- function(timestamp, orderqty, portfolio, symbol, ruletype, ...)
{
  trading.pl <- sum(getTxns(Portfolio = portfolio, Symbol = symbol)$Txn.Value)
  trading.fee <- 0 - sum(getTxns(Portfolio = portfolio, Symbol = symbol)$Txn.Fees)
  total.equity <- tradeSize - trading.pl - trading.fee
  print(paste0("osFixedMoneyEntry trading.pl:",trading.pl))
  print(paste0("osFixedMoneyEntry trading.fee:",trading.fee))
  print(paste0("osFixedMoneyEntry total.equity:",total.equity))
  
  if(total.equity < 0)
  {
     orderqty <- 0
     return (orderqty)
  }
  if(trading.pl == 0)
  {    
	 tradeSizeNow <- tradeSize
  }
  else
  {
     tradeSizeNow <- total.equity
  }
  print(paste0("osFixedMoneyEntry ttradeSizeNow:",tradeSizeNow))
  atr <- as.numeric(get(symbol)[timestamp,]$atr)
  orderqty <- round(tradeSizeNow/atr,-2)  
  price <- as.numeric(Op(get(symbol)[timestamp, ]))
  atrSize <- orderqty * price
  if(tradeSizeNow < minTradeSize)
  {
    desireSize <- tradeSizeNow
  }
  else
  {
    desireSize <- minTradeSize
  }
  print(paste0("osFixedMoneyEntry desireSize:",desireSize))
  if(atrSize > desireSize)
  {
	orderqty <- round(desireSize/price,-2)
  }
  print(paste0("osFixedMoneyEntry timestamp:",timestamp))
  print(paste0("osFixedMoneyEntry price:",price))
  print(paste0("osFixedMoneyEntry trading.pl:",trading.pl))
  print(paste0("osFixedMoneyEntry orderqty:",orderqty))

	
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
  arguments = list(column = "atrTrendFollow", relationship = "gt",threshold=0,cross=FALSE),
  label = "signal.gt.atrTrendFollow")	   

#加仓，如果剩余资金起始的50%
osPercentEquity <- function(timestamp, orderqty, portfolio,symbol, ruletype,trade.percent = 0.5,...)
{
    pos <- getPosQty(multi.trend, symbol, timestamp)
	if(pos == 0)
	{
	   orderqty <- 0
	   print(paste0("osPercentEquity:",orderqty))
	   return (orderqty)
	}
	price <- as.numeric(Op(get(symbol)[timestamp, ]))
	
    trading.pl <- sum(getTxns(Portfolio = portfolio, Symbol = symbol)$Txn.Value)
    trading.fee <- 0 - sum(getTxns(Portfolio = portfolio, Symbol = symbol)$Txn.Fees)
    total.equity <- tradeSize - trading.pl - trading.fee	
    tradeSizeNow <- total.equity
	print(paste0("pos:",pos))
	print(paste0("trading.pl:",trading.pl))
	print(paste0("trading.fee:",trading.fee))
	print(paste0("total.equity:",total.equity))
	
    if(total.equity < 0)
    {
	   orderqty <- 0
       return (orderqty)
    }	

    if(tradeSizeNow < minTradeSize)
    {
      desireSize <- tradeSizeNow
    }
    else
    {
      desireSize <- minTradeSize
    }	
    orderqty <- round(desireSize/price,-2)
	print(paste0("here price:",price))
	print(paste0("here timestamp:",timestamp))
	print(paste0("here orderqty:",orderqty))
    return(orderqty)
}

	
######################################################
#dd.最近三天的收盘价都在10天均线之下
add.indicator(
  strategy = qs.strategy,
  name = "isBelowSMA10LastDays",
  arguments = list(
    x = quote(Cl(mktdata))
  ),
  label = "isBelowSMA10Recently"
)

add.signal(
  qs.strategy,
  name = "sigThreshold",
  arguments = list(column = "isBelowSMA10Recently", relationship = "gt",threshold=0,cross=FALSE),
  label = "signal.isBelowSMA10Recently")

  
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
  arguments = list(column = "trendDownMinus", relationship = "gt",threshold=0,cross=FALSE),
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
  arguments = list(column = "treat_trendDownPlus", relationship = "gt",threshold=0,cross=FALSE),
  label = "signal.gt.trendDownPlus")

add.signal(
  qs.strategy,
  name = "sigFormula",
  arguments = list(
    columns = c(
	  "signal.isBelowSMA10Recently",
      "signal.gt.trendDownMinus",
      "signal.gt.trendDownPlus"
    ),
    formula = "(signal.isBelowSMA10Recently == 1) & ((signal.gt.trendDownMinus == 1) | (signal.gt.trendDownPlus == 1))",
    cross = FALSE
  ),
  label = "longExit"
) 

#########################################
# normal enter
trade.percent=-0.004

getTnxFee <- function(TxnQty, TxnPrice, Symbol)
{
  #print(paste0("getTnxFee TxnQty:",TxnQty))
  #print(paste0("getTnxFee TxnPrice:",TxnPrice))
  qty <- abs(TxnQty)
  if(qty == 0)
  {
    fee <- 0
  }
  else
  {
    transFeeShanghai <- round(qty/1000,0)
    if(transFeeShanghai < 1)
    {
      transFeeShanghai <- 1
    }
    fee <- round(qty * TxnPrice * trade.percent, 2) - transFeeShanghai
  }
  #print(paste0("getTnxFee fee:",fee))
  return(fee)
}
 
add.rule(
  qs.strategy,
  name = 'ruleSignal',
  arguments = list(
    sigcol = "longEntry",
    sigval = TRUE,
	replace=FALSE,
	TxnFees='getTnxFee',
    orderqty = 100,
    ordertype = 'market',
	prefer='Open',
    orderside = 'long',
    osFUN='osFixedMoneyEntry',
	orderset='ocolong'),
  type = 'enter',
  label='EnterLONG'
)


#ATR tradeFollow enter
add.rule(
  qs.strategy,
  name = 'ruleSignal',
  arguments = list(
    sigcol = "signal.gt.atrTrendFollow",
    sigval = TRUE,
	replace=FALSE,
	TxnFees='getTnxFee',
    orderqty = 100,
    ordertype = 'market',
	prefer='Open',
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
	prefer='Open',
    orderqty = 'all',
    ordertype = 'market',
    orderside = 'long',
	orderset='ocolong'),
  type = 'exit',
  label='ExitLONG'
)
 

.stoptrailing=0.1
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
		orderqty='all',
		orderset='ocolong'
	),
	type='chain', parent='ATRFollowEnter',
	label='StopTrailingATR',
	enabled=FALSE
)

enable.rule(qs.strategy, type="chain", label="StopTrailingLONG")
enable.rule(qs.strategy, type="chain", label="StopTrailingATR")	

#########################################

setupPairsSignals(qs.strategy, stockData)

add.rule(
      qs.strategy, 
      name = 'ruleSignal',
	  arguments=list(
		sigcol='Stock.upperAdj', 
		sigval=TRUE,
		ordertype='stoptrailing', 
		orderside='long',		
		replace=FALSE,
		tmult=TRUE, 
		threshold=quote(.stoptrailing),
		orderqty='all',
		orderset='ocolong'
	),
	type='chain', parent='pairEnterUpper',
	label='StopTrailingPairEnterUpper',
	enabled=FALSE
)

add.rule(
      qs.strategy, 
      name = 'ruleSignal',
	  arguments=list(
		sigcol='Stock.lowerAdj', 
		sigval=TRUE,
		replace=FALSE,
		orderside='long',
		ordertype='stoptrailing', 
		tmult=TRUE, 
		threshold=quote(.stoptrailing),
		orderqty='all',
		orderset='ocolong'
	),
	type='chain', parent='pairEnterLower',
	label='StopTrailingPairEnterLower',
	enabled=FALSE
)  

enable.rule(qs.strategy, type="chain", label="StopTrailingPairEnterLower")
enable.rule(qs.strategy, type="chain", label="StopTrailingPairEnterUpper")	

#########################################
require(doParallel)
registerDoParallel(cores=4)


sink(paste0(result.folder,'aa.txt'))
reblanceImp(strategy=qs.strategy, portfolios=multi.trend)
sink()

updatePortf(multi.trend)
updateAcct(multi.trend)
updateEndEq(multi.trend)	


for(symbol in symbols) 
{ 
  sink(paste0(result.folder,symbol,'tnx.txt'))
  tnx <- getTxns(Portfolio=multi.trend, symbol)
  print(tnx)
  sink()
}

sink(paste0(result.folder,'order.txt'))
getOrderBook(multi.trend)
sink()

getStaticInfo(multi.trend,multi.trend,symbols,result.folder,tradeSize)



