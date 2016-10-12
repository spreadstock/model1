library(quantstrat)

startDate <- '2011-01-01'
endDate <- '2016-09-02'
stock.folder <- 'C:/Users/exubixu/Desktop/Imp/git_new/model1/StockDatas/2016-08-09-Former_Rehabilitation_leaned/'
symbols = c("SH600681","SH600353")
#stock.folder <- 'C:/Users/exubixu/Desktop/new1/'
source.folder <- 'C:/Users/exubixu/Desktop/Imp/git_new/model1/'
result.folder <- 'C:/Users/exubixu/Desktop/result/'

shortSMA <- 5
middleSMA <- 13
initEq <- 50000 * length(symbols)
tradeSize <- 45000
pct <- 0.5
minTradeSize <- tradeSize * pct

initDate <- '2001-08-08'
currency('USD')
Sys.setenv(TZ="UTC")
multi.trend <- "multi.trend"
qs.strategy <- "trend1"
source(paste0(source.folder,"trend_strategy_funciton1.r"))
source(paste0(source.folder,"commonPackages.r"))


#symbols <- listStocksFromDir(stock.folder)

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
  
  atr <- as.numeric(mktdata[timestamp,]$atr)
  orderqty <- round(tradeSizeNow/atr,-2)  
  price <- as.numeric(Cl(mktdata[timestamp, ]))
  atrSize <- orderqty * price
  if(tradeSizeNow < minTradeSize)
  {
    desireSize <- tradeSizeNow
  }
  else
  {
    desireSize <- minTradeSize
  }
  
  if(atrSize > desireSize)
  {
	orderqty <- round(desireSize/price,-2)
  }
  return (orderqty)
}
  

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
	mktdata[tnxIndex]$atr
	mktdata["2014-04-18::2014-04-18",]
	price <- as.numeric(Op(mktdata[timestamp, ]))
	Portfolio <- get(paste("portfolio", multi.trend, sep = "."), envir = .blotter)
    tnxlast <- last(Portfolio$symbols[[symbol]]$txn[paste('::',timestamp,sep='')])
	print(paste0("tnxlast:",tnxlast$Txn.Price))
	
    trading.pl <- sum(getTxns(Portfolio = portfolio, Symbol = symbol)$Txn.Value)
    trading.fee <- 0 - sum(getTxns(Portfolio = portfolio, Symbol = symbol)$Txn.Fees)
    total.equity <- tradeSize - trading.pl - trading.fee	
    tradeSizeNow <- total.equity
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
# last(getPrice(mktdata)[paste('::',as.character(curIndex),sep='')][,1]) * 0.05
# normal enter
trade.percent=-0.004

getTnxFee <- function(TxnQty, TxnPrice, Symbol)
{
  print(paste0("getTnxFee TxnQty:",TxnQty))
  print(paste0("getTnxFee TxnPrice:",TxnPrice))
  qty <- abs(TxnQty)
  if(qty == 0)
  {
    fee <- 0
  }
  else
  {
    #fee <- round(5000 * TxnPrice * trade.percent, 2)
    #fee <- TxnQty * TxnPrice * trade.percent
    transFeeShanghai <- round(qty/1000,0)
    if(transFeeShanghai < 1)
    {
      transFeeShanghai <- 1
    }
    fee <- round(qty * TxnPrice * trade.percent, 2) - transFeeShanghai
  }
  print(paste0("getTnxFee fee:",fee))
  return(fee)
}

#applyRules(portfolio=multi.trend, symbol='SH600000',strategy=qs.strategy, mktdata=mktdata)  
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


enable.rule(qs.strategy, type="chain", label="StopTrailingLONG")



#########################################
require(doParallel)
registerDoParallel(cores=4)


#sink(paste0(result.folder,'aa.txt'))
applyStrategy(strategy=qs.strategy, portfolios=multi.trend)
#sink()

updatePortf(multi.trend)
updateAcct(multi.trend)
updateEndEq(multi.trend)	


#sink(paste0(result.folder,'SZ000040','tnx.txt')
#getTxns(Portfolio=multi.trend, Symbol='SZ000040')
#sink()

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

#getStaticInfo(multi.trend,symbols,result.folder,tradeSize)


#查看transaction 历史
#sink(paste0(result.folder,'statistic.txt'))
#t(tradeStats(multi.trend))
#sink()

#sink(paste0(result.folder,'trade.txt'))
#perTradeStats(multi.trend)
#sink()

write.csv(x=mktdata, file=paste0(result.folder,'SH600684','mktdata.csv'), row.names = TRUE)

#sink(paste0(result.folder,'account.txt'))
#getAccount(multi.trend)
#sink()



