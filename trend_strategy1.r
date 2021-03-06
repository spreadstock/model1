library(quantstrat)
startDate <- '2010-01-01'
endDate <- Sys.Date()
Sys.setenv(TZ="UTC")
multi.trend <- "multi.trend"
qs.strategy <- "trend1"
shortSMA <- 5
middleSMA <- 13
stock.folder <- 'C:/Users/exubixu/Desktop/Imp/git_new/model1/StockDatas/2016-08-09-Former_Rehabilitation_leaned/'
initDate <- '2001-08-08'

currency('USD')
source.folder <- 'C:/Users/exubixu/Desktop/Imp/git_new/model1/'
source(paste0(source.folder,"trend_strategy_funciton1.r"))
source(paste0(source.folder,"commonPackages.r"))
source(paste0(source.folder,"pairForTrend.r"))
source(paste0(source.folder,"trend_selectStock.r"))
MaxPos <- 35000  #max position in stockA; 
# max position in stock B will be max * ratio, i.e. no hard position limit in 
# Stock B
lvls <- 4 #how many times to fade; Each order's qty will = MaxPos/lvls

#for test
#symbols <- listStocksFromDir(stock.folder)
#symbList = c("SH600398","SH600446","SH600775")
data.folder <- 'C:/Users/exubixu/Desktop/Imp/git_new/model1/StockDatas/2016-08-09-Later_Rehabilitation_Cleaned/'
symbList <- getTrendMatchStocks(data.folder)
clustering.folder <- "C:/Users/exubixu/Desktop/Imp/git_new/model1/testResult/testRel/"
clustering.name <- "clusteringResult_bk"
pairList <- matchPairs(clustering.folder,clustering.name,symbList)
newSymbList <- unique(c(symbList,as.vector(na.omit(pairList[-1,]))))
#need remove NO
symbols <- newSymbList[newSymbList!="NO"]

initEq <- 50000 * length(symbols)
enterAtr<-0
tradeSize <- 45000
pct <- 0.5
minTradeSize <- tradeSize * pct

for(symbol in symbols) 
{ 
  stock(symbol, currency='USD',multiplier=1)
  a  <-  loadStock(stock.folder, symbol, operation.name="all") 
  a <- subsetByDateRange(a,'2011-01-01','2016-09-02')
  a$SMAShort <- SMA(Cl(a),shortSMA)
  a$SMAMid <- SMA(Cl(a),middleSMA)
  a$SMADiff <- a$SMAShort - a$SMAMid
  a$osc <- MACD(Cl(a))
  a$atr <- ATR(a)$atr
  assign(symbol,a)
  rm(a)
}


stockData <- Cl(get(symbols[[1]]))
for (aStockName in symbols[-1]) {
  stockData <- cbind(stockData, Cl(get(aStockName)))
}
setupPairsGlobals(multi.trend, na.omit(pairList),lvls)


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
#cross=FALSE 每一个点都会标记为操作点
#cross=TRUE 连续的点会被忽略，直到下一次cross
add.signal(
  qs.strategy,
  name = "sigThreshold",
  arguments = list(column = "isvolumeUp", relationship = "gt",threshold=0,cross=FALSE),
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
  arguments = list(column = "trained_osc", relationship = "gt",threshold=0,cross=FALSE),
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
      "signal.gt.osc",
      "signal.gt.trendGrowMinus",
      "signal.gt.trendGrowPlus"
    ),
    formula = "(signal.isvolumeUp == 1) & ((signal.gt.osc == 1) | (signal.gt.trendGrowMinus == 1) | (signal.gt.trendGrowPlus == 1))",
    cross = FALSE
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
     print(paste0("osFixedMoneyFirstEntry skip:",total.equity))
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
	print("xubin2")
	orderqty <- round(desireSize/price,-2)
  }
  print(paste0("osFixedMoneyFirstEntry used :",trading.pl))
  print(paste0("osFixedMoneyFirstEntry remain :",total.equity))
  print(paste0("osFixedMoneyFirstEntry desireSize :",desireSize))
  print(paste0("osFixedMoneyFirstEntry price :",price))
  print(paste0("osFixedMoneyFirstEntry:",orderqty))
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

    trading.pl <- sum(getTxns(Portfolio = portfolio, Symbol = symbol)$Txn.Value)
    trading.fee <- 0 - sum(getTxns(Portfolio = portfolio, Symbol = symbol)$Txn.Fees)
    total.equity <- tradeSize - trading.pl - trading.fee	
    tradeSizeNow <- total.equity
    if(total.equity < 0)
    {
	   orderqty <- 0
       print(paste0("osPercentEquity skip:",total.equity))
       return (orderqty)
    }	
    price <- as.numeric(Cl(mktdata[timestamp, ]))
    if(tradeSizeNow < minTradeSize)
    {
      desireSize <- tradeSizeNow
    }
    else
    {
      desireSize <- minTradeSize
    }	
    orderqty <- round(desireSize/price,-2)
	
	print(paste0("osPercentEquity used :",trading.pl))
	print(paste0("osPercentEquity remain :",total.equity))
    print(paste0("osPercentEquity desireSize :",desireSize))	
	print(paste0("osPercentEquity price :",price))
	print(paste0("osPercentEquity:",orderqty))
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
  arguments = list(column = "trendDownMinus", relationship = "lt",threshold=0,cross=FALSE),
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
      "signal.gt.trendDownMinus",
      "signal.gt.trendDownPlus"
    ),
    formula = "(signal.gt.trendDownMinus == 1) | (signal.gt.trendDownPlus == 1)",
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
	fee <- round(qty * TxnPrice * trade.percent, 2) + transFeeShanghai
  }
  print(paste0("getTnxFee abs:",qty))
  print(paste0("getTnxFee:",fee))
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
	TxnFees='getTnxFee',
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
		TxnFees='getTnxFee',
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
		TxnFees='getTnxFee',
		orderqty='all',
		orderset='ocolong'
	),
	type='chain', parent='ATRFollowEnter',
	label='StopTrailingATR',
	enabled=FALSE
)

ruleReblance <- function (..., portfolio, symbol, timestamp) 
{
  return (1)
}

add.rule(qs.strategy, 'ruleReblance',
         arguments=list(rebalance_on='days'),
         type='rebalance',
         label='rebalance'
)

enable.rule(qs.strategy, type="chain", label="StopTrailingLONG")
enable.rule(qs.strategy, type="chain", label="StopTrailingATR")	

#setup Pairs Signals
add.indicator(
    strategy = qs.strategy,
    name = "calculate_betaforTrend",
    arguments = list(
      x = quote(Cl(stockData)),
      currentStockName = quote(colnames(Cl(mktdata)))
    ),
    label = "SPREAD"
  )
  
  add.signal(
    qs.strategy,
    name = "sigCrossover",
    arguments = list(columns = c("BetaTotal.SPREAD", "Upper.SPREAD"), relationship = "gt"),
    label = "Spread.cross.upper"
  )
  
  
  add.signal(
    qs.strategy,
    name = "sigCrossover",
    arguments = list(columns = c("BetaTotal.SPREAD", "Lower.SPREAD"), relationship = "lt"),
    label = "Spread.cross.lower"
  )
  
  add.signal(
    qs.strategy,
    name = "sigFormula",
    arguments = list(
      columns = c(
        "Spread.cross.upper"
      ),
      formula = "(Spread.cross.upper == 1)",
      cross = FALSE
    ),
    label = "Stock.upperAdj"
  )
  
  add.signal(
    qs.strategy,
    name = "sigFormula",
    arguments = list(
      columns = c(
        "Spread.cross.lower"
      ),
      formula = "(Spread.cross.lower == 1)",
      cross = FALSE
    ),
    label = "Stock.lowerAdj"
  )
  
  
  add.rule(
    qs.strategy,
    name = 'ruleSignal',
    arguments = list(
      sigcol = "Stock.upperAdj",
      sigval = TRUE,
      ordertype = 'market',
      orderside = 'long',
      replace = FALSE,
      prefer = 'Open',
      TxnFees='getTnxFee',
      osFUN = 'osSpreadForTrend',
      ordersidetype = 'upperAdj',
      portfolioName = multi.trend,
	  orderset='ocolong'
    ),
    type = 'enter',
	label='pairEnterUpper'
  )
  
  add.rule(
      qs.strategy, 
      name = 'ruleSignal',
	  arguments=list(
		sigcol='Stock.upperAdj', 
		sigval=TRUE,
		replace=FALSE,
		orderside='long',
		ordertype='stoptrailing', 
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
    arguments = list(
      sigcol = "Stock.lowerAdj",
      sigval = TRUE,
      ordertype = 'market',
      orderside = 'long',
      replace = FALSE,
      prefer = 'Open',
      TxnFees='getTnxFee',
      osFUN = 'osSpreadForTrend',
      ordersidetype = 'lowerAdj',
      portfolioName = multi.trend,
	  orderset='ocolong'
    ),
    type = 'enter',
	label='pairEnterLower'
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

#applyIndicators(qs.strategy, SH600000)
#aaa <- applyStrategy(strategy=qs.strategy, portfolios=multi.trend)
sink("C:/Users/exubixu/Desktop/aa.txt")
applyStrategy.rebalancing(qs.strategy , portfolios=multi.trend,verbose=TRUE)
sink()

updatePortf(multi.trend)
updateAcct(multi.trend)
updateEndEq(multi.trend)	

#getTxns(Portfolio=multi.trend, Symbol="SH600000", "2002-01-07")
#pdf("labpair10_2.pdf",width=40, height=15) 

sink("C:/Users/exubixu/Desktop/SH600684tnx.txt")
getTxns(Portfolio=multi.trend, Symbol='SH600684')
sink()

sink("C:/Users/exubixu/Desktop/SZ000040tnx.txt")
getTxns(Portfolio=multi.trend, Symbol='SZ000040')
sink()

sink("C:/Users/exubixu/Desktop/order.txt")
getOrderBook(multi.trend)
sink()

write.csv(x=mktdata, file="C:/Users/exubixu/Desktop/mktdata.csv", row.names = TRUE)







