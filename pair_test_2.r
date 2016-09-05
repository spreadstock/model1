#setup
currency("USD")
Sys.setenv(TZ="UTC")

#clean up
if (!exists('.blotter')) .blotter <- new.env()
if (!exists('.strategy')) .strategy <- new.env() 
suppressWarnings(rm(list = ls(envir = .blotter), envir = .blotter))
suppressWarnings(rm(list = ls(envir = .strategy), envir = .strategy))

initDate <- '2012-01-01'
startDate <- '2013-01-01'
endDate <- '2014-12-31'
initEq <- 1e5

MaxPos <- 35000  #max position in stockA; 
# max position in stock B will be max * ratio, i.e. no hard position limit in 
# Stock B
lvls <- 3 #how many times to fade; Each order's qty will = MaxPos/lvls

stock.folder.daily <- 'C:/important/ideas/stock/projects/model1/StockDatas/2016-08-09-Former_Rehabilitation_leaned/'
#symbList = c("SH601169" ,"SH601328")
symbList = c("SH600353" ,"SZ002123")

for(symbol in symbList) 
{ 
  a  <-   subsetByDateRange(loadStock(stock.folder.daily, symbol, operation.name="all"),startDate, endDate)
  assign(symbol,a)
  rm(a)
}

stock_daily <- get(symbList[1])
stock_daily <-cbind(stock_daily, get(symbList[2]))



for(symbol in symbList)
{
  stock(symbol, currency = "USD", multiplier = 1)
}



qs.strategy <- "qsModel1"
initPortf(qs.strategy, symbols=symbList, initDate = initDate)
initAcct(
  qs.strategy,
  portfolios = qs.strategy,
  initDate = initDate,
  currency = "USD",
  initEq = initEq
)

initOrders(portfolio = qs.strategy, initDate = initDate)


# osFUN will need to know which symbol is leg 1 and which is leg 2 as well as 
# what the values are for MaxPos and lvls.  So, create a slot in portfolio to 
# hold this info.
pair <- c(1,2 , MaxPos, lvls,0,0,0)
names(pair) <- c(symbList[1], symbList[2], "MaxPos", "lvls","transA","transB","transBInit")
.blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair <- pair

# Create initial position limits and levels by symbol
# allow 3 entries for long and short if lvls=3.
# addPosLimit(portfolio=qs.strategy, timestamp=initDate, symbol=symbList[1], 
#             maxpos=MaxPos, longlevels=lvls, minpos=-0, shortlevels=lvls)
# addPosLimit(portfolio=qs.strategy, timestamp=initDate, symbol=symbList[2], 
#             maxpos=MaxPos * 2, longlevels=lvls, minpos=0, shortlevels=lvls)

strategy(qs.strategy, store = TRUE)

#Signal set
#build singal set 
#SH601169.Open SH601169.High SH601169.Low SH601169.Close SH601169.Volume 
#SH601169.Adjusted SH601328.Open SH601328.High SH601328.Low SH601328.Close
#SH601328.Volume SH601328.Adjusted SMA.SMA30D_1 SMA.SMA30D_2 StockMonth.1.SMA
#StockMonthSMA10.1.SMA StockMonth.2.SMA StockMonthSMA10.2.SMA Spread.SPREAD
#Beta.SPREAD Upper.SPREAD Lower.SPREAD Mean.SPREAD
add.indicator(
  strategy = qs.strategy,
  name = "SMA",
  arguments = list(
    x = quote(Cl(mktdata)),
    n=15),
  label = "SMA30D"
)



add.indicator(
  strategy = qs.strategy,
  name = "get.montlySMA",
  arguments = list(
    mktdata = quote(Cl(mktdata)),
    n=5),
  label = "SMA"
)

add.indicator(
  strategy = qs.strategy,
  name = "calculate_beta",
  arguments = list(
    x = quote(Cl(stock_daily))
  ),
  label = "SPREAD"
)


add.signal(
  qs.strategy,
  name = "sigCrossover",
  arguments = list(
    columns = c("Close", "StockMonthSMA10.SMA"),
    relationship = "gt"
  ),
  label = "StockMCl.gt.SMA"
)

add.signal(
  qs.strategy,
  name = "sigCrossover",
  arguments = list(
    columns = c("Close", "StockMonthSMA10.SMA"),
    relationship = "lt"
  ),
  label = "StockMCl.lt.SMA"
)  




add.signal(
  qs.strategy,
  name = "sigCrossover",
  arguments = list(columns = c("Close", "SMA.SMA30D"), relationship = "gt"),
  label = "StockCl.gt.SMA"
)

add.signal(
  qs.strategy,
  name = "sigCrossover",
  arguments = list(columns = c("Close", "SMA.SMA30D"), relationship = "lt"),
  label = "StockCl.lt.SMA"
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
      "StockCl.gt.SMA"
    ),
    formula = "(StockCl.gt.SMA == 1)",
    cross = FALSE
  ),
  label = "Stock.longEnter"
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


add.signal(
  qs.strategy,
  name = "sigFormula",
  arguments = list(
    columns = c(
      "StockMCl.lt.SMA"
    ),
    formula = "(StockMCl.lt.SMA == 1)",
    cross = FALSE
  ),
  label = "Stock.longExit"
)


# #add rules
add.rule(
  qs.strategy,
  name = 'ruleSignal',
  arguments = list(
    sigcol = "Stock.longEnter",
    sigval = TRUE,
    ordertype = 'market',
    orderside = 'long',
    replace = FALSE,
    prefer = 'Open',
    TxnFees="takeTranxFee",
    orderset="pairForTrend",
    osFUN = 'osSpreadMaxDollar',
    tradeSize = floor(MaxPos / 2 / lvls),
    maxSize = floor(MaxPos)
  ),
  type = 'enter',
  label='longRule'
)

# add.rule(
#   qs.strategy,
#   name = 'ruleSignal',
#   arguments = list(
#     sigcol = "Stock.longExit",
#     sigval = TRUE,
#     orderqty = 'all',
#     ordertype = 'market',
#     orderside = NULL
#   ),
#   type = 'exit'
# )



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
    TxnFees="takeTranxFee",
    orderset="pairForTrend",
    osFUN = 'osSpreadSize',
    ordersidetype = 'upperAdj'
  ),
  type = 'enter',
  label='UpperAdjRule'
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
    TxnFees="takeTranxFee",
    orderset="pairForTrend",
    osFUN = 'osSpreadSize',
    ordersidetype = 'lowerAdj'
  ),
  type = 'enter',
  label='LowerAdjRule'
)

add.rule(qs.strategy, 'ruleReblance',
         arguments=list(rebalance_on='days'),
         type='rebalance',
         label='rebalance'
)

stopLossPercent <- 0.1
add.rule(
  qs.strategy,
  name='ruleSignal',
  arguments = list(sigcol="Stock.longEnter", sigval=TRUE,
                   replace=FALSE,
                   orderside='long',
                   ordertype='stoptrailing',
                   tmult=TRUE,
                   threshold=quote(stopLossPercent),
                   orderqty='all',
                   prefer = 'Open',
                   TxnFees="takeTranxFee",
                   orderset='pairForTrend'),
  type='chain', parent="longRule",
  label='StopLossLong',
  enabled=FALSE
)


add.rule(
  qs.strategy,
  name='ruleSignal',
  arguments = list(sigcol="Stock.lowerAdj", sigval=TRUE,
  replace=FALSE,
  orderside='long',
  ordertype='stoptrailing',
  tmult=TRUE,
  threshold=quote(stopLossPercent),
  orderqty='all',
  prefer = 'Open',
  TxnFees="takeTranxFee",
  orderset='pairForTrend'),
type='chain', parent="LowerAdjRule",
label='StopLossLower',
enabled=FALSE
)

add.rule(
  qs.strategy,
  name='ruleSignal',
  arguments = list(sigcol="Stock.upperAdj", sigval=TRUE,
                   replace=FALSE,
                   orderside='long',
                   ordertype='stoptrailing',
                   tmult=TRUE,
                   threshold=quote(stopLossPercent),
                   orderqty='all',
                   prefer = 'Open',
                   TxnFees="takeTranxFee",
                   orderset='pairForTrend'),
  type='chain', parent="UpperAdjRule",
  label='StopLossUpper',
  enabled=FALSE
)
  
# 
# ################################################################################

