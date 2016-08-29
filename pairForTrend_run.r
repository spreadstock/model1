#setup
currency("USD")
Sys.setenv(TZ="UTC")

#clean up
if (!exists('.blotter')) .blotter <- new.env()
if (!exists('.strategy')) .strategy <- new.env() 
suppressWarnings(rm(list = ls(envir = .blotter), envir = .blotter))
suppressWarnings(rm(list = ls(envir = .strategy), envir = .strategy))

initDate <- '2012-01-01'
startDate <- '2012-01-02'
endDate <- '2014-09-30'
initEq <- 1e5

MaxPos <- 35000  #max position in stockA; 
# max position in stock B will be max * ratio, i.e. no hard position limit in 
# Stock B
lvls <- 4 #how many times to fade; Each order's qty will = MaxPos/lvls

stock.folder <- 'C:/important/ideas/stock/projects/model1/StockDatas/2016-08-09-Later_Rehabilitation_Cleaned/'
#symbList = c("SH601169" ,"SH600037","SH601908","SH600298")
symbList = c("SH600298")

pairList <- matchPairs(symbList)

newSymbList <- unique(c(symbList,as.vector(na.omit(pairList[-1,]))))

for(symbol in newSymbList) 
{ 
  a  <-   subsetByDateRange(loadStock(stock.folder, symbol, operation.name="all"),startDate, endDate)
  assign(symbol,a)
  rm(a)
}

multi.trend <- "multitrend"
stock(newSymbList, currency='USD',multiplier=1)
rm.strat(multi.trend)
portfolio <- initPortf(multi.trend, newSymbList, initDate = initDate)
initAcct(
  multi.trend,
  portfolios = multi.trend,
  initDate = initDate,
  initEq = initEq
)
initOrders(portfolio = multi.trend, initDate = initDate)

strategy(multi.trend, store =TRUE)

setupPairsGlobals(multi.trend, na.omit(pairList),2)

lvls <- getPairLvls(multi.trend, "SZ002123")

# setPairPosition(multi.trend, "SH600037", 1000)
# pos <- getPairPosition(multi.trend, "SH600037")

stockData <- Cl(get(newSymbList[1]))

for (aStockName in newSymbList[-1]) {
  stockData <- cbind(stockData, Cl(get(aStockName)))
}

add.indicator(
  strategy = multi.trend,
  name = "initlongTime",
  arguments = list(
    mktdata = quote(Cl(mktdata))
  ),
  label = "LONGTIME"
)

add.signal(
  multi.trend,
  name = "sigComparison",
  arguments = list(columns = c("LongTime.LONGTIME", "LongCondition.LONGTIME"), relationship = "eq"),
  label = "Stock_LongTime"
)

add.signal(
  multi.trend,
  name = "sigComparison",
  arguments = list(columns = c("LongStart.LONGTIME", "LongCondition.LONGTIME"), relationship = "eq"),
  label = "Stock_LongStartTime"
)

add.rule(
  multi.trend,
  name = 'ruleSignal',
  arguments = list(
    sigcol = "Stock_LongTime",
    sigval = TRUE,
    ordertype = 'market',
    orderside = 'long',
    replace = FALSE,
    prefer = 'Open',
    TxnFees="takeTranxFee",
    osFUN = 'osSpreadForTrend',
    ordersidetype = 'initLong',
    portfolioName = multi.trend
  ),
  type = 'enter'
)

setupPairsSignals(multi.trend, stockData)

