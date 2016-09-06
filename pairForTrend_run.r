

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

clustering.folder <- "C:/important/ideas/stock/projects/model1/testResult/testRel/"
clustering.name <- "clusteringResult_bk"
stock.folder <- 'C:/important/ideas/stock/projects/model1/StockDatas/2016-08-09-Later_Rehabilitation_Cleaned/'
symbList = c("SH600298","SH600037","SH601908")#for testing

#generated result example,
#global data
#Stock1     Stock2     lvls direction
#aPair "SH600298" "SZ002123" "4"  "0"      
#aPair "SH600037" "SZ002027" "4"  "0"      
#aPair "SH601908" "NO"       "4"  "0"     
#pairList
#[1,] "2"        NA        
#[2,] "SH600298" "SZ002123"
#[3,] "SH600037" "SZ002027"
#[4,] "SH601908" "NO"
#newSymbList
#"SH600298" "SH600037" "SH601908" "SZ002123" "SZ002027"
pairList <- matchPairs(clustering.folder,clustering.name,symbList)

newSymbList <- unique(c(symbList,as.vector(na.omit(pairList[-1,]))))
#need remove NO
newSymbList <- newSymbList[newSymbList!="NO"]

for(symbol in newSymbList) 
{ 
  a  <-   subsetByDateRange(loadStock(stock.folder, symbol, operation.name="all"),startDate, endDate)
  assign(symbol,a)
  rm(a)
}
#prepare stock data besides market data
stockData <- Cl(get(newSymbList[1]))

for (aStockName in newSymbList[-1]) {
  stockData <- cbind(stockData, Cl(get(aStockName)))
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

setupPairsGlobals(multi.trend, na.omit(pairList),lvls)

### extra test
alvls <- getPairLvls(multi.trend, "SZ002123")
adir <- getPairDirection(multi.trend, "SH600037")
aPair <- getPaired(multi.trend,"SZ002123")
### extra test end

### this long rule only for testing
### Trend rules shall be inserted here
add.indicator(
  strategy = multi.trend,
  name = "get.montlySMA",
  arguments = list(
    mktdata = quote(Cl(mktdata)),
    n=5),
  label = "SMA"
)

add.signal(
  multi.trend,
  name = "sigCrossover",
  arguments = list(
    columns = c("Close", "StockMonthSMA10.SMA"),
    relationship = "gt"
  ),
  label = "StockMCl.gt.SMA"
)

add.signal(
  multi.trend,
  name = "sigCrossover",
  arguments = list(
    columns = c("Close", "StockMonthSMA10.SMA"),
    relationship = "lt"
  ),
  label = "StockMCl.lt.SMA"
) 

add.signal(
  qs.strategy,
  name = "sigFormula",
  arguments = list(
    columns = c(
      "StockMCl.gt.SMA"
    ),
    formula = "(StockMCl.gt.SMA == 1)",
    cross = FALSE
  ),
  label = "Stock.longEnter"
)

add.rule(
  multi.trend,
  name = 'ruleSignal',
  arguments = list(
    sigcol = "Stock.longEnter",
    sigval = TRUE,
    ordertype = 'market',
    orderside = 'long',
    replace = FALSE,
    prefer = 'Open',
    TxnFees="takeTranxFee",
    osFUN = 'osSpreadMaxDollar',
    tradeSize = floor(MaxPos / 2 / lvls),
    maxSize = floor(MaxPos)
  ),
  type = 'enter',
  label='longRule'
)
### this long rule only for testing End

add.rule(multi.trend, 'ruleReblance',
         arguments=list(rebalance_on='days'),
         type='rebalance',
         label='rebalance'
)

setupPairsSignals(multi.trend, stockData)

