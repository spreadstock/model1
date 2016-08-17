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

MaxPos <- 1000  #max position in stockA; 
# max position in stock B will be max * ratio, i.e. no hard position limit in 
# Stock B
lvls <- 4 #how many times to fade; Each order's qty will = MaxPos/lvls

stock.folder.daily <- 'C:/important/ideas/stock/projects/model1/StockDatas/2016-08-09-Later_Rehabilitation_Cleaned/'
symbList = c("SH601169" ,"SH601328")

SH601169 <- subsetByDateRange(loadStock(stock.folder.daily, stock.name= symbList[1], operation.name="All"),startDate, endDate) 
SH601328 <- subsetByDateRange(loadStock(stock.folder.daily, stock.name= symbList[2], operation.name="All"),startDate, endDate) 
stock_daily <- SH601169
stock_daily <-cbind(stock_daily, SH601328)


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
pair <- c(1,2 , MaxPos, lvls)
names(pair) <- c(symbList[1], symbList[2], "MaxPos", "lvls")
.blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair <- pair

# Create initial position limits and levels by symbol
# allow 3 entries for long and short if lvls=3.
addPosLimit(portfolio=qs.strategy, timestamp=initDate, symbol=symbList[1], 
            maxpos=MaxPos, longlevels=lvls, minpos=-MaxPos, shortlevels=lvls)
addPosLimit(portfolio=qs.strategy, timestamp=initDate, symbol=symbList[2], 
            maxpos=MaxPos * 2, longlevels=lvls, minpos=-MaxPos, shortlevels=lvls)

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
  name = "rollingV2_1",
  arguments = list(
    x = quote(Cl(stock_daily)),
    width=10,
    FUN=calculate_spread,
    PREFUN=calcuateAbsPrice),
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
  arguments = list(columns = c("Spread.SPREAD", "Upper.SPREAD"), relationship = "gt"),
  label = "Spread.cross.upper"
)

add.signal(
  qs.strategy,
  name = "sigCrossover",
  arguments = list(columns = c("Spread.SPREAD", "Lower.SPREAD"), relationship = "lt"),
  label = "Spread.cross.lower"
)




# add.signal(
#   qs.strategy,
#   name = "sigCrossover",
#   arguments = list(columns = c("Spread.SPREAD", "Mean.SPREAD"), relationship = "lt"),
#   label = "Spread.lt.middle"
# )
# 
# add.signal(
#   qs.strategy,
#   name = "sigCrossover",
#   arguments = list(columns = c("Spread.SPREAD", "Mean.SPREAD"), relationship = "gt"),
#   label = "Spread.gt.middle"
# )

#combine Signals
# add.signal(
#   qs.strategy,
#   name = "sigFormula",
#   arguments = list(
#     columns = c(
#       "StockMCl.gt.SMA"
#     ),
#     formula = "(StockMCl.gt.SMA == 1)",
#     cross = FALSE
#   ),
#   label = "Stock.longEntry"
# )

# 
# add.signal(
#   qs.strategy,
#   name = "sigFormula",
#   arguments = list(
#     columns = c(
#       "StockCl.gt.SMA",
#       "Spread.cross.lower"
#       
#     ),
#     formula = "(StockCl.gt.SMA == 1) & (Spread.cross.lower == 1)",
#     cross = FALSE
#   ),
#   label = "Stock.lowerAdjSell"
# )

# add.signal(
#   qs.strategy,
#   name = "sigFormula",
#   arguments = list(
#     columns = c(
#       "StockMCl.lt.SMA",
#       "Spread.lt.middle",
#       "Spread.gt.middle"
#     ),
#     formula = "((StockMCl.lt.SMA == 1) & ((Spread.lt.middle == 1) | (Spread.gt.middle  == 1)))",
#     cross = FALSE
#   ),
#   label = "Stock.longExit"
# )



# #add rules
add.rule(
  qs.strategy,
  name = 'ruleSignal',
  arguments = list(
    sigcol = "StockMCl.gt.SMA",
    sigval = TRUE,
    orderqty = 2000,
    ordertype = 'market',
    orderside = NULL,
    osFUN = 'osMaxPos'
  ),
  type = 'enter'
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



#adjustment by spread

# add.rule(
#   qs.strategy,
#   name = 'ruleSignal',
#   arguments = list(
#     sigcol = "Stock.lowerAdjSell",
#     sigval = TRUE,
#     orderqty = 200,
#     ordertype = 'market',
#     orderside = 'lowerAdjSell',
#     osFUN = 'osSpreadMaxPos'
#   ),
#   type = 'enter'
# )

add.rule(
  qs.strategy,
  name = 'ruleSignal',
  arguments = list(
    sigcol = "Spread.cross.upper",
    sigval = TRUE,
    orderqty = 200,
    ordertype = 'market',
    orderside = 'upperAdj',
    osFUN = 'osSpreadMaxPos'
  ),
  type = 'enter'
)

add.rule(
  qs.strategy,
  name = 'ruleSignal',
  arguments = list(
    sigcol = "Spread.cross.lower",
    sigval = TRUE,
    orderqty = 200,
    ordertype = 'market',
    orderside = 'lowerAdj',
    osFUN = 'osSpreadMaxPos'
  ),
  type = 'enter'
)

  
# 
# ################################################################################

