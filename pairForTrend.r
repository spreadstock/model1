################################################################################
# custom indicator function for monthly SMA                                    #
################################################################################
get.montlySMA <- function(mktdata, n) {
  
  monthData <- mktdata
  indicatorM <- to.monthly(monthData, indexAt='endof', drop.time=TRUE)
  indicator <- SMA (Cl(indicatorM),n)
  indicator <- cbind(Cl(indicatorM), indicator)
  colnames(indicator) <- c("StockMonth", "StockMonthSMA10")
  
  
  numCol <- ncol(mktdata)
  numColNew <- ncol(indicator)
  indicator <- cbind(mktdata,indicator)
  
  aName <- "StockMonth"
  indicator[, aName] <-
    na.locf(indicator[, aName], na.rm = FALSE, fromLast = FALSE)
  aName <- "StockMonthSMA10"
  indicator[, aName] <-
    na.locf(indicator[, aName], na.rm = FALSE, fromLast = FALSE)
  
  
  return(indicator[,(numCol+1):(numCol+numColNew)])
}

#match pair and setup pair data
#folderName
#fileName, clustering result file name
#x, master stock symbols
#return, updated stock symbols including paired stocks
matchPairs <- function(folderName, fileName, x)
{
  #stock.output <- 'C:/important/ideas/stock/projects/model1/testResult/testRel/'
  #outputFile <- "clusteringResult_bk"
  finalResult_1st <- read.csv(paste(folderName, fileName, '.csv', sep=''), sep=',', check.names=FALSE)
  finalResult_1st <- finalResult_1st[,-1]
  #only cointegration <= 0.1 considered
  finalResult_1st <- finalResult_1st[finalResult_1st$Cointegration<=0.1,]
  pairResult <- matrix(NA,nrow=(length(x)+1), ncol= 2);
  numPairs <- 0
  for (aStock in x) {
    if (aStock %in% pairResult[,1] | aStock %in% pairResult[,2]) {
      
    } else {
      tmp <- finalResult_1st[(finalResult_1st$Stock1 == aStock
                              | finalResult_1st$Stock2 == aStock),]
      if (nrow(tmp) > 0) {
        aPair <- xts::first(tmp[tmp$Cointegration == min(tmp$Cointegration),c(1,2)])
        if (aPair[1,1] %in% pairResult[,1] | aPair[1,1] %in% pairResult[,2]
            | aPair[1,2] %in% pairResult[,1] | aPair[1,2] %in% pairResult[,2]) {
        } else {
          pairResult[(numPairs+2),1] <- as.character(aPair$Stock1)
          pairResult[(numPairs+2),2] <- as.character(aPair$Stock2)
          numPairs <- numPairs + 1
          
        }
      }      
    }

    
  }
  #add not paired
  i <- numPairs + 2
  for (aStock in x) {
    if (aStock %in% pairResult[,1] | aStock %in% pairResult[,2]) {
    } else {
      pairResult[i,1] <- as.character(aStock)
      pairResult[i,2] <- "NO"
      i <- i + 1
    }
  }
  pairResult[1,1] <- numPairs
  
  return(pairResult)
  
}


getPaired <- function(portfolio, x)
{
  pairedStock <- NULL
  pairList <- .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs
  if (x %in% pairList[,1]) {
    theOther <- pairList[pairList[,1] == x,2]
    if (theOther != "NO")
      pairedStock <- pairList[pairList[,1] == x,]
  } else  if (x %in% pairList[,2]) {
    pairedStock <- pairList[pairList[,2] == x,]
  } 

  return(pairedStock)
  
}

#setup pair execution env
#protfolio
#x, pairList, contains paired symbols
#lvls, percentage of position for each pair transaction
#return, N/A
setupPairsGlobals <- function(portfolio, x, lvls=1)
{
  pairs <- matrix(ncol=6)
  
  for (aItem in 1:nrow(x)) {
    aPair <- c(x[aItem,1],x[aItem,2],lvls,0,"1016-01-01",0)
    pairs <- rbind(pairs,aPair)
  }
  colnames(pairs) <- c("Stock1", "Stock2", "lvls","direction","Timestamp","Stock2Qty")
  pairs<- pairs[-1,,drop=FALSE]
  .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs <- pairs
}


#set pair direction of pair stocks
#protfolio
#x, a stock symbol
#direction
#return, N/A
setPairDirection <- function(portfolio, x, direction)
{
  
  pairs <- .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs
  
  if (x %in% pairs[,1]) {
    .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs[x == pairs[,1],"direction"] <- direction
  } else if (x %in% pairs[,2]) {
    .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs[x == pairs[,2],"direction"] <- direction
  } else {
    print(paste("Warning! stock is no pair ", x))
  }
}

#get pair direction from pair stocks
#protfolio
#x, a stock symbol
#return, position
getPairDirection <- function(portfolio, x)
{
  pairs <- .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs
  
  qty <- 0
  
  if (x %in% pairs[,1]) {
    qty <- .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs[x == pairs[,1],"direction"]
  } else if (x %in% pairs[,2]) {
    qty <- .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs[x == pairs[,2],"direction"]
  } else {
    print(paste("Warning! stock is no pair ", x))
  }
  return (as.numeric(qty))
}

#set pair qty of pair stocks
#protfolio
#x, a stock symbol
#qty
#return, N/A
setPairQty <- function(portfolio, x, timestamp, qty)
{
  
  pairs <- .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs
  
  if (x %in% pairs[,1]) {

  } else if (x %in% pairs[,2]) {
    .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs[x == pairs[,2],"Stock2Qty"] <- qty
    .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs[x == pairs[,2],"Timestamp"] <- as.character(timestamp)
  } else {
    print(paste("Warning! stock is no pair ", x))
  }
}

#get pair qty from pair stocks
#protfolio
#x, a stock symbol
#return, qty
getPairQty <- function(portfolio, x)
{
  pairs <- .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs
  
  qty <- 0
  
  if (x %in% pairs[,1]) {
    return(c("2016-01-01","0"))
  } else if (x %in% pairs[,2]) {
    qty <- as.character(.blotter[[paste('portfolio', portfolio, sep='.')]]$pairs[x == pairs[,2],"Stock2Qty"])
    timestamp <- as.character(.blotter[[paste('portfolio', portfolio, sep='.')]]$pairs[x == pairs[,2],"Timestamp"])
  } else {
    print(paste("Warning! stock is no pair ", x))
  }
  return (cbind(timestamp,qty))
}


#get lvls from pair stocks
#protfolio
#x, a stock symbol
#return, Lvl
getPairLvls <- function(portfolio, x)
{
  pairs <- .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs
  
  lvls <- 0
  
  if (x %in% pairs[,1]) {
    lvls <- pairs[(x == pairs[,1]),"lvls"]
  } else if (x %in% pairs[,2]) {
    lvls <- pairs[(x == pairs[,2]),"lvls"]
  } else {
    #print(paste("Warning! stock is no pair ", x))
  }
  return (as.numeric(lvls))
}




calculate_betaforTrend <- function(x, currentStockName) {
  aStock <- word(currentStockName,sep=fixed("."))
  pairedStock <- getPaired(multi.trend, aStock)
  if (is.null(pairedStock)) {
    returnBeta <- rep(0,nrow(x))
    returnBeta <- cbind(returnBeta, 0,1,-1)
  } else {
    stockData <- x[,paste(pairedStock["Stock1"], 'Close', sep=".")]
    stockData <- cbind(stockData,x[,paste(pairedStock["Stock2"], 'Close', sep=".")])
    #V1
    #estimatedBeta <- calculate_spreadV1(stockData, 0.075)
    #returnBeta <- estimatedBeta[,c(1,2,3,4,5)]
    #V2
    estimatedBeta <- calculate_spreadV2(stockData, 0.5)
    returnBeta <- estimatedBeta[,c(1,2,3,4,5)]
    #V3
    #estimatedBeta <- calculate_spreadV3(stockData, 1.2)
    #returnBeta <- estimatedBeta[,c(1,3,4,5,6)]
    #V5
    #estimatedBeta <- calculate_spreadV5(stockData, 1)
    #returnBeta <- estimatedBeta[,c(1,2,4,5,6)]
    #V5.1
    #estimatedBeta <- calculate_spreadV5_1(stockData, 0.5, 1)
    #returnBeta <- estimatedBeta[,c(1,4,5,6,7)]
    
  }
  
  colnames(returnBeta) <- c("Beta","Beta0","BetaTotal","Upper", "Lower")
  return (returnBeta) 
}

#setup pair execution env, including indicator/signals etc
#portfolio
#stockData, the stock data for stockX, stocky.
#return, N/A
setupPairsSignals <- function(portfolio,stockData)
{
  add.indicator(
    strategy = portfolio,
    name = "calculate_betaforTrend",
    arguments = list(
      x = quote(Cl(stockData)),
      currentStockName = quote(colnames(Cl(mktdata)))
    ),
    label = "SPREAD"
  )
  
  add.signal(
    portfolio,
    name = "sigCrossover",
    arguments = list(columns = c("BetaTotal.SPREAD", "Upper.SPREAD"), relationship = "gt"),
    label = "Spread.cross.upper"
  )
  
  
  add.signal(
    portfolio,
    name = "sigCrossover",
    arguments = list(columns = c("BetaTotal.SPREAD", "Lower.SPREAD"), relationship = "lt"),
    label = "Spread.cross.lower"
  )
  
  add.signal(
    portfolio,
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
    portfolio,
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
    portfolio,
    name = 'ruleSignal',
    arguments = list(
      sigcol = "Stock.upperAdj",
      sigval = TRUE,
      ordertype = 'market',
      orderside = 'long',
      replace = FALSE,
      prefer = 'Open',
      TxnFees="takeTranxFee",
      osFUN = 'osSpreadForTrend',
      ordersidetype = 'upperAdj',
      portfolioName = portfolio,
      marketTime = quote(index(stockData))
    ),
    type = 'enter'
  )
  
  add.rule(
    portfolio,
    name = 'ruleSignal',
    arguments = list(
      sigcol = "Stock.lowerAdj",
      sigval = TRUE,
      ordertype = 'market',
      orderside = 'long',
      replace = FALSE,
      prefer = 'Open',
      TxnFees="takeTranxFee",
      osFUN = 'osSpreadForTrend',
      ordersidetype = 'lowerAdj',
      portfolioName = portfolio,
      marketTime = quote(index(stockData))
    ),
    type = 'enter'
  )
}

osSpreadForTrend <- function (data, timestamp, ordertype, orderside, 
                              portfolio, symbol, ruletype, ..., orderprice, ordersidetype, marketTime) {
  portfolioName <- portfolio
  portf <- getPortfolio(portfolio)
  
  lvls <- getPairLvls(portfolioName, symbol)
  thePair <- as.vector(getPaired(portfolioName,symbol))
  if (is.null(thePair))
    return (0)
  
  pairLocation <- which(thePair==symbol)

  currentQtyA <- getPosQty(portfolio, thePair[1], timestamp)
  currentQtyB <- getPosQty(portfolio, thePair[2], timestamp)
  if (pairLocation == 1) {
    timedQty <- getPairQty(portfolio,thePair[2])
    if (timedQty[1] != "1016-01-01") {
      nextTimestampIndex <- which(marketTime==timedQty[1])
      nextTimestamp <- marketTime[nextTimestampIndex+1]
      if (nextTimestamp == timestamp)
        currentQtyB <- currentQtyB + as.numeric(timedQty[2])
    
    }
  }
    
 if ((currentQtyA == 0 ) | (currentQtyB == 0)) {
    return (0)
  }
  
  transDirection <- getPairDirection(portfolioName, symbol)
  
  


  
  beta <-  mktdata[,"Beta0.SPREAD"]
  ratio <- as.numeric(coredata(beta[timestamp]))
  #print(ratio)
  
  if (ordersidetype == "upperAdj") {
    if (transDirection == 1) {
      # reject transaction for dup direction
      qty <- 0
    } else {
      #sell spread
      # amount decided by the stock 2
      qtyB <- floor(currentQtyB / lvls)
      if (pairLocation == 1) {
        qty <- floor(qtyB * ratio)
      } else {
        qty <- -qtyB
        if (qty != 0)
          setPairDirection(portfolioName,symbol,1)
      }      
    }
  
  } else if (ordersidetype == "lowerAdj") {  
    if (transDirection == -1) {
      # reject transaction for dup direction
      qty <- 0
    } else {
      #buy spread
      # amount decided by the stock 1
      qtyA <- floor(currentQtyA / lvls)
      if (pairLocation == 1) {
        qty <- -qtyA
        
      } else {
        qtyB <- floor(qtyA / ratio)
        qty <- qtyB
        if (qty != 0)
          setPairDirection(portfolioName,symbol,-1)
      }      
    }

  } else {
    qty <- 0
  }
  
  
  setPairQty(portfolioName,symbol,timestamp,qty)
  orderqty <- qty
  return(orderqty) 
}

osSpreadMaxDollar <- function(data, timestamp, orderqty, ordertype, orderside,
                              portfolio, symbol, prefer="Open",
                              maxSize, integerQty=TRUE,
                              ...) {
  portfolioName <- portfolio
  portf <- getPortfolio(portfolio)
  thePair <- as.vector(getPaired(portfolioName,symbol))
  if (is.null(thePair))
    return (0)
  
  pairLocation <- which(thePair==symbol)
  beta <-  mktdata[,"Beta.SPREAD"]
  ratio <- as.numeric(coredata(beta[timestamp]))
  if (ratio == 0 ) {
    #to do
    return (0)
  } else {
    posStock1 <- getPosQty(portfolio, thePair[1], timestamp)
    posStock2 <- getPosQty(portfolio, thePair[2], timestamp)
    
    if(prefer=="Close") {
      price <- as.numeric(Cl(data[timestamp,]))
    } else {
      price <- as.numeric(Op(data[timestamp,]))
    }
    if (pairLocation == 1) {
      if (posStock1 > 0)
        return (0)
      priceOther <- price * ratio
      if (posStock1 == 0 & posStock2 == 0) {
        remainValue <- maxSize / 2
      } else
        remainValue <- maxSize - posStock2 * priceOther
    } else if (pairLocation == 2)  {
      if (posStock2 > 0)
        return (0)
      priceOther <- price / ratio
      if (posStock1 == 0 & posStock2 == 0) {
        remainValue <- maxSize / 2
      } else
        remainValue <- maxSize - posStock1 * priceOther
    }
    
    #posVal <- pos*price
    dollarsToTransact <- remainValue
    #If our position is profitable, we don't want to sell needlessly.
    if(dollarsToTransact < 0) {dollarsToTransact=0}

    qty <- dollarsToTransact/price
    if(integerQty) {
      qty <- trunc(qty)
    }
    if (qty > 0)
      setPairQty(portfolioName,symbol,timestamp,qty)
    return(qty)     
  }
  
}

reblanceImp <- function (strategy, portfolios, mktdata = NULL, parameters = NULL, 
                         ..., verbose = TRUE, symbols = NULL, initStrat = FALSE, updateStrat = FALSE) 
{
  
  ret <- list()
  if (!is.strategy(strategy)) {
    s <- try(getStrategy(strategy))
    if (inherits(s, "try-error")) 
      stop("You must supply an object of type 'strategy'.")
  }
  else {
    s <- strategy
  }
  
  if (missing(mktdata)) 
    load.mktdata = TRUE
  else load.mktdata = FALSE
  
  for (portfolio in portfolios) {
    if (isTRUE(initStrat)) 
      initStrategy(strategy = s, portfolio, symbols, ... = ...)
    ret[[portfolio]] <- list()
    pobj <- getPortfolio(portfolio)
    symbols <- ls(pobj$symbols)
    st <- new.env()
    plist <- list()
    for (symbol in symbols) {
      sret <- list()
      if (isTRUE(load.mktdata)) 
        mktdata <- get(symbol)
      
      sret$indicators <- applyIndicators(strategy = s, 
                                         mktdata = mktdata, parameters = parameters, ...)
      if (inherits(sret$indicators, "xts") & nrow(mktdata) == 
          nrow(sret$indicators)) {
        mktdata <- sret$indicators
      }
      sret$signals <- applySignals(strategy = s, mktdata = mktdata, 
                                   sret$indicators, parameters = parameters, ...)
      if (inherits(sret$signals, "xts") & nrow(mktdata) == 
          nrow(sret$signals)) {
        mktdata <- sret$signals
      }
      assign(symbol, mktdata, pos = st)
      sret$rules <- list()
      ret[[portfolio]][[symbol]] <- sret
    }
    
    pindex <- as.POSIXct(index(mktdata))
    for (i in 2:length(pindex)) {
      for (symbol in symbols) {
        mktdata <- get(symbol, pos = st)
        md_subset <- mktdata[pindex[i -1], ]
        sret$rules$pathdep <- rbind(sret$rules$pathdep, 
                                    applyRules(portfolio = portfolio, symbol = symbol, 
                                               strategy = s, mktdata = md_subset, Dates = NULL, 
                                               indicators = sret$indicators, signals = sret$signals, 
                                               parameters = parameters, ..., path.dep = TRUE))
        # ruleProc(s$rules$rebalance, timestamp = pindex[i], 
        #          path.dep = TRUE, ruletype = "rebalance", 
        #          ..., mktdata = md_subset, parameters = parameters, 
        #          portfolio = portfolio, symbol = symbol)
        #print(paste("reblance",symbol,index(md_subset)))
      }
    }
    if (isTRUE(updateStrat)) 
      updateStrategy(s, portfolio, Symbols = symbols, ... = ...)
    
  }
  if (verbose) 
    return(ret)
}