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
        aPair <- first(tmp[tmp$Cointegration == min(tmp$Cointegration),c(1,2)])
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
  pairs <- matrix(ncol=4)
  
  for (aItem in 1:nrow(x)) {
    aPair <- c(x[aItem,1],x[aItem,2],lvls,0)
    pairs <- rbind(pairs,aPair)
  }
  colnames(pairs) <- c("Stock1", "Stock2", "lvls","direction")
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

calculate_totalbeta <- function(x, threshold) {
  lastPoint <- 1
  xx <- na.fill(x,0)
  num <- nrow(xx)
  checkPoint1 <- threshold
  checkPoint2 <- -threshold
  for (aItem in 2:num) {
    aValue <- coredata(xx[aItem,1]) - coredata(xx[lastPoint,1])
    xx[aItem,2] <- aValue
    if ((aValue > checkPoint1) | (aValue < checkPoint2) ) {
      lastPoint <- aItem
    }
  }
  return (xx)
}


calculate_betaforTrend <- function(x, currentStockName) {
  
  aStock <- word(currentStockName,sep=fixed("."))
  pairedStock <- getPaired(multi.trend, aStock)
  if (is.null(pairedStock)) {
    return
  } else {
    stockData <- x[,paste(pairedStock["Stock1"], 'Close', sep=".")]
    stockData <- cbind(stockData,x[,paste(pairedStock["Stock2"], 'Close', sep=".")])
  }
  
  dx <- na.omit(stockData)
  beta<-round(dx[,2] / dx[,1],5)
  #beta <- lag(beta,1) #no need lag, because the platform already delay 1 day
  beta_total <- calculate_totalbeta(cbind(beta, 0), 0.075)
  beta <- cbind(beta_total, 0.075, -0.075)
  
  colnames(beta) <- c("Beta","BetaTotal","Upper", "Lower")
  return (beta) 
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
      portfolioName = portfolio
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
      portfolioName = portfolio
    ),
    type = 'enter'
  )
}

osSpreadForTrend <- function (data, timestamp, ordertype, orderside, 
                              portfolio, symbol, ruletype, ..., orderprice, ordersidetype) {
  portfolioName <- portfolio
  portf <- getPortfolio(portfolio)
  
  lvls <- getPairLvls(portfolioName, symbol)
  thePair <- as.vector(getPaired(portfolioName,symbol))
  if (is.null(thePair))
    return (0)
  
  currentQtyA <- getPosQty(portfolio, thePair[1], timestamp)
  currentQtyB <- getPosQty(portfolio, thePair[2], timestamp)
  
  if ((currentQtyA == 0 ) | (currentQtyB == 0)) {
    return (0)
  }
  
  transDirection <- getPairDirection(portfolioName, symbol)
  
  
  pairLocation <- which(thePair==symbol)

  
  beta <-  mktdata[,"Beta.SPREAD"]
  ratio <- as.numeric(coredata(beta[timestamp]))
  #print(ratio)
  
  if (ordersidetype == "upperAdj") {
    if (transDirection == 1) {
      # reject transaction for dup direction
      qty <- 0
    } else {
      qtyB <- floor(currentQtyB / lvls)
      qtyA <- floor(qtyB * ratio)
      if (pairLocation == 1) {
        qty <- qtyA
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
      qtyA <- floor(currentQtyA / lvls)
      qtyB <- floor(qtyA / ratio)
      if (pairLocation == 1) {
        qty <- -qtyA
      } else {
        qty <- qtyB
        if (qty != 0)
          setPairDirection(portfolioName,symbol,-1)
      }
      }
      
  } else {
    qty <- 0
  }
  

  orderqty <- qty
  return(orderqty) 
}

osSpreadMaxDollar <- function(data, timestamp, orderqty, ordertype, orderside,
                              portfolio, symbol, prefer="Open", tradeSize,
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
      priceOther <- price * ratio
    } else {
      priceOther <- price / ratio
    }
    
    totalPosValue <- posStock1 * price + posStock2 * priceOther
    remainValue <- (maxSize - totalPosValue) / 2
    
    #posVal <- pos*price
    if (orderside=="short") {
      dollarsToTransact <- max(tradeSize, maxSize-posVal)
      #If our position is profitable, we don't want to cover needlessly.
      if(dollarsToTransact > 0) {dollarsToTransact=0}
    } else {
      dollarsToTransact <- min(tradeSize, remainValue)
      #If our position is profitable, we don't want to sell needlessly.
      if(dollarsToTransact < 0) {dollarsToTransact=0}
    }
    qty <- dollarsToTransact/price
    if(integerQty) {
      qty <- trunc(qty)
    }
    return(qty)    
  }
  
}

ruleReblance <- function (..., portfolio, symbol, timestamp) 
{
  return (1)
}