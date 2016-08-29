#match pair and setup pair data
#x, master stock symbols
#return, updated stock symbols including paired stocks
matchPairs <- function(x)
{
  stock.output <- 'C:/important/ideas/stock/projects/model1/testResult/testRel/'
  outputFile <- "clusteringResult_bk"
  finalResult_1st <- read.csv(paste(stock.output, outputFile, '.csv', sep=''), sep=',', check.names=FALSE)
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
  pairResult[1,1] <- numPairs
  
  return(pairResult)
  
}


getPaired <- function(portfolio, x)
{
  pairedStock <- NULL
  pairList <- .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs
  if (x %in% pairList[,1]) {
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
  pairs <- matrix(ncol=8)
  
  for (aItem in 1:nrow(x)) {
    aPair <- c(x[aItem,1],x[aItem,2],lvls,0,0,0,0,0)
    pairs <- rbind(pairs,aPair)
  }
  colnames(pairs) <- c("Stock1", "Stock2", "lvls","transA","transB", "transAInit", "transBInit","direction")
  pairs<- pairs[-1,,drop=FALSE]
  .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs <- pairs
}


#set position of pair stocks
#protfolio
#x, a stock symbol
#position
#lvls
#return, N/A
setPairPosition <- function(portfolio, x, position)
{

  pairs <- .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs
  
  if (x %in% pairs[,1]) {
    .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs[x == pairs[,1],"transA"] <- position
  } else if (x %in% pairs[,2]) {
    .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs[x == pairs[,2],"transB"] <- position
  } else {
    print(paste("Warning! stock is no pair ", x))
  }
}

#get position from pair stocks
#protfolio
#x, a stock symbol
#return, position
getPairPosition <- function(portfolio, x)
{
  pairs <- .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs
  
  qty <- 0
  
  if (x %in% pairs[,1]) {
    qty <- .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs[x == pairs[,1],"transA"]
  } else if (x %in% pairs[,2]) {
    qty <- .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs[x == pairs[,2],"transB"]
  } else {
    print(paste("Warning! stock is no pair ", x))
  }
  return (as.numeric(qty))
}

#set position of pair stocks
#protfolio
#x, a stock symbol
#position
#lvls
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

#get position from pair stocks
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

#set position of pair stocks
#protfolio
#x, a stock symbol
#position
#lvls
#return, N/A
setPairInitPosition <- function(portfolio, x, position)
{
  
  pairs <- .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs
  
  if (x %in% pairs[,1]) {
    .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs[x == pairs[,1],"transAInit"] <- position
  } else if (x %in% pairs[,2]) {
    .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs[x == pairs[,2],"transBInit"] <- position
  } else {
    print(paste("Warning! stock is no pair ", x))
  }
}

#get position from pair stocks
#protfolio
#x, a stock symbol
#return, position
getPairInitPosition <- function(portfolio, x)
{
  pairs <- .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs
  
  qty <- 0
  
  if (x %in% pairs[,1]) {
    qty <- .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs[x == pairs[,1],"transAInit"]
  } else if (x %in% pairs[,2]) {
    qty <- .blotter[[paste('portfolio', portfolio, sep='.')]]$pairs[x == pairs[,2],"transBInit"]
  } else {
    print(paste("Warning! stock is no pair ", x))
  }
  return (as.numeric(qty))
}

#get position from pair stocks
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
        "Spread.cross.upper",
        "Stock_LongStartTime"
      ),
      formula = "((Spread.cross.upper == 1) & (Stock_LongStartTime  == 1))",
      cross = FALSE
    ),
    label = "Stock.upperAdj"
  )
  
  add.signal(
    portfolio,
    name = "sigFormula",
    arguments = list(
      columns = c(
        "Spread.cross.lower",
        "Stock_LongStartTime"
      ),
      formula = "((Spread.cross.lower == 1) & (Stock_LongStartTime  == 1))",
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
                              portfolio, symbol, ruletype, ..., orderprice, ordersidetype,portfolioName) {
  portf <- getPortfolio(portfolio)
  
  lvls <- getPairLvls(portfolioName, symbol)
  thePair <- as.vector(getPaired(portfolioName,symbol))
  if (is.null(thePair))
    return (0)
  
  pairLocation <- which(thePair==symbol)
  if (pairLocation == 1) {
    transA <- getPosQty(portfolio, symbol, timestamp)
    transB <- getPairPosition(portfolioName, thePair[2])
  } else {
    transB <- getPosQty(portfolio, symbol, timestamp)
    transA <- getPairPosition(portfolioName, thePair[1])
  }
  transAInit <- getPairInitPosition(portfolioName, thePair[1])
  transBInit <- getPairInitPosition(portfolioName, thePair[2])
  transDirection <- getPairDirection(portfolioName, symbol)
  beta <-  mktdata[,"Beta.SPREAD"]
  ratio <- as.numeric(coredata(beta[timestamp]))
  #print(ratio)
  
  if (ordersidetype == "initLong") {
    setPairDirection(portfolioName,symbol,0)
    if (pairLocation == 1) {
        if (transAInit != 0) {
          qtyA <- transAInit
          if (transBInit != 0) {
            qtyB <- transBInit
          } else {
            #re-adjust transBInit by transAinit
            qtyB <- floor(transAInit / ratio)
          }
        } else {
          qtyA <- floor(0.5 * 35000 / orderprice )
          qtyB <- floor(qtyA / ratio)
        }
        setPairPosition(portfolioName,thePair[1],qtyA )
        setPairPosition(portfolioName,thePair[2],qtyB )
        setPairInitPosition(portfolioName,thePair[1],qtyA )
        setPairInitPosition(portfolioName,thePair[2],qtyB )
        qty <- qtyA
      
    } else {
        if (transAInit != 0) {
          qtyA <- transAInit
          if (transBInit != 0) {
            qtyB <- transBInit
          } else {
            #re-adjust transBInit by transAinit
            qtyB <- floor(transAInit / ratio)
          }
        } else {
          qtyA <- floor(0.5 * 35000 / orderprice )
          qtyB <- floor(qtyA / ratio)
        }
        setPairPosition(portfolioName,thePair[1],qtyA )
        setPairPosition(portfolioName,thePair[2],qtyB )
        setPairInitPosition(portfolioName,thePair[1],qtyA )
        setPairInitPosition(portfolioName,thePair[2],qtyB )
        qty <- qtyB
    } 
  } else {
    if (transA <= 0 | transB <= 0)
      return (0)
    
    qty <- 0
    
    
    if (ordersidetype == "upperAdj") {
      if (transDirection == 1) {
        # reject transaction for dup direction
        qty <- 0
      } else {
        qtyB <- floor(transB / lvls)
        qtyA <- floor(transB / lvls * ratio)
        setPairPosition(portfolioName,thePair[1],transA + qtyA )
        setPairPosition(portfolioName,thePair[2],transB - qtyB )
        setPairDirection(portfolioName,symbol,1)
        if (pairLocation == 1) {
          qty <- qtyA
        } else {
          qty <- -qtyB
        }        
      }

    } else if (ordersidetype == "lowerAdj") {   
      if (transDirection == -1) {
        # reject transaction for dup direction
        qty <- 0
      } else {
        qtyA <- floor(transA / lvls)
        qtyB <- floor(transA / lvls / ratio)
        setPairPosition(portfolioName,thePair[1],transA - qtyA )
        setPairPosition(portfolioName,thePair[2],transB + qtyB )
        setPairDirection(portfolioName,symbol,-1)
        if (pairLocation == 1) {
          qty <- -qtyA
        } else {
          qty <- qtyB
        }
      }
    } else {
      qty <- 0
    }         
  }
  
  
  
  orderqty <- qty
  return(orderqty) 
}

################################################################################
# custom indicator function for monthly SMA                                    #
################################################################################
initlongTime <- function(mktdata) {
  
  timeList <- index(mktdata)
  longTime <- rep(0, nrow(mktdata))
  longTime[1] <- 1
  longStart <- rep(1, nrow(mktdata))
  longStart[1] <- 0
  longResult <- cbind(longTime,longStart, 1)
  colnames(longResult) <- c("LongTime","LongStart" ,"LongCondition")
  longResultTimed <- xts(x=longResult, order.by=timeList)
  
  return(longResultTimed)
}