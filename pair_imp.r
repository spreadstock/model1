calculate_spread <- function(x) {
  

  aLm <- lm(y ~ x - 1, data=as.data.frame(x))
  betas <- coef(aLm)[1]
  #yValue <- coredata(x$y[nrow(x)])
  #xValue <- coredata(x$x[nrow(x)])

  
#  spreadUpper <- mean(spreadValue, na.rm = TRUE) + sd(spreadValue, na.rm = TRUE)
#  spreadLower <- mean(spreadValue, na.rm = TRUE) - sd(spreadValue, na.rm = TRUE)
#  spreadResult <- c(median(spreadValue), betas)

  
  return (round(betas,5))
}

calculate_newSpread <- function(xValue,yValue,beta) {
  
  
  spreadValue <- coredata(yValue) - coredata(beta) * coredata(xValue)

  return (round(spreadValue,5))
}

# calculate_totalbeta <- function(x, threshold) {
#   lastPoint <- 1
#   xx <- na.fill(x,0)
#   yy <- rep(0, nrow(xx))
#   num <- nrow(xx)
#   checkPoint1 <- threshold
#   checkPoint2 <- -threshold
#   for (aItem in 1:num) {
#     aValue <- sum(xx[lastPoint:aItem,1])
#     yy [aItem] <-aValue
#     if ((aValue > checkPoint1) | (aValue < checkPoint2) ) {
#       xx[aItem,2] <- aValue
#       lastPoint <- aItem + 1
#     } 
#   }
#   return (xx)
# }

calculate_betaV2 <- function(dx, width) {
  colnames(dx) <- c("x","y")
  beta <-rollapply(dx, width=width, FUN=calculate_spread, by.column=FALSE, align="right")
  return (beta)
}

calculate_totalbetaV6 <- function (dx) {
  x<-dx[,1]
  y<-dx[,2]
  n<-length(x)
  mod <- dlmModReg(x)
  diag(C0(mod)) <- c(1E-16, 1E-16)
  V(mod) <- 0.001
  delta <- 0.008
  diag(W(mod)) <- delta/(1-delta)
  filtered <- dlmFilter(y,mod)
  returnBeta <- xts(filtered$m[-1,2],index(x))
  intercept <- xts(filtered$m[-1,1],index(x))
  predR <- dlmSvd2var(filtered$U.R, filtered$D.R)
  X <- cbind(1,x)
  predQ <- sqrt(sapply(1:n,function(i){X[i,]%*%predR[[i]]%*%t(X[i,]) + mod$V}))
  predE <- y - xts(filtered$f, index(x))
  upper <- 1.0 * predQ
  lower <- 1.0 * -predQ
  returnBeta <- cbind(returnBeta,intercept, predE,upper,lower)
  returnBeta[1:5,3] <- 0
  #return(returnBeta[-1:-5,])
  return(returnBeta)
}

calculate_totalbetaV4 <- function(x, dx) {
  lastPoint <- 1
  returns <-calcuateSimpleReturn(dx)
  returnBeta <- calculate_betaV2(returns, 10)
  xx <- cbind(x, returnBeta)
  xx[1,2] <- NA
  num <- nrow(xx)
  for (aItem in 2:num) {
    if (is.na(xx[lastPoint,3])) {
      xx[aItem,2] <- NA
    } else {
      spread <- round(returns[aItem,2] - returns[aItem,1] * coredata(xx[lastPoint,3]), 5)
      xx[aItem,2] <- spread
    }
    lastPoint <- aItem
    
  }
  return (xx)
}

calculate_totalbetaV5 <- function(x, dx) {
  lastPoint <- 1
  lastBeta <- NA
  xx <- x
  xx[1,3] <- NA
  num <- nrow(xx)
  for (aItem in 2:num) {
    if (is.na(lastBeta)) {
      xx[aItem,3] <- NA
    } else {
      spread <- round(dx[aItem,2] - dx[aItem,1] * coredata(lastBeta), 5)
      xx[aItem,3] <- spread
    }
    lastPoint <- aItem
    if (!is.na(xx[lastPoint,2]) & xx[lastPoint,2] != 0)
      lastBeta <- xx[lastPoint,2]
  }
  return (xx)
}

calculate_totalbetaV3 <- function(x, dx) {
  lastPoint <- 1
  xx <- x
  xx[1,2] <- NA
  num <- nrow(xx)
  for (aItem in 2:num) {
    if (is.na(xx[lastPoint,1])) {
      xx[aItem,2] <- NA
    } else {
      spread <- round(dx[aItem,2] - dx[aItem,1] * coredata(xx[lastPoint,1]), 5)
      xx[aItem,2] <- spread
    }
    lastPoint <- aItem

  }
  return (xx)
}

calculate_totalbetaV2 <- function(x, threshold, dx) {
  lastPoint <- 1
  xx <- x
  xx[1,2] <- NA
  num <- nrow(xx)
  checkPoint1 <- threshold
  checkPoint2 <- -threshold
  for (aItem in 2:num) {
    if (is.na(xx[lastPoint,1])) {
      xx[aItem,2] <- NA
      lastPoint <- aItem
    } else {
      spread <- round(dx[aItem,2] - dx[aItem,1] * coredata(xx[lastPoint,1]), 5)
      xx[aItem,2] <- spread
      if ((spread > checkPoint1) | (spread < checkPoint2) ) {
        lastPoint <- aItem
      }     
    }

    
  }
  return (xx)
}

calculate_totalbeta <- function(x, threshold, dx) {
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

calculate_betaPrice <- function(x) {
  
  dx <- na.omit(x)
  beta<-round(dx[,2] / dx[,1],5)
  colnames(beta) <- c("BetaPrice")
  return (beta) 
}

calculate_beta <- function(x) {

  lookBack <- 30
  dx <- na.omit(x)
  #beta<-round(dx[,2] / dx[,1],5)
  beta <- calculate_betaV2(dx,10)
  #beta <- lag(beta,1) #no need lag, because the platform already delay 1 day
  #beta_total <- calculate_totalbeta(cbind(beta, 0), 0.1,dx)
  #beta_total <- calculate_totalbeta(cbind(beta, 0), 0.075,dx)#for another pair
  #beta_total <- calculate_totalbetaV2(cbind(beta, 0), 1,dx)
  #beta_total <- calculate_totalbetaV3(cbind(beta, 0),dx)
  #beta_total <- calculate_totalbetaV4(cbind(beta, 0),dx)

  #movingAvg = calcuateSMA(beta_total[,2],lookBack) #Moving average
  #movingStd = runSD(beta_total[,2],lookBack, sample=FALSE) #Moving standard deviation / bollinger bands
  #spreadUpper <- movingAvg + 2.5 * movingStd
  #spreadLower <- movingAvg - 2.5 * movingStd
  #beta <- cbind(beta_total, 1, -1)
  #beta <- cbind(beta_total, 0.075, -0.075) #for another pair
  #beta <- cbind(beta_total, spreadUpper, spreadLower)

  #colnames(beta) <- c("Beta","BetaTotal","Upper", "Lower")
  #colnames(beta) <- c("Beta","BetaTotal","BetaReturn", "Upper", "Lower")
  
  
  #V5
  #need calcuate ZScore for Beta first
  #movingAvgBeta = calcuateSMA(beta,lookBack) #Moving average
  #movingStdBeta = runSD(beta,lookBack, sample=FALSE) #Moving standard deviation / bollinger bands
  #betaUpper <- movingAvgBeta + 1.5 * movingStdBeta
  #betaLower <- movingAvgBeta - 1.5 * movingStdBeta
  #newBeta <- ifelse (beta > betaUpper | beta < betaLower ,0, beta)
  #beta <- cbind(beta, newBeta,betaUpper, betaLower)
  #lastBeta <- NA 
  #for (aBeta in 1:nrow(beta)) {
  #  if (is.na(beta[aBeta,2])) {
  #    
  #  } else if (beta[aBeta,2] != 0) {
  #    lastBeta <- beta[aBeta,2]
  #  } else {
  #    beta[aBeta,2] <- lastBeta
  #  }
  #
  #}
  
  beta <- cbind(beta,beta)
  beta_total <- calculate_totalbetaV5(cbind(beta, 0),dx)
  movingAvg = calcuateSMA(beta_total[,3],lookBack) #Moving average
  movingStd = runSD(beta_total[,3],lookBack, sample=FALSE) #Moving standard deviation / bollinger bands
  spreadUpper <- movingAvg + 1.5 * movingStd
  spreadLower <- movingAvg - 1.5 * movingStd
  beta <- cbind(beta_total, spreadUpper, spreadLower)
  
  
  colnames(beta) <- c("Beta","newBeta","BetaTotal","Upper", "Lower")
  #colnames(beta) <- c("Beta","newBeta","Upper", "Lower")
  
  #V6
  #beta <- calculate_totalbetaV6(dx)
  #colnames(beta) <- c("Beta","intercept","BetaTotal","Upper", "Lower")
  
  return (beta) 
}

loadDailyClose <- function (start_date, end_date, symbList = NULL) {
  #stock.folder.daily <- 'C:/important/ideas/stock/projects/model1/StockDatas/Bluechips/'
  #stock.folder.daily <- 'C:/important/ideas/stock/projects/model1/javaCode/Data/Median2/bank/after/'
  #stock.folder.daily <- 'C:/important/ideas/stock/projects/model1/javaCode/Data/Median2/'
  stock.folder.daily <- 'C:/important/ideas/stock/projects/model1/StockDatas/2016-08-09-Former_Rehabilitation_leaned/'
  
  if (length(symbList) == 0) {
    #symbList <- c("SH600000","SH600037","SH600039","SH600053","SH600054","SH600056", "SH600090", "SH600094", "SH600074","SH601872","SH601908")
    symbList <- listStocksFromDir(stock.folder.daily)
    
  }
  
  entStocks <- loadMultipleStock(stock.folder.daily, symbList)
  
  x<-subsetByDateRange(entStocks, start_date, end_date)
  
  return (x)
}

rollingV2_1 <- function(x, width, FUN, PREFUN) {
  
  xIn <- x
  timeList <- index(x)
  
  
  rollingResultList <- matrix(NA,nrow=(width - 1), ncol= 1);
  rollingResult <- 0
  
  for (aNext in 1:(length(timeList)-width + 1)) {
    startPoint <- timeList[aNext]
    endPoint <- timeList[aNext + width - 1]
    startDateOfWindow <- as.Date(startPoint)
    endDateOfWindow <- as.Date(endPoint)
    firstWindow <- window(x,start=startDateOfWindow,end=endDateOfWindow)
    
    #clean up data, remove stocks which miss data
    #firstWindow <- firstWindow[,colSums(is.na(firstWindow)) < width]
    #firstWindow <- na.locf(firstWindow, fromLast = FALSE)
    #firstWindow <- na.locf(firstWindow, fromLast = TRUE)
    firstWindow <- PREFUN (firstWindow)
    #clean up data, remove stocks which no change in window
    #firstWindow <- firstWindow[,colSums(firstWindow == 0) < nrow(firstWindow)]
    
    #cannot proceed, if only one column or nothing in window
    if ((ncol(firstWindow) < 2) | (nrow(firstWindow) == 0)) {
      rollingResult <- NA
    } else {
      #call function first window
#      if (aNext == 1) {
        colnames(firstWindow) <- c("x", "y")
        rollingResult <- FUN(firstWindow)
#      } 
        

      rollingResultList <- rbind(rollingResultList, rollingResult)

    }
    
  }
  
  
  #add time line
  lookBack <- 60
  
  rollingResultTimed <- xts(x=rollingResultList, order.by=timeList)
  stockData <- PREFUN(x)
  #change beta
  spread <- stockData[,2] - lag(rollingResultTimed[,1], 1) * stockData[,1]
  #not change beta
  #spread <- stockData[,2] - rollingResult * stockData[,1]
  movingAvg = calcuateSMA(spread,lookBack) #Moving average
  movingStd = runSD(spread,lookBack, sample=FALSE) #Moving standard deviation / bollinger bands
  spreadUpper <- movingAvg + 2 * movingStd
  spreadLower <- movingAvg - 2 * movingStd
  #rollingResultTimed <- cbind(spread, rollingResultTimed, spreadUpper, spreadLower,movingAvg,0,0, calculate_betaPrice(x))
  rollingResultTimed <- cbind(spread, rollingResultTimed, spreadUpper, spreadLower,movingAvg,0,0)
  #rollingResultTimed <- cbind(spread, rollingResult, spreadUpper, spreadLower)

  colnames(rollingResultTimed) <- c("OldSpread","OldBeta","Upper","Lower", "Mean","Beta","Sprea")
  
  #adjust beta
  lastBetaPoint <- width
  for (aNext in 2:(length(timeList)-width)) {
    nextStartPoint <- aNext
    nextEndPoint <- nextStartPoint + width - 1
    
    aBeta <- rollingResultTimed[lastBetaPoint,2]
    aUpper <- rollingResultTimed[nextEndPoint,3]
    aLower <- rollingResultTimed[nextEndPoint,4]
    if (!is.na(aBeta) && !is.na(aUpper) && !is.na(aLower)) {
      aBetaPrice <- round(x[lastBetaPoint,2] / x[lastBetaPoint,1],5)
      xValue <- stockData[nextEndPoint,1]
      yValue <- stockData[nextEndPoint,2]
      aSpread <- calculate_newSpread(xValue,yValue,aBeta)
      if ((aSpread > aUpper) | (aSpread < aLower)) {
        #no need re-calcuate beta, value already available
        #dataWindow <- stockData[nextStartPoint:nextEndPoint,]
        #colnames(dataWindow) <- c("x", "y")
        #aBeta <- calculate_spread(dataWindow)
        aBetaPrice <- round(x[nextEndPoint,2] / x[nextEndPoint,1],5)
        #aBeta <- rollingResultTimed[nextEndPoint,2]
        #aSpread <- calculate_newSpread(xValue,yValue,aBeta)
        rollingResultTimed[nextEndPoint,6] <- aBetaPrice
        rollingResultTimed[nextEndPoint,7] <- aSpread
        lastBetaPoint <- nextEndPoint
      } else {
        rollingResultTimed[nextEndPoint,6] <- aBetaPrice
        rollingResultTimed[nextEndPoint,7] <- aSpread
      }
    } else {
      lastBetaPoint <- nextEndPoint 
    }

  }
  
  
  
  #mktdataTimed <- xts (order.by = index(xIn))
  #rollingResultTimed <- merge(mktdataTimed, rollingResultTimed, all=FALSE, join = 'left')
  
  return (rollingResultTimed)
  #return (rollingResultList)
}


# make a simple init long order sizing function
#######################_ORDER SIZING FUNCTION_##################################
# check to see which stock it is. If it's the second stock, reverse orderqty and 
# orderside
osInitLongPos <- function (data, timestamp, orderqty, ordertype, orderside, 
                            portfolio, symbol, ruletype, ..., orderprice) {
  portf <- getPortfolio(portfolio)
  #check to make sure pair slot has the things needed for this function
  if (!any(names(portf$pair) == "MaxPos") || !any(names(portf$pair) == "lvls")) 
    stop('pair must contain MaxPos and lvls')  
  
  if (portf$pair[symbol] == 1) {
    qty <- 1000
  } else {
    qty <- 2000
  }


  orderside = 'long'
  #if (qty > 0) orderside = 'long'
  #if (qty < 0) orderside = 'long'
  
  #print(timestamp)
  #print(qty)
  # if (qty != 0 )
  #   orderqty <- osMaxPos(data=data,timestamp=timestamp, orderqty=qty,
  #                      ordertype=ordertype, orderside=orderside,
  #                      portfolio=portfolio, symbol=symbol, ruletype=ruletype, 
  #                      ...)
  # else 
  #   orderqty <- 0
  orderqty <- qty
  
  #Add the order here instead of in the ruleSignal function
  if (!is.null(orderqty) & !orderqty == 0 & !is.null(orderprice)) {
    addOrder(portfolio=portfolio, symbol=symbol, 
             timestamp=timestamp, qty=orderqty, price=as.numeric(orderprice), 
             ordertype=ordertype, side=orderside, replace=FALSE,
             status="open", ...=...)
  }
  return(0) #so that ruleSignal function doesn't also try to place an order
}

# make an order sizing function
#######################_ORDER SIZING FUNCTION_##################################
# check to see which stock it is. If it's the second stock, reverse orderqty and 
# orderside
osSpreadMaxPos <- function (data, timestamp, ordertype, orderside, 
                            portfolio, symbol, ruletype, ..., orderprice, ordersidetype) {
  #orderprice <- as.numeric(orderprice1[timestamp])
  maxstock <- 5000
  portf <- getPortfolio(portfolio)
  #check to make sure pair slot has the things needed for this function
  if (!any(names(portf$pair) == "MaxPos") || !any(names(portf$pair) == "lvls")) 
    stop('pair must contain MaxPos and lvls')  
  
  maxPos <- portf$pair["MaxPos"]
  lvls <- portf$pair["lvls"]
  transA <- portf$pair["transA"]
  transB <- portf$pair["transB"]
  transBInit <- portf$pair["transBInit"]
  beta <-  mktdata[,"Beta.SPREAD"]
  ratio <- as.numeric(coredata(beta[timestamp]))
  #print(ratio)

  qty <- 0
  posStock <- getPosQty(portfolio, symbol, timestamp)
  #posStock1 <- getPosQty(portfolio, names(portf$pair[1]), timestamp)
  #posStock2 <- getPosQty(portfolio, names(portf$pair[2]), timestamp)
  # aPosStock <- getPosLimit(portfolio, names(portf$pair[1]), timestamp)
  # maxPosStock1 <- coredata(aPosStock$MaxPos)
  # aPosStock <- getPosLimit(portfolio, names(portf$pair[2]), timestamp)
  # maxPosStock2 <- coredata(aPosStock$MaxPos)
  
  
  # Nx * Px + Ny * Py = maxPos
  # Py = Px * r
  # Opt1
  # Nx * Px + Ny * Px * r = maxPos => Nx + Ny * r = maxPos / Px
  # Nx = 1/2 * maxPos / Px, Ny = 1/2 * maxPos / (Px * r)
  # Opt2
  # Nx * Py / r + Ny * Py = maxPos => Nx/r + Ny = maxPos / Py
  # Nx = 1/2 * maxPos * r / Py, Ny = 1/2 * maxPos / Py
  if (ordersidetype == "initLong") {
    if (portf$pair[symbol] == 1) {
      #not yet init
      if (transA == 0) {
        qty <- floor(0.5 * maxPos / orderprice )
        qtyB <- floor(0.5 * maxPos / (orderprice * ratio))
        .blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair["transA"] <- qty
        .blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair["transB"] <- qtyB
        .blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair["transBInit"] <- qtyB
        #portf$pair["lvls"] <- qtyB
        # addPosLimit(portfolio = portfolio,
        #             timestamp = timestamp,
        #             symbol = names(portf$pair[1]),
        #             maxpos = maxstock ,
        #             longlevels = lvls,
        #             minpos = -maxstock,
        #             shortlevels = lvls)
        # addPosLimit(portfolio = portfolio,
        #     timestamp = timestamp,
        #     symbol = names(portf$pair[2]),
        #     maxpos = qtyB,
        #     longlevels = lvls,
        #     minpos = -qtyB,
        #     shortlevels = lvls)
      } else { #already init
        qty <- 0
      }
      
    } else {
      #not yet init
      if (transB == 0) {
      #  qtyA <- floor((0.5 * maxPos / orderprice ))
      #  qty <- floor(0.5 * maxPos * ratio / orderprice)
      #  .blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair["transA"] <- qtyA
      #  .blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair["transB"] <- qty
        #portf$pair["lvls"] <- qtyA
        # addPosLimit(portfolio = portfolio,
        #   timestamp = timestamp,
        #   symbol = names(portf$pair[1]),
        #   maxpos = qtyA,
        #   longlevels = lvls,
        #   minpos = -qtyA,
        #   shortlevels = lvls)
        # addPosLimit(portfolio = portfolio,
        #             timestamp = timestamp,
        #             symbol = names(portf$pair[2]),
        #             maxpos = maxstock ,
        #             longlevels = lvls,
        #             minpos = -maxstock,
        #             shortlevels = lvls)
      #} else { #already init
      #  qty <- transBInit
      #}
      } else {
        qty <- transBInit
      }
    }
    
  } else {

    # currentQty <- floor(0.5 * maxPos)
    # transCheck <- abs(trans)
    # if (transCheck > 0) {
    #   for (i in 1:transCheck) {
    #     currentQty <- floor ( currentQty * (1- 1/lvls))
    #   }
    #   #we buy back if current qty not aligned
    #   currentQty <- floor(currentQty / orderprice)
    #   buyBackQty <- currentQty - posStock
    # } else {
    #   currentQty <- floor(currentQty / orderprice)
    #   buyBackQty <- currentQty - posStock
    # }
    
    if (ordersidetype == "upperAdj") {
      if (portf$pair[symbol] == 1) {
        #qty <- floor(currentQty / lvls) + buyBackQty
        #trans <- trans + 1
        #calcuate value based on the other side
        qtyB <- floor(transB / lvls)
        # if (abs(qtyB) >= trans)
        #   qtyB <- trans
        qty <- floor(transB / lvls * ratio)
        #.blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair["transA"] <- transA + qtyA
        .blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair["transB"] <- transB - qtyB
      } else {
        # qty <- -floor(currentQty / lvls) + buyBackQty
        # trans <- trans - 1
        qty <- -floor(posStock / lvls)
        # if (abs(qty) >= posStock)
        #   qty <- -posStock
        #estimate the other stock
        qtyA <- floor(posStock / lvls * ratio)
        qtyA <- transA + qtyA
        .blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair["transA"] <- qtyA
      }  
    } else if (ordersidetype == "lowerAdj") {    
      if (portf$pair[symbol] == 1) {
        # qty <- -floor(currentQty / lvls) + buyBackQty
        # trans <- trans - 1
        qty <- -floor(posStock / lvls)
        #estimate the other stock
        qtyB <- floor(posStock / lvls / ratio)
        qtyB <- transB + qtyB
        .blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair["transB"] <- qtyB

      } else {
        # qty <- floor(currentQty / lvls) + buyBackQty
        # trans <- trans + 1
        qtyA <- floor(transA / lvls)
        # if (abs(qtyB) >= trans)
        #   qtyB <- trans
        qty <- floor(transA / lvls / ratio)
        .blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair["transA"] <- transA - qtyA
        ## Comment out next line to use equal ordersizes for each stock.
        # addPosLimit(
        #   portfolio = portfolio,
        #   timestamp = timestamp,
        #   symbol = symbol,
        #   maxpos = floor(MaxPos / ratio),
        #   longlevels = lvls,
        #   minpos = 0,
        #   shortlevels = lvls
        # ) 
      }
    }else {
      qty <- 0
    }
  }




  
  #orderside = 'long'
  #if (qty > 0) orderside = 'long'
  #if (qty < 0) orderside = 'long'
  
  #print(timestamp)
  #print(qty)
  # if (qty != 0 )
  #   orderqty <- osMaxPos(data=data,timestamp=timestamp, orderqty=qty,
  #                      ordertype=ordertype, orderside=orderside,
  #                      portfolio=portfolio, symbol=symbol, ruletype=ruletype,
  #                      ...)
  # else
  #   orderqty <- 0
  orderqty <- qty
  
  #Add the order here instead of in the ruleSignal function
  # if (!is.null(orderqty) & !orderqty == 0 & !is.null(orderprice)) {
  #   addOrder(portfolio=portfolio, symbol=symbol, 
  #            timestamp=timestamp, qty=orderqty, price=as.numeric(orderprice), 
  #            ordertype=ordertype, side=orderside, replace=FALSE,
  #            status="open", ...=...)
  # }
  return(orderqty) #so that ruleSignal function doesn't also try to place an order
}


osSpreadMaxDollarV2 <- function(data, timestamp, orderqty, ordertype, orderside,
                              portfolio, symbol, prefer="Open", tradeSize,
                              maxSize, integerQty=TRUE,
                              ...) {
  portf <- getPortfolio(portfolio)
  symbol1 <- names(portf$pair[1])
  symbol2 <- names(portf$pair[2])
  beta <-  mktdata[,"Beta.SPREAD"]
  ratio <- as.numeric(coredata(beta[timestamp]))
  if (ratio == 0 ) {
    #to do
    return (0)
  } else {
    posStock1 <- getPosQty(portfolio, symbol1, timestamp)
    posStock2 <- getPosQty(portfolio, symbol2, timestamp)
    
    if(prefer=="Close") {
      price <- as.numeric(Cl(data[timestamp,]))
    } else {
      price <- as.numeric(Op(data[timestamp,]))
    }
    if (portf$pair[symbol] == 1) {
      priceOther <- price * ratio
      currentPos <- posStock1
    } else {
      priceOther <- price / ratio
      currentPos <- posStock2
    }
    
    if ((posStock1 > 0 ) & (posStock2 > 0)) {
      remainValue <- 0
    } else if (posStock1 == 0 & posStock2 == 0) {
      remainValue <- maxSize / 2
    } else {
      totalPosValue <- posStock1 * price + posStock2 * priceOther
      remainValue <- maxSize - totalPosValue
    }
    
    #posVal <- pos*price
    if (orderside=="short") {
      dollarsToTransact <- max(tradeSize, maxSize-posVal)
      #If our position is profitable, we don't want to cover needlessly.
      if(dollarsToTransact > 0) {dollarsToTransact=0}
    } else {
        dollarsToTransact <- remainValue
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

osSpreadMaxDollar <- function(data, timestamp, orderqty, ordertype, orderside,
                          portfolio, symbol, prefer="Open", tradeSize,
                          maxSize, integerQty=TRUE,
                          ...) {
  portf <- getPortfolio(portfolio)
  symbol1 <- names(portf$pair[1])
  symbol2 <- names(portf$pair[2])
  beta <-  mktdata[,"Beta.SPREAD"]
  ratio <- as.numeric(coredata(beta[timestamp]))
  if (ratio == 0 ) {
    #to do
    return (0)
  } else {
    posStock1 <- getPosQty(portfolio, symbol1, timestamp)
    posStock2 <- getPosQty(portfolio, symbol2, timestamp)
    
    if(prefer=="Close") {
      price <- as.numeric(Cl(data[timestamp,]))
    } else {
      price <- as.numeric(Op(data[timestamp,]))
    }
    if (portf$pair[symbol] == 1) {
      priceOther <- price * ratio
    } else {
      priceOther <- price / ratio
    }
    
    totalPosValue <- posStock1 * price + posStock2 * priceOther
    remainValue <- maxSize - totalPosValue
    
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
  print(paste("reblance",symbol,timestamp))
  return (1)
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
    for (i in 1:length(pindex)) {
      for (symbol in symbols) {
        mktdata <- get(symbol, pos = st)
        md_subset <- mktdata[pindex[i], ]
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

osSpreadSize <- function (data, timestamp, ordertype, orderside, 
                             portfolio, symbol, ruletype, ..., orderprice, ordersidetype) {
  #orderprice <- as.numeric(orderprice1[timestamp])
  portf <- getPortfolio(portfolio)
  #check to make sure pair slot has the things needed for this function
  if (!any(names(portf$pair) == "MaxPos") || !any(names(portf$pair) == "lvls")) 
    stop('pair must contain MaxPos and lvls')  
  
  maxPos <- portf$pair["MaxPos"]
  lvls <- portf$pair["lvls"]
  beta <-  mktdata[,"Beta.SPREAD"]
  ratio <- as.numeric(coredata(beta[timestamp]))
  #print(ratio)
  
  qty <- 0
  symbol1 <- names(portf$pair[1])
  symbol2 <- names(portf$pair[2])
  posStock1 <- getPosQty(portfolio, symbol1, timestamp)
  posStock2 <- getPosQty(portfolio, symbol2, timestamp)
  
  if ((posStock1 == 0 ) | (posStock2 == 0)) {
    return (0)
  }
  
  if (ordersidetype == "upperAdj") {
    #sell spread
    # amount decided by the stock 2
    qtyB <- floor(posStock2 / lvls)
    if (portf$pair[symbol] == 1) {
        qty <- floor(qtyB * ratio)
    } else {
      qty <- -qtyB
      
    }  
  } else if (ordersidetype == "lowerAdj") {  
    #buy spread
    # amount decided by the stock 1
    qtyA <- floor(posStock1 / lvls)
    if (portf$pair[symbol] == 1) {
      qty <- -qtyA

    } else {
      qtyB <- floor(qtyA / ratio)
      qty <- qtyB
    }
  }else {
    qty <- 0
  }
  
  
  orderqty <- qty
  
  return(orderqty) #so that ruleSignal function doesn't also try to place an order
}

osSpreadForPair <- function (data, timestamp, ordertype, orderside, 
                            portfolio, symbol, ruletype, ..., orderprice, ordersidetype) {
  #orderprice <- as.numeric(orderprice1[timestamp])
  portf <- getPortfolio(portfolio)
  #check to make sure pair slot has the things needed for this function
  if (!any(names(portf$pair) == "MaxPos") || !any(names(portf$pair) == "lvls")) 
    stop('pair must contain MaxPos and lvls')  
  
  maxPos <- portf$pair["MaxPos"]
  lvls <- portf$pair["lvls"]
  transA <- portf$pair["transA"]
  transB <- portf$pair["transB"]
  transBInit <- portf$pair["transBInit"]
  beta <-  mktdata[,"Beta.SPREAD"]
  ratio <- as.numeric(coredata(beta[timestamp]))
  #print(ratio)
  
  qty <- 0
  #capture symbols for pair stock1_stock2
  #Stock1st <- word(symbol,1,sep=fixed("_"))
  #Stock2nd <- word(symbol,2,sep=fixed("_"))
  #posStock1st <- getPosQty(portfolio, Stock1st, timestamp)
  #posStock2nd <- getPosQty(portfolio, Stock2nd, timestamp)
  currentPosStock <- getPosQty(portfolio, symbol, timestamp)


  if (ordersidetype == "upperAdj") {
    if (portf$pair[symbol] == 1) {
      #calcuate value based on the other side
      currentQtyB <- posStock2nd + 
      qtyB <- floor(posStock2nd / lvls)
      # if (abs(qtyB) >= trans)
      #   qtyB <- trans
      qty <- floor(transB / lvls * ratio)
      #.blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair["transA"] <- transA + qtyA
      .blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair["transB"] <- transB - qtyB
    } else {
      # qty <- -floor(currentQty / lvls) + buyBackQty
      # trans <- trans - 1
      qty <- -floor(posStock / lvls)
      # if (abs(qty) >= posStock)
      #   qty <- -posStock
      #estimate the other stock
      qtyA <- floor(posStock / lvls * ratio)
      qtyA <- transA + qtyA
      .blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair["transA"] <- qtyA
    }  
  } else if (ordersidetype == "lowerAdj") {    
    if (portf$pair[symbol] == 1) {
      # qty <- -floor(currentQty / lvls) + buyBackQty
      # trans <- trans - 1
      qty <- -floor(posStock / lvls)
      #estimate the other stock
      qtyB <- floor(posStock / lvls / ratio)
      qtyB <- transB + qtyB
      .blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair["transB"] <- qtyB
      
    } else {
      # qty <- floor(currentQty / lvls) + buyBackQty
      # trans <- trans + 1
      qtyA <- floor(transA / lvls)
      # if (abs(qtyB) >= trans)
      #   qtyB <- trans
      qty <- floor(transA / lvls / ratio)
      .blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair["transA"] <- transA - qtyA
      ## Comment out next line to use equal ordersizes for each stock.
      # addPosLimit(
      #   portfolio = portfolio,
      #   timestamp = timestamp,
      #   symbol = symbol,
      #   maxpos = floor(MaxPos / ratio),
      #   longlevels = lvls,
      #   minpos = 0,
      #   shortlevels = lvls
      # ) 
    }
  }else {
    qty <- 0
  }
  
  
  
  
    if (ordersidetype == "initLong") {
    if (portf$pair[symbol] == 1) {
      #not yet init
      if (transA == 0) {
        qty <- floor(0.5 * maxPos / orderprice )
        qtyB <- floor(0.5 * maxPos / (orderprice * ratio))
        .blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair["transA"] <- qty
        .blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair["transB"] <- qtyB
        .blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair["transBInit"] <- qtyB
        #portf$pair["lvls"] <- qtyB
        # addPosLimit(portfolio = portfolio,
        #             timestamp = timestamp,
        #             symbol = names(portf$pair[1]),
        #             maxpos = maxstock ,
        #             longlevels = lvls,
        #             minpos = -maxstock,
        #             shortlevels = lvls)
        # addPosLimit(portfolio = portfolio,
        #     timestamp = timestamp,
        #     symbol = names(portf$pair[2]),
        #     maxpos = qtyB,
        #     longlevels = lvls,
        #     minpos = -qtyB,
        #     shortlevels = lvls)
      } else { #already init
        qty <- 0
      }
      
    } else {
      #not yet init
      if (transB == 0) {
        #  qtyA <- floor((0.5 * maxPos / orderprice ))
        #  qty <- floor(0.5 * maxPos * ratio / orderprice)
        #  .blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair["transA"] <- qtyA
        #  .blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair["transB"] <- qty
        #portf$pair["lvls"] <- qtyA
        # addPosLimit(portfolio = portfolio,
        #   timestamp = timestamp,
        #   symbol = names(portf$pair[1]),
        #   maxpos = qtyA,
        #   longlevels = lvls,
        #   minpos = -qtyA,
        #   shortlevels = lvls)
        # addPosLimit(portfolio = portfolio,
        #             timestamp = timestamp,
        #             symbol = names(portf$pair[2]),
        #             maxpos = maxstock ,
        #             longlevels = lvls,
        #             minpos = -maxstock,
        #             shortlevels = lvls)
        #} else { #already init
        #  qty <- transBInit
        #}
      } else {
        qty <- transBInit
      }
    }
    
  } else {
    
    # currentQty <- floor(0.5 * maxPos)
    # transCheck <- abs(trans)
    # if (transCheck > 0) {
    #   for (i in 1:transCheck) {
    #     currentQty <- floor ( currentQty * (1- 1/lvls))
    #   }
    #   #we buy back if current qty not aligned
    #   currentQty <- floor(currentQty / orderprice)
    #   buyBackQty <- currentQty - posStock
    # } else {
    #   currentQty <- floor(currentQty / orderprice)
    #   buyBackQty <- currentQty - posStock
    # }
    
    if (ordersidetype == "upperAdj") {
      if (portf$pair[symbol] == 1) {
        #qty <- floor(currentQty / lvls) + buyBackQty
        #trans <- trans + 1
        #calcuate value based on the other side
        qtyB <- floor(transB / lvls)
        # if (abs(qtyB) >= trans)
        #   qtyB <- trans
        qty <- floor(transB / lvls * ratio)
        #.blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair["transA"] <- transA + qtyA
        .blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair["transB"] <- transB - qtyB
      } else {
        # qty <- -floor(currentQty / lvls) + buyBackQty
        # trans <- trans - 1
        qty <- -floor(posStock / lvls)
        # if (abs(qty) >= posStock)
        #   qty <- -posStock
        #estimate the other stock
        qtyA <- floor(posStock / lvls * ratio)
        qtyA <- transA + qtyA
        .blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair["transA"] <- qtyA
      }  
    } else if (ordersidetype == "lowerAdj") {    
      if (portf$pair[symbol] == 1) {
        # qty <- -floor(currentQty / lvls) + buyBackQty
        # trans <- trans - 1
        qty <- -floor(posStock / lvls)
        #estimate the other stock
        qtyB <- floor(posStock / lvls / ratio)
        qtyB <- transB + qtyB
        .blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair["transB"] <- qtyB
        
      } else {
        # qty <- floor(currentQty / lvls) + buyBackQty
        # trans <- trans + 1
        qtyA <- floor(transA / lvls)
        # if (abs(qtyB) >= trans)
        #   qtyB <- trans
        qty <- floor(transA / lvls / ratio)
        .blotter[[paste('portfolio', qs.strategy, sep='.')]]$pair["transA"] <- transA - qtyA
        ## Comment out next line to use equal ordersizes for each stock.
        # addPosLimit(
        #   portfolio = portfolio,
        #   timestamp = timestamp,
        #   symbol = symbol,
        #   maxpos = floor(MaxPos / ratio),
        #   longlevels = lvls,
        #   minpos = 0,
        #   shortlevels = lvls
        # ) 
      }
    }else {
      qty <- 0
    }
  }
  
  
  
  
  
  #orderside = 'long'
  #if (qty > 0) orderside = 'long'
  #if (qty < 0) orderside = 'long'
  
  #print(timestamp)
  #print(qty)
  # if (qty != 0 )
  #   orderqty <- osMaxPos(data=data,timestamp=timestamp, orderqty=qty,
  #                      ordertype=ordertype, orderside=orderside,
  #                      portfolio=portfolio, symbol=symbol, ruletype=ruletype,
  #                      ...)
  # else
  #   orderqty <- 0
  orderqty <- qty
  
  #Add the order here instead of in the ruleSignal function
  # if (!is.null(orderqty) & !orderqty == 0 & !is.null(orderprice)) {
  #   addOrder(portfolio=portfolio, symbol=symbol, 
  #            timestamp=timestamp, qty=orderqty, price=as.numeric(orderprice), 
  #            ordertype=ordertype, side=orderside, replace=FALSE,
  #            status="open", ...=...)
  # }
  return(orderqty) #so that ruleSignal function doesn't also try to place an order
}


osSpreadForTrend <- function (data, timestamp, ordertype, orderside, 
                            portfolio, symbol, ruletype, ..., orderprice, ordersidetype) {
  portf <- getPortfolio(portfolio)

  lvls <- getPairLvls(portf, symbol)
  thePair <- as.vector(getPaired(symbol))
  if (is.null(thePair))
    return (0)

  pairLocation <- which(thePair, symbol)
  transA <- getPairPosition(portf, symbol)
  transB <- getPairPosition(portf, symOther)
  beta <-  mktdata[,"Beta.SPREAD"]
  ratio <- as.numeric(coredata(beta[timestamp]))
  #print(ratio)
  
  if (ordersidetype == "initLong") {
    if (pairLocation == 1) {
      if (transA != 0) {
        qtyA <- transA
      } else {
        qtyA <- floor(0.5 * 35000 / orderprice )
        qtyB <- floor(0.5 * 35000 / (orderprice * ratio))
        setPairPosition(portf,thePair[1],qtyA )
        setPairPosition(portf,thePair[2],qtyB )
      }
      qty <- qtyA
      
    } else {
      if (transB != 0) {
        qtyB <- transB
      } else {
        qtyA <- floor(0.5 * 35000 / orderprice )
        qtyB <- floor(0.5 * 35000 / (orderprice * ratio))
        setPairPosition(portf,thePair[1],qtyA )
        setPairPosition(portf,thePair[2],qtyB )
      }
      qty <- qtyB
    } 
  } else {
    if (transA == 0 | transB == 0)
      return (0)
    
    qty <- 0
    #posStock <- getPosQty(portfolio, symbol, timestamp)
    
    if (ordersidetype == "upperAdj") {
      qtyB <- floor(transB / lvls)
      qtyA <- floor(transB / lvls * ratio)
      setPairPosition(portf,thePair[1],transA + qtyA )
      setPairPosition(portf,thePair[2],transB - qtyB )
      if (pairLocation == 1) {
        qty <- qtyA
      } else {
        qty <- -qtyB
      }  
    } else if (ordersidetype == "lowerAdj") {   
      qtyA <- floor(transB / lvls)
      qtyB <- floor(transB / lvls / ratio)
      setPairPosition(portf,thePair[1],transA - qtyA )
      setPairPosition(portf,thePair[2],transB + qtyB )
      if (pairLocation == 1) {
        qty <- -qtyA
      } else {
        qty <- qtyB
      }
    }else {
      qty <- 0
    }    
  }
  


  orderqty <- qty
  return(orderqty) 
}



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

################################################################################
# custom indicator function for monthly SMA                                    #
################################################################################
get.longTime <- function(mktdata, date) {
  
  timeList <- index(mktdata)
  longResult <- ifelse (index(mktdata) == date, 1, 0)
  longConditionPos <- first(which(longResult == 1))
  longStart <- ifelse (index(longResult) > longConditionPos, 1, 0)
  longResult <- cbind(longResult, longStart, 1)
  colnames(longResult) <- c("LongTime","LongStart", "LongCondition")
  longResultTimed <- xts(x=longResult, order.by=timeList)

  return(longResultTimed)
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


takeTranxFee <- function(TxnQty, TxnPrice, Symbol,...) {
  
  aFee <- abs(TxnQty) * TxnPrice * -0.005
  return (aFee)
}


################# Main 
stock.output <- 'C:/important/ideas/stock/projects/model1/testResult/testRel/'

# 1st level processing
start_date2 <- "2013-01-01"
end_date2 <- "2014-12-31"

# #stock_daily <- loadDailyClose(start_date=start_date2, end_date=end_date2, symbList = c("SH600391" ,"SZ000738"))
#stock_daily <- loadDailyClose(start_date=start_date2, end_date=end_date2, symbList = c("SH601169" ,"SH601328"))
stock_daily <- loadDailyClose(start_date=start_date2, end_date=end_date2, symbList = c("SH600353" ,"SZ002123"))
#yy <-rollingV2_1(x=stock_daily, width=10, FUN=calculate_spread, PREFUN=calcuateSimpleReturn)
# <-rollingV2_1(x=stock_daily, width=10, FUN=calculate_spread, PREFUN=calcuateAbsPrice)
yy<- calculate_beta(stock_daily)

#betaZZ <- cumsum(na.omit(yy$Beta))
#betaZZ <- yy$Beta / betaZZ
#yy <- cbind (yy, betaZZ)
# zz <- cumsum(na.omit(yy$Spread * (lag(yy$Beta,1))))
# yy<-cbind(yy,zz)
# returns <- calcuateSimpleReturn(stock_daily)
# colnames(yy) <- c("Spread","Beta" ,"Upper","Lower","AccSpread")
