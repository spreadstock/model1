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

calculate_betaPrice <- function(x) {
  
  dx <- na.omit(x)
  beta<-round(dx[,2] / dx[,1],5)
  colnames(beta) <- c("BetaPrice")
  return (beta) 
}

calculate_beta <- function(x) {

  dx <- na.omit(x)
  beta<-round(dx[,2] / dx[,1],5)
  #beta <- lag(beta,1) #no need lag, because the platform already delay 1 day
  beta_total <- calculate_totalbeta(cbind(beta, 0), 0.015)
  beta <- cbind(beta_total, 0.015, -0.015)

  colnames(beta) <- c("Beta","BetaTotal","Upper", "Lower")
  return (beta) 
}

loadDailyClose <- function (start_date, end_date, symbList = NULL) {
  #stock.folder.daily <- 'C:/important/ideas/stock/projects/model1/StockDatas/Bluechips/'
  #stock.folder.daily <- 'C:/important/ideas/stock/projects/model1/javaCode/Data/Median2/bank/after/'
  #stock.folder.daily <- 'C:/important/ideas/stock/projects/model1/javaCode/Data/Median2/'
  stock.folder.daily <- 'C:/important/ideas/stock/projects/model1/StockDatas/2016-08-09-Later_Rehabilitation_Cleaned/'
  
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
      #if (transA == 0) {
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
      #} else { #already init
      #  qty <- transA
      #}
      
    } else {
      #not yet init
      #if (transBInit == 0) {
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
        qty <- transBInit
      #}
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
  longStart <- ifelse (index(longResult) >= longConditionPos, 1, 0)
  longResult <- cbind(longResult, longStart, 1)
  colnames(longResult) <- c("LongTime","LongStart", "LongCondition")
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
start_date2 <- "2012-01-01"
end_date2 <- "2014-12-31"

# #stock_daily <- loadDailyClose(start_date=start_date2, end_date=end_date2, symbList = c("SH600391" ,"SZ000738"))
#stock_daily <- loadDailyClose(start_date=start_date2, end_date=end_date2, symbList = c("SH601169" ,"SH601328"))
#yy <-rollingV2_1(x=stock_daily, width=10, FUN=calculate_spread, PREFUN=calcuateSimpleReturn)
#yy <-rollingV2_1(x=stock_daily, width=10, FUN=calculate_spread, PREFUN=calcuateAbsPrice)


#betaZZ <- cumsum(na.omit(yy$Beta))
#betaZZ <- yy$Beta / betaZZ
#yy <- cbind (yy, betaZZ)
# zz <- cumsum(na.omit(yy$Spread * (lag(yy$Beta,1))))
# yy<-cbind(yy,zz)
# returns <- calcuateSimpleReturn(stock_daily)
# colnames(yy) <- c("Spread","Beta" ,"Upper","Lower","AccSpread")
