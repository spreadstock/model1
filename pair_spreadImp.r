calculate_spread <- function(x) {
  
  
  aLm <- lm(y ~ x - 1, data=as.data.frame(x))
  betas <- coef(aLm)[1]

  return (round(betas,5))
}
#VA
#This implementation calcuates beta by y / x
calculate_betaVA <- function(dx) {
  beta<-round(dx[,2] / dx[,1],5)
  return (beta)
}
#VB
#This implementation calcuates beta bylinear regression
calculate_betaVB <- function(dx, width) {
  colnames(dx) <- c("x","y")
  beta <-rollapply(dx, width=width, FUN=calculate_spread, by.column=FALSE, align="right")
  return (beta)
}
#V1
#This implementation monitors the change of beta
#Spread = beta(t1) - beta(t0)
#where beta = y / x
#   t1, latest beta
#   t0, first beta when the beta change cross threshold
#Parameters,
#   x, cbind(beta, <beta(t0)>, <spread>), where beta is y/x, <beta(t0)> is the reserved slot for beta(t0)
#       , <spread> is the reserved slot for spread
#   threshold
#To do,
#   1, The current testing does not directly test the stationary of beta
#   2, Require a solution to search best threshold, where threshold need be optimized for each stock pair
calculate_totalbetaV1 <- function(x, threshold) {
  lastPoint <- 1
  xx <- na.fill(x,0)
  num <- nrow(xx)
  checkPoint1 <- threshold
  checkPoint2 <- -threshold
  xx[1,2] <- xx[1,1]
  for (aItem in 2:num) {
    aValue <- coredata(xx[aItem,1]) - coredata(xx[lastPoint,1])
    xx[aItem,3] <- aValue
    if ((aValue > checkPoint1) | (aValue < checkPoint2) ) {
      lastPoint <- aItem
      xx[aItem,2] <- xx[aItem,1]
    } else {
      xx[aItem,2] <- xx[lastPoint,1]
    }
  }
  return (xx)
}
calculate_spreadV1 <- function(x, threshold) {
  dx <- na.omit(x)
  beta<-calculate_betaVA(dx)
  #beta <- lag(beta,1) #no need lag, because the platform already delay 1 day
  beta_total <- calculate_totalbetaV1(cbind(beta, 0, 0), threshold) 

  beta <- cbind(beta_total, threshold, -threshold) #for another pair
  colnames(beta) <- c("Beta","Beta0","BetaTotal","Upper", "Lower")

  return (beta) 
}
######### End of V1

#V2
#This implementation monitors the change of beta
#Spread = y - x * beta(t0)
#where beta = y / x
#   t0, first beta when the spread cross threshold
#Parameters,
#   x, cbind(beta, <beta(t0)>, <spread>), where beta is y/x, <beta(t0)> is the reserved slot for beta(t0)
#       , <spread> is the reserved slot for spread
#   threshold, 
#   dx, the stock data
#To do,
#   1, Require a solution to search best threshold, where threshold need be optimized for each stock pair

calculate_totalbetaV2 <- function(x, threshold,dx) {
  lastPoint <- 1
  xx <- x
  num <- nrow(xx)
  checkPoint1 <- threshold
  checkPoint2 <- -threshold
  xx[1,2] <- xx[1,1]
  xx[1,3] <- NA
  for (aItem in 2:num) {
    if (is.na(xx[lastPoint,1])) {
      xx[aItem,2] <- NA
      xx[aItem,3] <- NA
      lastPoint <- aItem
    } else {
      spread <- round(dx[aItem,2] - dx[aItem,1] * coredata(xx[lastPoint,1]), 5)
      xx[aItem,3] <- spread
      if ((spread > checkPoint1) | (spread < checkPoint2) ) {
        lastPoint <- aItem
        xx[aItem,2] <- xx[aItem,1]
      } else {
        xx[aItem,2] <- xx[lastPoint,1]
      }     
    }
    
    
  }
  return (xx)
}
calculate_spreadV2 <- function(x, threshold) {
  dx <- na.omit(x)
  beta<-calculate_betaVA(dx)
  #beta<-calculate_betaVB(dx,15)
  #beta <- lag(beta,1) #no need lag, because the platform already delay 1 day
  beta_total <- calculate_totalbetaV2(cbind(beta, 0, 0), threshold, dx) 
  
  beta <- cbind(beta_total, threshold, -threshold) #for another pair
  colnames(beta) <- c("Beta","Beta0","BetaTotal","Upper", "Lower")
  
  return (beta) 
}
######### End of V2

#V3
#This implementation monitors the change of spread
#Spread = y - x * beta
#where beta = y / x
#Parameters,
#   x, cbind(beta, <spread>), where beta is y/x
#       , <spread> is the reserved slot for spread
#   dx, the stock data
#To do,
#   N/A

calculate_totalbetaV3 <- function(x, dx) {
  lastPoint <- 1
  xx <- x
  xx[1,3] <- NA
  num <- nrow(xx)
  for (aItem in 2:num) {
    if (is.na(xx[lastPoint,1])) {
      xx[aItem,3] <- NA
    } else {
      spread <- round(dx[aItem,2] - dx[aItem,1] * coredata(xx[lastPoint,1]), 5)
      xx[aItem,3] <- spread
    }
    lastPoint <- aItem
    
  }
  return (xx)
}

calculate_spreadV3 <- function(x, threshold) {
  lookBack <- 20 # lookup can be calcuated as half-life
  dx <- na.omit(x)
  beta<-calculate_betaVB(dx,lookBack)
  beta0<-calculate_betaVA(dx)
  #beta <- lag(beta,1) #no need lag, because the platform already delay 1 day
  beta_total <- calculate_totalbetaV3(cbind(beta, beta0, 0), dx) 
  
  movingAvg = calcuateSMA(beta_total[,3],lookBack) #Moving average
  movingStd = runSD(beta_total[,3],lookBack, sample=FALSE) #Moving standard deviation / bollinger bands
  spreadUpper <- movingAvg + threshold * movingStd
  spreadLower <- movingAvg - threshold * movingStd
  
  beta <- cbind(beta0,beta_total, spreadUpper, spreadLower)
  colnames(beta) <- c("Beta","BetaLinear","Beta0","BetaTotal","Upper", "Lower")
  
  return (beta) 
}

######### End of V3

#V4
#This implementation monitors the change of log return's spread
#Spread = y - x * beta
#where beta = y / x
#   y, x is the log return
#Parameters,
#   x, cbind(beta, <spread>), where beta is y/x
#       , <spread> is the reserved slot for spread
#   dx, the log return stock data
#To do,
#   1, this might require a further test to secure log price is co-integration.

calculate_totalbetaV4 <- function(x, dx) {
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

calculate_spreadV4 <- function(x, threshold) {
  lookBack <- threshold # lookup can be calcuated as half-life
  dx <- calcuateLogReturn(x)
  
  beta<-calculate_betaVB(dx,lookBack)
  #beta <- lag(beta,1) #no need lag, because the platform already delay 1 day
  beta_total <- calculate_totalbetaV4(cbind(beta, 0), dx) 
  
  movingAvg = calcuateSMA(beta_total[,2],lookBack) #Moving average
  movingStd = runSD(beta_total[,2],lookBack, sample=FALSE) #Moving standard deviation / bollinger bands
  spreadUpper <- movingAvg + 1.2 * movingStd
  spreadLower <- movingAvg - 1.2 * movingStd
  
  beta <- cbind(beta_total, spreadUpper, spreadLower)
  colnames(beta) <- c("Beta","BetaTotal","Upper", "Lower")
  
  return (beta) 
}


######### End of V4

#V5
#This implementation monitors the change of spread, and smooth by Kalman
#Spread = y - x * beta
#where beta estimated by Kalman
#Parameters,
#   dx, the stock data
#   threshold,
#   returnBeta, cbind(returnBeta,intercept, spread,upper,lower)
#     where, returnBeta, estimated beta
#       intercept, estimated intercept
#       spread,
#       upper,
#       lower,
#To do,
#   1, this might require a further test to secure log price is co-integration.

calculate_totalbetaV5 <- function (dx, threshold) {
  x<-dx[,1]
  y<-dx[,2]
  n<-length(x)
  mod <- dlmModReg(x)
  diag(C0(mod)) <- c(1E-16, 1E-16)
  V(mod) <- 0.001
  delta <- 0.005
  diag(W(mod)) <- delta/(1-delta)
  filtered <- dlmFilter(y,mod)
  returnBeta <- xts(filtered$m[-1,2],index(x))
  intercept <- xts(filtered$m[-1,1],index(x))
  predR <- dlmSvd2var(filtered$U.R, filtered$D.R)
  X <- cbind(1,x)
  predQ <- sqrt(sapply(1:n,function(i){X[i,]%*%predR[[i]]%*%t(X[i,]) + mod$V}))
  predE <- y - xts(filtered$f, index(x))
  upper <- threshold * predQ
  lower <- threshold * -predQ
  returnBeta <- cbind(returnBeta,intercept, predE,upper,lower)
  returnBeta[1:5,3] <- 0
  #return(returnBeta[-1:-5,])
  return(returnBeta)
}

calculate_spreadV5 <- function(x, threshold) {
  dx <- na.omit(x)
  beta_total <- calculate_totalbetaV5(dx, threshold) 
  beta<-calculate_betaVA(dx)
  beta_total <- cbind(beta,beta_total)
  
  colnames(beta_total) <- c("Beta","Beta0","Intercept","BetaTotal","Upper", "Lower")
  
  return (beta_total) 
}

# a combination of V2 and V5
calculate_totalbetaV5_1 <- function(x, threshold,dx) {
  lastPoint <- 1
  xx <- x
  num <- nrow(xx)
  checkPoint1 <- threshold
  checkPoint2 <- -threshold
  xx[1,3] <- xx[1,1]
  xx[1,4] <- NA
  for (aItem in 2:num) {
    if (is.na(xx[lastPoint,1])) {
      xx[aItem,3] <- NA
      xx[aItem,4] <- NA
      lastPoint <- aItem
    } else {
      spread <- round(dx[aItem,2] - dx[aItem,1] * coredata(xx[lastPoint,1]) - coredata(xx[lastPoint,2]), 5)
      xx[aItem,4] <- spread
      if ((spread > checkPoint1) | (spread < checkPoint2) ) {
        lastPoint <- aItem
        xx[aItem,3] <- xx[aItem,1]
      } else {
        xx[aItem,3] <- xx[lastPoint,1]
      }     
    }
    
    
  }
  xx[1:5,4] <- 0
  return (xx)
}
calculate_spreadV5_1 <- function(x, threshold1, threshold2) {
  dx <- na.omit(x)
  beta_filtered <- calculate_totalbetaV5(dx, threshold2)
  beta0<-beta_filtered[,1:2]
  beta<-calculate_betaVA(dx)
  #beta<-calculate_betaVB(dx,15)
  #beta <- lag(beta,1) #no need lag, because the platform already delay 1 day
  beta_total <- calculate_totalbetaV5_1(cbind(beta0, 0, 0), threshold1, dx) 
  
  beta <- cbind(beta, beta_total, threshold1, -threshold1) #for another pair
  colnames(beta) <- c("Beta","BetaFiltered","Intercept","Beta0","BetaTotal","Upper", "Lower")
  
  return (beta) 
}


################# Main 
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
calculate_spreadV1_test <- function() {
  stock.output <- 'C:/important/ideas/stock/projects/model1/testResult/testRel/'
  
  # 1st level processing
  start_date2 <- "2013-01-01"
  end_date2 <- "2014-12-31"
  
  stock_daily <- loadDailyClose(start_date=start_date2, end_date=end_date2, symbList = c("SH600353" ,"SZ002585"))
  yy<- calculate_spreadV1(stock_daily, 0.075)
  return(yy)
}

calculate_spreadV2_test <- function() {
  stock.output <- 'C:/important/ideas/stock/projects/model1/testResult/testRel/'
  
  # 1st level processing
  start_date2 <- "2013-01-01"
  end_date2 <- "2014-12-31"
  
  stock_daily <- loadDailyClose(start_date=start_date2, end_date=end_date2, symbList = c("SH600353" ,"SZ002585"))
  yy<- calculate_spreadV2(stock_daily, 0.4)
  return(yy)
}

calculate_spreadV3_test <- function() {
  stock.output <- 'C:/important/ideas/stock/projects/model1/testResult/testRel/'
  
  # 1st level processing
  start_date2 <- "2013-01-01"
  end_date2 <- "2014-12-31"
  
  stock_daily <- loadDailyClose(start_date=start_date2, end_date=end_date2, symbList = c("SH600353" ,"SZ002585"))
  yy<- calculate_spreadV3(stock_daily, 1.2)
  return(yy)
}

calculate_spreadV4_test <- function() {
  stock.output <- 'C:/important/ideas/stock/projects/model1/testResult/testRel/'
  
  # 1st level processing
  start_date2 <- "2013-01-01"
  end_date2 <- "2014-12-31"
  
  stock_daily <- loadDailyClose(start_date=start_date2, end_date=end_date2, symbList = c("SH600353" ,"SZ002585"))
  yy<- calculate_spreadV4(stock_daily, 20)
  return(yy)
}

calculate_spreadV5_test <- function() {
  stock.output <- 'C:/important/ideas/stock/projects/model1/testResult/testRel/'
  
  # 1st level processing
  start_date2 <- "2013-01-01"
  end_date2 <- "2014-12-31"
  
  stock_daily <- loadDailyClose(start_date=start_date2, end_date=end_date2, symbList = c("SH600353" ,"SZ002585"))
  yy<- calculate_spreadV5(stock_daily, 1)
  return(yy)
}

calculate_spreadV5_1_test <- function() {
  stock.output <- 'C:/important/ideas/stock/projects/model1/testResult/testRel/'
  
  # 1st level processing
  start_date2 <- "2013-01-01"
  end_date2 <- "2014-12-31"
  
  stock_daily <- loadDailyClose(start_date=start_date2, end_date=end_date2, symbList = c("SH600353" ,"SZ002585"))
  yy<- calculate_spreadV5_1(stock_daily, 0.5, 1)
  return(yy)
}

yy <- calculate_spreadV5_1_test()

