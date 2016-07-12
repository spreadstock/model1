library(quantstrat)
stock.folder <- 'C:/Users/exubixu/Desktop/new1/'
stock.name <- 'SH600000'
tmp <- read.csv(paste(stock.folder, stock.name, '.txt', sep=''), sep=',', check.names=FALSE)
stock  <- xts(tmp[,-1],as.Date(tmp[,1],"%Y/%m/%d")) 
start_date <- '2016-01-01'
end_date <- '2016-06-14'

isUpRateOver <- function(stock, percentage=0.5)
{
  b <- Cl(stock)  
  result <- as.matrix(diff(b))
  result <- na.omit(result) 
  up <- 0
  down <- 0
  for (i in 1:nrow(result)) 
  {
    zz <- as.numeric(result[i,1])
    if((zz > 0)) up=up+1 else down=down+1
  }
#return() 是一个函数
  if(up/nrow(stock) >= percentage) { return (TRUE) } else { return (FALSE) }
}
isUpRateOver(stock)
#
smresult<- SMA(Cl(stock),26)


isMatchUpdayRule<- function(stock)
{
  last10 <- last(Cl(stock),10)
  result <- as.matrix(diff(last10))
  result <- na.omit(result) 
  nrow(result)
  #last day is up, in 10 days, up is greater than down
  if(result[9,1] > 0 && isUpRateOver(last10,0.5)) { return (TRUE) } else { return (FALSE) }
}

print(isMatchUpdayRule(stock))


isMatchVolumeRule<- function(stock)
{
  last4 <- last(Vo(stock),4)
  result <- as.matrix(diff(last4))
  result <- na.omit(result) 
  for (i in 1:nrow(result)) 
  {
    zz <- as.numeric(result[i,1])
    if((zz < 0)) { return (FALSE) }
  }
  last3 <- last(Vo(stock),3)
  last6 = stock[(nrow(stock)-5):(nrow(stock)-3)]
  if(sum(last3)/sum(last6) < 2) { return (FALSE) }
  return (TRUE)
}

print(isMatchVolumeRule(stock))


isMatchMARule<- function(stock)
{
  MA<-Cl(stock)
  MA$SMA250 <- SMA(Cl(stock),250)
  MA$SMA5 <- SMA(Cl(stock),5)
  MA$SMA10 <- SMA(Cl(stock),10)
  MA$SMA20 <- SMA(Cl(stock),20)
  
  result <- na.omit(last(MA,20)) 
  result <- as.matrix(result)
  for (i in 1:nrow(result)) 
  {
    ma250 <- as.numeric(result[i,2])
    ma5 <- as.numeric(result[i,3])
    ma10 <- as.numeric(result[i,4])
    ma20 <- as.numeric(result[i,5])
    if((ma5 < ma250) || (ma10 < ma250) || (ma20 < ma250)) { return (FALSE) }
  }
  return (TRUE)
}

print(isMatchMARule(stock))


isMatchLowPriceRule<- function(stock, backDate=30)
{
  last5 <- last(Cl(stock),5)
  last <- stock[(nrow(stock)-30):(nrow(stock)-5)]
  
  if(min(last5) > min(last)) { return (TRUE) } else { return (FALSE) }
  
}

print(isMatchLowPriceRule(stock))

