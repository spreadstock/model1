#cc.最近三天的收盘价都在10天均线之上
isCloseOverSMA <- function(x)
{
  for (i in 1:nrow(x))
  {
     close <- as.numeric(x[i,1])
     ma10 <- as.numeric(x[i,2])
     if(close < ma10)
	 {
	   return (FALSE)
	 }	 
  }
  return (TRUE)
}

isOverSMA10LastDays <- function(x, flashBackDay=3) 
{ 
  x$SMA10 <- SMA(x,10)
  a <- matrix(FALSE,nrow=nrow(x),ncol=1)
  startIndex <- 10+flashBackDay;
  
  for (i in startIndex:nrow(x))
  {
    last3 <- x[(i-2):i]	
    if(isCloseOverSMA(last3))
    {
      a[i] <- TRUE
    }
  }
  return (a) 
}

#cc.最近三天的收盘价都在10天均线之下
isCloseBelowSMA <- function(x)
{
  for (i in 1:nrow(x))
  {
     close <- as.numeric(x[i,1])
     ma10 <- as.numeric(x[i,2])
     if(close > ma10)
	 {
	   return (FALSE)
	 }	 
  }
  return (TRUE)
}

isBelowSMA10LastDays <- function(x, flashBackDay=3) 
{ 
  x$SMA10 <- SMA(x,10)
  a <- matrix(FALSE,nrow=nrow(x),ncol=1)
  startIndex <- 10+flashBackDay;
  
  for (i in startIndex:nrow(x))
  {
    last3 <- x[(i-2):i]	
    if(isCloseBelowSMA(last3))
    {
      a[i] <- TRUE
    }
  }
  return (a) 
}

#c.	以上买点还必须符号最近10天里上涨时的平均交易量大于下跌时的平均交易量。
isVolumeTrendUp <- function(x)
{
  result <- diff(Cl(x))
  result <- na.omit(result)
  UpSum <- list(NA)
  DownSum <- list(NA)
  for (i in 1:nrow(result))
  {
    if(result[i] > 0) { UpSum <- cbind(UpSum,list(Vo(x)[i+1]))} else { DownSum <- cbind(DownSum,list(Vo(x)[i+1]))}
  }
  UpSum <- UpSum[-1]
  DownSum <- DownSum[-1]
  if(sum(as.numeric(UpSum)) == 0 || sum(as.numeric(DownSum)) == 0) { return (FALSE)}
  if(mean(as.numeric(UpSum)) > mean(as.numeric(DownSum)) ) { return (TRUE)} else { return (FALSE)}
}

isvolumeUp <- function(x, flashBackDay=10) 
{ 
  a <- matrix(FALSE,nrow=nrow(x),ncol=1)
  startIndex <- flashBackDay+1;
  
  for (i in startIndex:nrow(x))
  {
    if(isVolumeTrendUp(x[(i-flashBackDay):i]))
    {
      a[i] <- TRUE
    }
  }
  return (a) 
}

# 0.MACD越过第N次
trainGtOsc <- function(x, numberofBreak = 3)
{
  gtCount <- 0
  a <- matrix(FALSE,nrow=nrow(x),ncol=1)
  for (i in 1:nrow(x))
  {
    if(gtCount< (numberofBreak -1) && !is.na(x[i]) && x[i] > 0)
    {
      gtCount=gtCount+1;
    }
    else if(gtCount >= (numberofBreak -1) && !is.na(x[i]) && x[i] > 0)
    {
      a[i] <- TRUE
      gtCount <- 0
    }
  }
  return (a)  
  
}
  
#a.	当差值（是负数但是 删除）持续变大超过3或5天时，标识为买点
isTrendUp <- function(matrixList)
{
  for (i in 1:(nrow(matrixList)-1))
  {
    if(matrixList[i] > matrixList[i+1])
    {
      return (FALSE)
    }
  }
  return (TRUE)
}

treat_trendGrowMinus <- function(x, targetDiffGrowDay=3) 
{ 
  a <- matrix(FALSE,nrow=nrow(x),ncol=1)
  startIndex <- middleSMA+targetDiffGrowDay
  for (i in startIndex:nrow(x))
  {
    if(isTrendUp(as.matrix(x[(i-targetDiffGrowDay):i])))
    {
      a[i] <- TRUE
    }
  }
  return (a) 
}


#b.	当差值（是负数但是 删除）并持续变大但不到3天立即又变小，可以用最近变大的天数减去变小的天数，
#然后跟最近变大的周期天数相加，如果大于5也可以标识为买点
treat_trendGrowPlus <- function(x, targetShortGrowDay=2, targetDiffGrowDay=3, targetDiffDownDay=2) 
{ 
  a <- matrix(FALSE,nrow=nrow(x),ncol=1)
  startIndex <- middleSMA+targetDiffGrowDay+targetDiffDownDay+1
  for (i in startIndex:nrow(x))
  {
    if(isTrendOneUpCamel(x[(i-targetDiffGrowDay-targetDiffDownDay):i]))
    {
      a[i] <- TRUE
    }
  }
  return (a) 
}

isTrendOneUpCamel <- function(x ,targetShortGrowDay=2, targetDiffGrowDay=3, targetDiffDownDay=2)
{
  topIndex <- 1
  topCount <- 0
  lowIndex <- nrow(x)
  lowCount <- 0
  for (i in 1:nrow(x))
  {
	 if((i != 1) 
	    &&(i != nrow(x))
	    && (as.numeric(x[i]) > as.numeric(x[i-1])) 
	    && (as.numeric(x[i]) > as.numeric(x[i+1])))
	 { 
	      topIndex <- i 
		  topCount <- topCount +1
	 }
	 if((i != 1) 
	    &&(i != nrow(x)) 
	    && (as.numeric(x[i]) < as.numeric(x[i-1])) 
	    && (as.numeric(x[i]) < as.numeric(x[i+1])))
	 { 
	    lowIndex <- i 
		  lowCount <- lowCount +1
	 }
  }
  if(topCount ==1
     &&lowCount==1
	 &&lowIndex > topIndex
	 &&lowIndex - topIndex <= targetDiffDownDay
	 &&nrow(x) - (lowIndex - topIndex) >= targetDiffGrowDay
	 &&topIndex-1 >= targetShortGrowDay) { return (TRUE) } else { return (FALSE) }
 
}
  

#加仓，当上涨0.5个ATR并且还持有股票的时候，就增加ATR follow的买入信号
growCertainATRIndex <- function(x, index, closeTnxPrice, atrTnxPrice, ATRRate=0.5) 
{	
    #print(paste0("growCertainATRIndex: ",nrow(x),"  ",index))
	if(index == nrow(x))
	{
	  return (0)
	}
	aaa <- strsplit(colnames(x)[1],"[.]")
	symbol <- aaa[[1]][1]
	print(paste0("growCertainATRIndex: ",symbol))
	for (i in (index+1):nrow(x))
    {
	   diff <-(as.numeric(x[i]) - closeTnxPrice)
	   if( diff >= atrTnxPrice)
	   {
             return (i)
       }
    }
	return (0)
}

findGrowATRSig <- function(x, index, ATRRate=0.5) 
{
#如果符合趋势，这index+1是交易点
     if(x[index]$X1.isvolumeUp == 1 & (x[index]$X1.treat_trendGrowPlus == 1 | x[index]$X1.trendGrowMinus ==1))  
	 #if(x[index]$longEntry == 1) 
	 {
	     if(index == nrow(x))
		 {
		    return (0)
		 }
	     tnxIndex <- index+1
		 closeTnxPrice <- as.numeric(Cl(x[tnxIndex]))
         atrTnxPrice <- (ATRRate * as.numeric(x[tnxIndex]$atr))
		 return (growCertainATRIndex(Cl(x),index=tnxIndex,closeTnxPrice=closeTnxPrice,atrTnxPrice=atrTnxPrice))
	 }
	 
	 else
	 {
	    return (0)
	 }
}

addGrowATRSig <- function(x) 
{
  a <- matrix(FALSE,nrow=nrow(x),ncol=1)
  for (i in 2:nrow(x))
  {
    index <- findGrowATRSig(x,i)
	if(index != 0)
    {
      a[index] <- TRUE
    }
  }
  return (a) 
}
  

	
######################################################

#d.	当差值是正数但是持续变小超过3或5天，或者变负数时标识为卖点。
isTrendDown <- function(matrixList)
{
  for (i in 1:(nrow(matrixList)-1))
  {
    if(matrixList[i] < matrixList[i+1])
    {
      return (FALSE)
    }
  }
  return (TRUE)
}

treat_trendDownMinus <- function(x, targetDiffDownDay=3) 
{ 
  a <- matrix(FALSE,nrow=nrow(x),ncol=1)
  startIndex <- middleSMA+targetDiffDownDay
  for (i in startIndex:nrow(x))
  {
    if(isTrendDown(as.matrix(x[(i-targetDiffDownDay):i])) && x[i] > 0)
    {
      a[i] <- TRUE
    }
      if(x[i] < 0 && x[i-1] > 0)
      {
        a[i] <- TRUE
      }
  }
  return (a) 
}


#e.	当差值是正数并持续变小但不到3天立即又变大，可以用最近变小的天数减去变大的天数，然后跟最近变小的周期天数相加，如果大于5也可以标识为卖点
treat_trendDownPlus <- function(x, targetShortGrowDay=2, targetDiffGrowDay=3, targetDiffDownDay=2) 
{ 
  a <- matrix(FALSE,nrow=nrow(x),ncol=1)
  startIndex <- middleSMA+targetDiffGrowDay+targetDiffDownDay+1
  for (i in startIndex:nrow(x))
  {
    if(isTrendOneDwonCamel(x[(i-targetDiffGrowDay-targetDiffDownDay):i]))
    {
      a[i] <- TRUE
    }
  }
  return (a) 
}

isTrendOneDwonCamel <- function(x ,targetShortDownDay=2, targetDiffDownDay=3, targetDiffUpDay=2)
{
  topIndex <- 1
  topCount <- 0
  lowIndex <- nrow(x)
  lowCount <- 0
  for (i in 1:nrow(x))
  {
	 if((i != 1) 
	    &&(i != nrow(x))
	    && (as.numeric(x[i]) > as.numeric(x[i-1])) 
	    && (as.numeric(x[i]) > as.numeric(x[i+1])))
	 { 
	      topIndex <- i 
		  topCount <- topCount +1
	 }
	 if((i != 1) 
	    &&(i != nrow(x)) 
	    && (as.numeric(x[i]) < as.numeric(x[i-1])) 
	    && (as.numeric(x[i]) < as.numeric(x[i+1])))
	 { 
	    lowIndex <- i 
		  lowCount <- lowCount +1
	 }
  }
  if(topCount ==1
     &&lowCount==1
	 &&x[lowIndex] > 0
	 &&lowIndex < topIndex
	 &&topIndex - lowIndex <= targetDiffUpDay
	 &&nrow(x) - (lowIndex - topIndex) >= targetDiffDownDay
	 &&lowIndex-1 >= targetShortDownDay) { return (TRUE) } else { return (FALSE) }
 
}







