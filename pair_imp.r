calculate_spread <- function(x) {
  
  dx <- x
  
  dxx <- dx[1:nrow(dx)]
  dxLast <- dx[nrow(dx)]
  
  aLm <- lm(dxx$y ~ dxx$x - 1, data=as.data.frame(dxx))
  betas <- coef(aLm)[1]
  yValue <- coredata(dxLast$y)
  xValue <- coredata(dxLast$x)
  spreadValue <- as.numeric(yValue) - betas * as.numeric(xValue)
  spreadValue <- c(spreadValue, betas)
  
  return (spreadValue)
}

rollingV2_1 <- function(x, width, FUN, PREFUN) {
  
  xIn <- x
  timeList <- index(x)
  
  
  rollingResultList <- matrix(NA,nrow=(width - 1), ncol= 2);
  
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
      colnames(firstWindow) <- c("x", "y")
      rollingResult <- FUN(firstWindow)
      
      rollingResultList <- rbind(rollingResultList, rollingResult)

    }
    
  }
  
  
  #add time line
  rollingResultTimed <- xts(x=rollingResultList, order.by=timeList)
  colnames(rollingResultTimed) <- c("Spread","Beta")
  rollingResultTimedUpper <- mean(rollingResultTimed$Spread, na.rm = TRUE) + sd(rollingResultTimed$Spread, na.rm = TRUE)
  rollingResultTimedLower <- mean(rollingResultTimed$Spread, na.rm = TRUE) - sd(rollingResultTimed$Spread, na.rm = TRUE)
  rollingResultTimed$upper <- rollingResultTimedUpper
  rollingResultTimed$lower <- rollingResultTimedLower
  
  colnames(rollingResultTimed) <- c("Spread","Beta" ,"Upper","Lower")
  
  #mktdataTimed <- xts (order.by = index(xIn))
  #rollingResultTimed <- merge(mktdataTimed, rollingResultTimed, all=FALSE, join = 'left')
  
  return (rollingResultTimed)
  #return (rollingResultList)
}