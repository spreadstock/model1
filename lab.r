#COV(X,Y) = E[(X - E(X))(Y - E(Y))] =  E[XY] - E[X]E[Y]
#COR(X,Y) = COV(X,Y) / COV(X,X), which is slop of xy line 
increaseFun <- function(x) {
  return (x+1)
}

decreaseFun <- function (x) {
  return (x)
}

totalPoint = 12

stockA <- rep.int(10, totalPoint)

changeSize = 8

stockB <- 10
for (i in 1:(totalPoint-1)) {
  sit <- i %% 4
  if (sit == 0) {
    stockB<- c(stockB, (10 + i))
  } else if (sit == 1) {
    stockB <- c(stockB, (10 + changeSize + i))
  } else if (sit == 2) {
    stockB <- c(stockB, (10 + i))
  } else if (sit == 3) {
    stockB <- c(stockB, (10 - changeSize + i))
  } 

}

stockC <- 1
for (i in 1:(totalPoint-1)) {
  stockC <- c(stockC, increaseFun(i) )
}


stockD <- totalPoint
for (i in (totalPoint-1):1) {
  stockD <- c(stockD, decreaseFun(i) )
}




print(paste("stockA mean:", mean(stockA)))
print(paste("stockB mean:", mean(stockB)))
print(paste("stockC mean:", mean(stockC)))
print(paste("stockD mean:", mean(stockD)))
       
#plot stockA
# plot(x=1:totalPoint, y=stockA)
# 
#plot stockB
plot(x=1:totalPoint, y=stockB, type="l")

#plot stockC
#plot(x=1:totalPoint, y=stockC, type="l")

#plot stockD
#plot(x=1:totalPoint, y=stockD, type="l")

#plot stockE
#plot(x=1:totalPoint, y=stockE, type="l")

cov(stockA, stockB)
print(paste("Cov A B:", cov(stockA, stockB)))


cov(stockA, stockC)
print(paste("Cov A C:", cov(stockA, stockC)))

cov(stockA, stockD)
print(paste("Cov A D:", cov(stockA, stockD)))

cov(stockB, stockC)
print(paste("Cov B C:", cov(stockB, stockC)))
print(paste("Cor B C:", cor(stockB, stockC)))


cov(stockB, stockD)
print(paste("Cov B D:", cov(stockB, stockD)))
print(paste("Cor B D:", cor(stockB, stockD)))
