#loadStock loads single stock
#Example, 
#stock.folder <- 'C:/important/ideas/stock/stockdata/'
#SH601098<- loadStock(stock.folder, "SH601098","Op")
loadStock <- function(stock.folder, stock.name, operation.name = "Cl")
{
  tmp <- read.csv(paste(stock.folder, stock.name, '.txt', sep=''), sep='\t', check.names=FALSE)
  stock1  <- xts(tmp[,-1],as.Date(tmp[,1],"%Y/%m/%d"))
  if (operation.name == "Op") {
    return (Op(stock1))
  } else if (operation.name == "Hi") {
    return (Hi(stock1))
  } else if (operation.name == "Lo") {
    return (Lo(stock1))
  } else if (operation.name == "Cl") {
    return (Cl(stock1))
  } else if (operation.name == "Vo") {
    return (Vo(stock1))
  } else if (operation.name == "Ad") {
    return (Ad(stock1))
  } else if (operation.name == "OpCl") {
    return (OpCl(stock1))
  } else if (operation.name == "ClCl") {
    return (ClCl(stock1))
  } else if (operation.name == "HiCl") {
    return (HiCl(stock1))
  } else if (operation.name == "LoCl") {
    return (LoCl(stock1))
  } else if (operation.name == "LoHi") {
    return (LoHi(stock1))
  } else if (operation.name == "OpHi") {
    return (OpHi(stock1))
  } else if (operation.name == "OpLo") {
    return (OpLo(stock1))
  } else if (operation.name == "OpOp") {
    return (OpOp(stock1))
  } else if (operation.name == "HLC") {
    return (HLC(stock1))
  } else if (operation.name == "OHLC") {
    return (OHLC(stock1))
  } else if (operation.name == "OHLCV") {
    return (OHLCV(stock1))
  } else {
    return (stock1)
  }
    
}
#loadStock loads multiple stocks
#Example, 
#stock.folder <- 'C:/important/ideas/stock/stockdata/'
#stock_symbols <- c("SH600037","SH600088", "SH600136", "SH600229", "SH600373", "SH600576", "SH600633", "SH600637", "SH600715","SH600757", "SH600825", "SH600831", "SH600959","SH601098", "SH601801", "SH601900", "SH601928", "SH601929","SH601999","SH603598","SH603999")
#entStocks<-loadMultipleStock(stock.folder,stock_symbols)
loadMultipleStock <- function(stock.folder, stock.name.list, operation.name = "Cl")
{
  entStocks <- loadStock(stock.folder, stock.name.list[1])
  for(n in stock.name.list[-1]) {
    entStocks <- cbind(entStocks, loadStock(stock.folder, n, operation.name))
  } 
  return (entStocks)
}

calcuateLogReturn <- function(x)
{
  dx <- na.omit(x)
  dx<-diff(log(x), trim=TRUE)
  dx <- na.omit(dx)
  #dx <- dx[is.finite(dx)]
  return (dx)
}

calcuateSimpleReturn <- function(x)
{
  dx <- na.omit(x)
  dx<-diff(x) / x[-length(x)]
  dx <- na.omit(dx)
  return (dx)
}

calcuateSMA <- function(x, n)
{
  dx <- na.locf(x, na.rm = TRUE)
  dx<-SMA(dx,n)
  return (dx)
}