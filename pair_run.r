
qs.strategy <- "qsModel1"
out1 <-applyStrategy(strategy=qs.strategy,portfolios=qs.strategy)
updatePortf(Portfolio=qs.strategy,
            Dates=paste("::", as.Date(Sys.time()), sep=''))
updateAcct(qs.strategy, Dates=paste(startDate, endDate, sep="::"))
updateEndEq(qs.strategy, Dates=paste(startDate, endDate, sep="::"))
getEndEq(qs.strategy, Sys.time())