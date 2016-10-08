
# #qs.strategy <- "multitrend"
# qs.strategy <- "qsModel1"
# #enable.rule(qs.strategy,type="chain",labe="StopLoss")
# out1 <-applyStrategy(strategy=qs.strategy,portfolios=qs.strategy)
# updatePortf(Portfolio=qs.strategy,
#             Dates=paste("::", as.Date(Sys.time()), sep=''))
# updateAcct(qs.strategy, Dates=paste(startDate, endDate, sep="::"))
# updateEndEq(qs.strategy, Dates=paste(startDate, endDate, sep="::"))
# getEndEq(qs.strategy, Sys.time())

#qs.strategy <- "qsModel1"
qs.strategy <- "multitrend"
start_t<-Sys.time()
out<-reblanceImp(qs.strategy , portfolios=qs.strategy,verbose=TRUE)
end_t<-Sys.time()
print(end_t-start_t)

start_t<-Sys.time()
updatePortf(Portfolio=qs.strategy,Dates=paste('::',as.Date(Sys.time()),sep=''))
end_t<-Sys.time()
print("trade blotter portfolio update:")
print(end_t-start_t)
updateAcct(qs.strategy, Dates=paste(startDate, endDate, sep="::"))
updateEndEq(qs.strategy, Dates=paste(startDate, endDate, sep="::"))