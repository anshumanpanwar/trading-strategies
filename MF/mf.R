library(Quandl)
library(devtools)
library(httr)
library(RCurl)
library(quantmod)
library(tseries)
library(zoo)


Quandl.api_key("rAHKcgv_ymH7_o9s5nWN")

code.file<-"C:/Work/trading-strategies/MF/mf.csv"
output.file<-"C:/Work/trading-strategies/MF/mf-returns2.csv"
codes<-read.csv(code.file, header = TRUE)
start_date<-'2016-09-01'
end_date<-'2018-11-25'

alpha.beta<-function(prices, nifty){
  if(nrow(prices)<2){return(NA)}
  # nifty <-nifty[paste(index(prices)[1],index(prices)[length(index(prices))], sep="::"),]
  nifty<-nifty[index(prices),]
  prices<-prices[index(nifty),]
  ret.prices<-diff(prices[,1])/lag(prices[,1])
  ret.nifty<-diff(nifty$Close)/lag(nifty$Close)
  ret.prices<-ret.prices[!is.na(ret.prices[,1]),]
  ret.nifty<-ret.nifty[!is.na(ret.nifty[,1]),]
  if(nrow(ret.prices)<nrow(ret.nifty)){ret.nifty<-tail(ret.nifty,nrow(ret.prices))}
  else if(nrow(ret.nifty)<nrow(ret.prices)){ret.prices<-tail(ret.prices,nrow(ret.nifty))}
  if(nrow(ret.prices)<=2){return(1)}
  return(as.numeric(coef(lm(ret.prices[2:nrow(ret.prices)]~ret.nifty[2:nrow(ret.nifty)]))))
}

alpha.beta.monthly<-function(prices, nifty){
  if(nrow(prices)<2){return(NA)}
  # nifty <-nifty[paste(index(prices)[1],index(prices)[length(index(prices))], sep="::"),]
  nifty<-nifty[index(prices),]
  prices<-prices[index(nifty),]
  ret.prices<-monthlyReturn(prices[,1], subset=paste(start_date, end_date, sep = "::"), type = 'log', leading=FALSE)
  ret.nifty<-monthlyReturn(nifty[,"Close"], subset=paste(start_date, end_date, sep = "::"), type = 'log', leading=FALSE)
  ret.prices<-ret.prices[!is.na(ret.prices[,1]),]
  ret.nifty<-ret.nifty[!is.na(ret.nifty[,1]),]
  if(nrow(ret.prices)<nrow(ret.nifty)){ret.nifty<-tail(ret.nifty,nrow(ret.prices))}
  else if(nrow(ret.nifty)<nrow(ret.prices)){ret.prices<-tail(ret.prices,nrow(ret.nifty))}
  if(nrow(ret.prices)<=2){return(1)}
  return(as.numeric(coef(lm(ret.prices[2:nrow(ret.prices)]~ret.nifty[2:nrow(ret.nifty)]))))
}

nifty<-Quandl("NSE/CNX_NIFTY", start_date=start_date, end_date=end_date, type='xts')
nifty.returns<-monthlyReturn(nifty[,"Close"], subset=paste(start_date, end_date, sep = "::"), type = 'log', leading=FALSE)
nifty.returns$Close<-nifty.returns$monthly.returns

results<- data.frame(stringsAsFactors = FALSE)
for(i in 1:nrow(codes)){
  print(i)
  prices<-Quandl(paste("AMFI/", codes[i,"Regular"],sep=""), start_date=Sys.Date()-1200, end_date=Sys.Date(), type="xts")
  returns<-monthlyReturn(prices[,1], subset=paste(start_date, end_date, sep = "::"), type = 'log', leading=FALSE)
  avg<-mean(returns$'monthly.returns', na.rm=TRUE)
  stdev1<-sd(returns$'monthly.returns', na.rm=TRUE)
  sharpe<-(avg-log(1+0.07/12))/stdev1
  mar<-log(1+0.07/12)
  avg<-mean(returns$'monthly.returns', na.rm=TRUE)
  stdev2<-sqrt(sum((mar-returns$'monthly.returns'[returns$'monthly.returns'<mar])^2, na.rm=TRUE)/nrow(returns[!is.na(returns$'monthly.returns'),]))
  sortino<-(avg-mar)/stdev2
  pos<-nrow(returns$'monthly.returns'[returns$'monthly.returns'>=mar])
  neg<-nrow(returns$'monthly.returns'[returns$'monthly.returns'<mar])
  alpha.and.beta<-alpha.beta(prices[paste(start_date, end_date, sep = "::")], nifty)
  monthly.alpha.and.beta<-alpha.beta.monthly(prices[paste(start_date, end_date, sep = "::")], nifty)
  results<-rbind(results, list(codes[i,"Fund"], codes[i,"Regular"], avg*12, stdev1*(12^0.5), sharpe*(12^0.5), sortino*(12^0.5), pos, neg, alpha.and.beta[1]*240, alpha.and.beta[2], monthly.alpha.and.beta[1]*12, monthly.alpha.and.beta[2], as.numeric(lapply(returns, tail, 1))), stringsAsFactors = FALSE)
}
colnames(results)<-c('Fund', 'Code', 'Avg', 'Stdev', 'Sharpe', 'Sortino', 'Pos', 'Neg', 'alpha', 'beta', 'alpha-monthly', 'beta-monthly', 'last-month' )
write.csv(results, file=output.file, row.names = FALSE, col.names = TRUE)