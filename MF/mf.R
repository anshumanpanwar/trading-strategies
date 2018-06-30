library(Quandl)
library(devtools)
library(httr)
library(RCurl)
library(quantmod)
library(tseries)
library(zoo)


Quandl.api_key("rAHKcgv_ymH7_o9s5nWN")

code.file<-"D:/Work/Stocks/mf.csv"
output.file<-"D:/Work/Stocks/mf-returns2.csv"
codes<-read.csv(code.file, header = TRUE)

results<- data.frame(stringsAsFactors = FALSE)
for(i in 1:nrow(codes)){
  prices<-Quandl(paste("AMFI/", codes[i,"Direct"],sep=""), start_date=Sys.Date()-1000, end_date=Sys.Date(), type="xts")
  returns<-monthlyReturn(prices,subset='2016-11:30::2018-01-31',type = 'log', leading=FALSE)
  avg<-mean(returns$'monthly.returns', na.rm=TRUE)
  stdev1<-sd(returns$'monthly.returns', na.rm=TRUE)
  sharpe<-(avg-log(1+0.07/12))/stdev1
  mar<-log(1+0.07/12)
  avg<-mean(returns$'monthly.returns', na.rm=TRUE)
  stdev2<-sqrt(sum((mar-returns$'monthly.returns'[returns$'monthly.returns'<mar])^2, na.rm=TRUE)/nrow(returns[!is.na(returns$'monthly.returns'),]))
  sortino<-(avg-mar)/stdev2
  pos<-nrow(returns$'monthly.returns'[returns$'monthly.returns'>=mar])
  neg<-nrow(returns$'monthly.returns'[returns$'monthly.returns'<mar])
  results<-rbind(results, list(codes[i,"Fund"], codes[i,"Direct"], avg*12, stdev1*(12^0.5), sharpe*(12^0.5), sortino*(12^0.5), pos, neg), stringsAsFactors = FALSE)
}
colnames(results)<-c('Fund', 'Code', 'Avg', 'Stdev', 'Sharpe', 'Sortino', 'Pos', 'Neg' )
write.csv(results, file=output.file, row.names = FALSE, col.names = TRUE)