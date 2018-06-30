library(Quandl)
library(devtools)
library(httr)
library(RCurl)
library(quantmod)
library(tseries)
library(zoo)

Quandl.api_key("rAHKcgv_ymH7_o9s5nWN")

code.file<-"D:/Work/Stocks/MF/mf-rsi.csv"
output.file<-"D:/Work/Stocks/MF/mf-rsi-results3-14days.csv"
codes<-read.csv(code.file, header = TRUE, stringsAsFactors = FALSE)

results<- data.frame(stringsAsFactors = FALSE)
for(i in 1:nrow(codes)){
  prices<-Quandl(codes[i,"code"], start_date=Sys.Date()-1000, end_date=Sys.Date(), type="xts")
  col<-ifelse(grepl("AMFI", codes[i,"code"]), "Net Asset Value", "Close")
  rsi<-RSI(prices[,col])
  results<-rbind(results, list(codes[i,"Fund"], as.numeric(rsi[nrow(rsi),1])), stringsAsFactors = FALSE)
}
colnames(results)<-c('Fund', 'RSI')
write.csv(results, file=output.file, row.names = FALSE, col.names = TRUE)