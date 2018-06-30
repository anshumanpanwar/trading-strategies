library(Quandl)
library(httr)
library(tseries)
library(zoo)
library(ggplot2)
library(gridBase)
library(gridGraphics)
library(lubridate)
library(quantmod)
source('heiken-ashi.R')
Quandl.api_key("rAHKcgv_ymH7_o9s5nWN")
corp.action.file<-"D:/Work/Stocks/Corporate_Actions.csv"

prepare.predictiondata.start<-function(ticker, prices, ha, nifty){
  #MACD construction
  macd  <- MACD( prices$adj.close, 12, 26, 9, maType="EMA", percent=TRUE )
  macd$bars<-macd$macd-macd$signal
  macd$bars.firstderivative<-diff(macd$bars)
  macd$bars.secondderivative<-diff(macd$bars.firstderivative)
  macd$bars.firstderivative.5sma<-SMA(macd$bars.firstderivative, n=5)
  macd$bars.secondderivative.3sma<-SMA(macd$bars.secondderivative, n=3)
  macd.zero.signal.gap.threshold<-sd(macd$bars, na.rm=TRUE)/2 #the max gap between macd and signal line to decide crossing
  #few vitals for stock
  stock.beta<-beta(prices, nifty)
  stock.rsquare<-rsquare(prices,nifty)
  stock.ret.var<-(sd(diff(prices$adj.close)/lag(prices$adj.close), na.rm = TRUE) ^2)
  direction<-0  #0 for short, 1 for long
  current.start<-0
  sequence.start<-Sys.Date()
  first.candle.date<-Sys.Date()
  second.candle.date<-Sys.Date()
  sequence.start.index<-0
  i<-1
  header<-c("stock.ticker", "stock.beta", "stock.rsquare", "stock.ret.var", "pattern.start", "pattern.end", "direction", "pos.moves", "neg.moves", "total.moves",
            "seq.return", "pos.percentage", "bollinger.width", "bollinger.percentage","first.candle", "second.candle", "first.candle.stick", "second.candle.stick", 
            "first.bollinger.score", "second.bollinger.score", "first.bollinger.score.stick", "second.bollinger.score.stick", "first.neg.stick", "second.neg.stick", 
            "macd", "macd.bar", "macd.crossing", "macd.zero.signal", "macd.bar.dx", "macd.bar.d2x", "macd.bar.dx.sma", "macd.bar.d2x.sma", "score", "viable")
  result<-data.frame(stringsAsFactors = FALSE)
  
  while(i<=nrow(ha)){
    if(i==1){
      i<-i+1
      next
    }
    # printf("i is %d", i)
    if((as.numeric(ha$Close[i])-as.numeric(ha$Open[i])>0 & ((as.numeric(ha$Close[i-1])-as.numeric(ha$Open[i-1]))>0)) | 
       (as.numeric(ha$Close[i])-as.numeric(ha$Open[i])<0 & ((as.numeric(ha$Close[i-1])-as.numeric(ha$Open[i-1]))<0)) ){
      current.start<-i
      sequence.start<-as.Date(rownames(ha)[i+1])  #first 2 days were indicators, position taken on 3rd day i+1
      date.before.first.candle<-as.Date(rownames(ha)[i-2])
      first.candle.date<-as.Date(rownames(ha)[i-1])
      second.candle.date<-as.Date(rownames(ha)[i])
      sequence.start.index<-i+1
}