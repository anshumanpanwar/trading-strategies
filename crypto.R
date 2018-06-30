library(Quandl)
library(httr)
library(tseries)
library(zoo)
library(ggplot2)
library(gridBase)
library(gridGraphics)
library(lubridate)
library(quantmod)
library(TTR)
source('D:/Work/Stocks/util-functions.R')

data.dir<-"D:/Work/Stocks/data/"
Quandl.api_key("rAHKcgv_ymH7_o9s5nWN")


run.strategy<-function(ticker, prices, nifty, start.date, end.date){
  #prices<-normalize.prices(prices)
  #MACD indicator
  macd1  <- get.macd(prices, 12, 26, 9)
  macd1$bars<-macd1$macd-macd1$signal
  macd1$bars.firstderivative<-0
  macd1$bars.firstderivative.sma<-0
  macd<-macd1[paste(start.date, end.date, sep="::"),]
  #ADX indicator
  adx<-ADX(prices[,c("High","Low","Close")], n=14, maType="EMA")
  adx$firstderivative<-diff(adx$ADX)
  adx$firstderivative.sma<-SMA(adx$firstderivative, n=3)
  adx<-adx[paste(start.date, end.date, sep="::"),]
  #ADX indicator - Longer duration
  adx2<-ADX(prices[,c("High","Low","Close")], n=20, maType="EMA")
  adx2<-adx2[paste(start.date, end.date, sep="::"),]
  #SMI indicator
  smi1  <- SMI(prices[,c("High", "Low", "Close")], maType="EMA")
  smi1$bars<- (smi1$SMI-smi1$signal)
  smi<-smi1[paste(start.date, end.date, sep="::"),]
  #RSI indicator
  rsi<-RSI(prices[,c("Close")])
  rsi<-rsi[paste(start.date, end.date, sep="::"),]
  #MACD indicator-Nifty
  nifty.macd  <- get.macd(nifty, 12, 26, 9)
  nifty.macd$bars<-nifty.macd$macd-nifty.macd$signal
  nifty.macd<-nifty.macd[paste(start.date, end.date, sep="::"),]
  #ADX indicator for Index
  nifty.adx<-ADX(nifty[,c("High","Low","Close")], n=14, maType="EMA")
  nifty.adx<-nifty.adx[paste(start.date, end.date, sep="::"),]
  #ADX2 indicator for Index
  nifty.adx2<-ADX(nifty[,c("High","Low","Close")], n=20, maType="EMA")
  nifty.adx2<-nifty.adx2[paste(start.date, end.date, sep="::"),]
  #SMI indicator-nifty
  nifty.smi  <- SMI(nifty[,c("High", "Low", "Close")], maType="EMA")
  nifty.smi<-nifty.smi[paste(start.date, end.date, sep="::"),]
  
  automata.state<-0 #0 mean searching for signal, 1 mean into the trade
  direction<-1  #1 means uptrend, 0 means downtrend
  
  prices2<-prices[paste(start.date, end.date, sep="::"),]
  start.index<-1
  seq.start.date<-NULL
  end.index<-i
  result<-data.frame(stringsAsFactors = FALSE)
  # i<-max(min(which(!is.na(macd$bars))), min(which(!is.na(adx$ADX))))  #first Non NA macd$bar or adx$ADX otherwise fails for new stocks like ICICIPRULIFE
  i<-1
  drawdown<-0
  macdbar.high<-0
  flag<-FALSE
  check.macd.level<-FALSE #check MACD level compared to zero line for entering a trade
  check.macd.threshold<-FALSE  #MACD shud be above/below a threshold to enter a trade
  check.adx.level<-FALSE   #Check adx level for no inverse trend
  check.macdbar.exit<-FALSE  #MACD bar shud cross the threshold at least once to exit the trend
  drawdown.exit<-FALSE    #Exit on max drawdown
  macd.threshold<-0
  adx.threshold<-25
  macdbar.exit.threshold<-0.5
  drawdown.threshold<-0.1   #exit at this drawdown
  dirty.exit<-FALSE
  predictive.exit<-FALSE #Predict when macd bar is going to 0 and exit
  predictive.exit.lookahead<-4
  exit.at.top<-FALSE
  
  
  #few vitals for stock
  stock.vitals.lookback<-100
  nifty.vol.lookback<-30
  # temp.prices<-prices[paste(as.character(index(prices)[1]), as.character(cutoff.date), sep="::"),]
  # temp.nifty<-nifty[paste(as.character(index(prices)[1]), as.character(cutoff.date), sep="::"),]
  # # temp.prices
  # # temp.nifty
  # stock.beta<-beta(temp.prices, temp.nifty)
  # stock.rsquare<-rsquare(temp.prices,temp.nifty)
  # stock.ret.var<-(sd(diff(temp.prices$adj.close)/lag(temp.prices$adj.close), na.rm = TRUE) ^2)
  
  last.ret<-0
  last.seq<-0
  
  while(i<=nrow(macd)-2){   #MACD crossing takes place at close of -1 and trade closes at open of 0
    if(automata.state==1){
      if(direction==1){
        dd<-(as.numeric(prices2$Open[start.index+1]) - as.numeric(prices2$Close[i]))/as.numeric(prices2$Open[start.index+1])
        if(dd>drawdown) {drawdown<-dd}
      }
      else if(direction==0){
        dd<-(as.numeric(prices2$Close[i]) - as.numeric(prices2$Open[start.index+1]))/as.numeric(prices2$Open[start.index+1])
        if(dd>drawdown) {drawdown<-dd}
      }
      if(abs(ifelse(is.na(as.numeric(macd$bars[i])), 0 ,as.numeric(macd$bars[i])))  > macdbar.high) {macdbar.high<-abs(ifelse(is.na(as.numeric(macd$bars[i])), 0 ,as.numeric(macd$bars[i]))) }
      if(drawdown.exit){  #exit on drawdown
        if(drawdown>drawdown.threshold){
          if(direction==1){
            automata.state<-0
            end.index<-i
            i<- (i-1)  #Decrease i as this crossing potentially can also be the start of new pattern
            ret<-(as.numeric(prices2$Open[end.index+1]) - as.numeric(prices2$Open[start.index+1])) / as.numeric(prices2$Open[start.index+1])
            cons.ret<- (as.numeric(prices2$Low[end.index+1]) - as.numeric(prices2$High[start.index+1])) / as.numeric(prices2$High[start.index+1])   #bought high for uptrend and sold low
            temp.row<-list(ticker, as.character(index(macd)[start.index]), as.character(index(macd)[end.index]), direction, (end.index-start.index), ret, cons.ret)
            result<-rbind.data.frame(result, temp.row, stringsAsFactors=FALSE)
          }
          else if(direction==0){
            automata.state<-0
            end.index<-i
            i<- (i-1)  #Decrease i as this crossing potentially can also be the start of new pattern
            ret<- -(as.numeric(prices2$Open[end.index+1]) - as.numeric(prices2$Open[start.index+1])) / as.numeric(prices2$Open[start.index+1])  #return are negated as its downtrend
            cons.ret<- -(as.numeric(prices2$High[end.index+1]) - as.numeric(prices2$Low[start.index+1])) / as.numeric(prices2$Low[start.index+1])   #Sold low for downtrend and bought high
            temp.row<-list(ticker, as.character(index(macd)[start.index]), as.character(index(macd)[end.index]), direction, (end.index-start.index), ret, cons.ret)
            result<-rbind.data.frame(result, temp.row, stringsAsFactors=FALSE)
          }
          printf("drawdown %f is higher than threshold %f, so shud exit right now %s", drawdown, drawdown.threshold, as.Date(index(macd)[i]))
        }
      }
      if(predictive.exit){
        if(direction == 1 & (i-start.index)>3 & macd$bars[i]>0 & (macd$bars[i] + macd$bars.firstderivative.sma[i]*predictive.exit.lookahead)<0){
          printf("MACD level is %f and slope %f and direction %d so exiting early %s",macd$bars[i], macd$bars.firstderivative.sma[i], direction, as.character(index(macd)[i+1]))
          automata.state<-0
          end.index<-i+1
          ret<-(as.numeric(prices2$Open[end.index+1]) - as.numeric(prices2$Open[start.index+1])) / as.numeric(prices2$Open[start.index+1])
          cons.ret<- (as.numeric(prices2$Low[end.index+1]) - as.numeric(prices2$High[start.index+1])) / as.numeric(prices2$High[start.index+1])   #bought high for uptrend and sold low
          
          temp.row<-list(ticker, as.character(index(macd)[start.index]), as.character(index(macd)[end.index]), direction, (end.index-start.index), ret, cons.ret)
          result<-rbind.data.frame(result, temp.row, stringsAsFactors=FALSE)
          last.ret<-ret
          last.seq<-(end.index-start.index)
        }
        else if(direction == 0 & (i-start.index)>3 & macd$bars[i]<0 & (macd$bars[i] + macd$bars.firstderivative.sma[i]*predictive.exit.lookahead)>0){
          printf("MACD level is %f and slope %f and direction %d so exiting early %s",macd$bars[i], macd$bars.firstderivative.sma[i], direction, as.character(index(macd)[i+1]))
          automata.state<-0
          end.index<-i+1
          ret<- -(as.numeric(prices2$Open[end.index+1]) - as.numeric(prices2$Open[start.index+1])) / as.numeric(prices2$Open[start.index+1])  #return are negated as its downtrend
          cons.ret<- -(as.numeric(prices2$High[end.index+1]) - as.numeric(prices2$Low[start.index+1])) / as.numeric(prices2$Low[start.index+1])   #Sold low for downtrend and bought high
          
          temp.row<-list(ticker, as.character(index(macd)[start.index]), as.character(index(macd)[end.index]), direction, (end.index-start.index), ret, cons.ret)          
          result<-rbind.data.frame(result, temp.row, stringsAsFactors=FALSE)
          last.ret<-ret
          last.seq<-(end.index-start.index)
        }
      }
      if(exit.at.top){
        printf("MACD level is %f and next level  %f and direction %d index %s i %d start.index %d",macd$bars[i], macd$bars[i+1], direction, as.character(index(macd)[i+1]), i, start.index)
        if(direction == 1 & (i-start.index)>3 & (as.numeric(macd$bars[i])>as.numeric(macd$bars[i+1]))){
          printf("MACD level is %f and next level  %f and direction %d so exiting early at top %s",macd$bars[i], macd$bars[i+1], direction, as.character(index(macd)[i+1]))
          automata.state<-0
          end.index<-i+1
          bollinger.sd.value<-bollinger.sd(prices[2:nrow(prices),], as.Date(index(prices2)[start.index]), 20 )
          #Bollinger width in terms of percentage
          bollinger.width<-bollinger.sd.value/bollinger.ma(prices[2:nrow(prices),], as.Date(index(prices2)[start.index]), 20)
          bollinger.percentage<-bollinger.sd.value/bollinger.sd(prices[2:nrow(prices),], as.Date(index(prices2)[start.index]), 150 )
          ret<-(as.numeric(prices2$Open[end.index+1]) - as.numeric(prices2$Open[start.index+1])) / as.numeric(prices2$Open[start.index+1])
          cons.ret<- (as.numeric(prices2$Low[end.index+1]) - as.numeric(prices2$High[start.index+1])) / as.numeric(prices2$High[start.index+1])   #bought high for uptrend and sold low
          # Stock vitals
          temp.prices<-prices[paste(as.character(seq.start.date-stock.vitals.lookback), as.character(seq.start.date), sep="::"),]
          temp.nifty<-nifty[paste(as.character(seq.start.date-stock.vitals.lookback), as.character(seq.start.date), sep="::"),]
          # temp.prices
          # temp.nifty
          stock.beta<-beta(temp.prices, temp.nifty)
          stock.rsquare<-rsquare(temp.prices,temp.nifty)
          stock.ret.var<-(sd(diff(temp.prices$adj.close)/lag(temp.prices$adj.close), na.rm = TRUE) ^2)
          temp.nifty2<-nifty[paste(as.character(seq.start.date-nifty.vol.lookback), as.character(seq.start.date), sep="::"),]
          market.vol<-(sd(diff(temp.nifty2$Close)/lag(temp.nifty2$Close), na.rm = TRUE) ^2)
          
          temp.row<-list(ticker, stock.beta, stock.rsquare, stock.ret.var, as.character(index(macd)[start.index]), as.character(index(macd)[end.index]), direction, dirty.exit, (end.index-start.index), ret, cons.ret, last.ret, last.seq, drawdown, 
                         -macd$macd[start.index], macd$bars.firstderivative.sma[start.index], adx$ADX[start.index]*ifelse(adx$DIn[start.index] < adx$DIp[start.index], 1, -1), adx$firstderivative.sma[start.index]*ifelse(adx$DIn[start.index] < adx$DIp[start.index], 1, -1), adx2$ADX[start.index]*ifelse(adx2$DIn[start.index] < adx2$DIp[start.index], 1, -1),
                         -smi$SMI[start.index], rsi$EMA[start.index], bollinger.width, bollinger.percentage, market.vol, -nifty.macd$macd[start.index], nifty.macd$bars[start.index], -nifty.smi$SMI[start.index],
                         nifty.adx$ADX[start.index]*ifelse(nifty.adx$DIn[start.index] < nifty.adx$DIp[start.index], 1, -1), stock.beta*nifty.adx$ADX[start.index]*ifelse(nifty.adx$DIn[start.index] < nifty.adx$DIp[start.index], 1, -1), 
                         stock.rsquare*nifty.adx$ADX[start.index]*ifelse(nifty.adx$DIn[start.index] < nifty.adx$DIp[start.index], 1, -1), nifty.adx2$ADX[start.index]*ifelse(nifty.adx2$DIn[start.index] < nifty.adx2$DIp[start.index], 1, -1), stock.beta*nifty.adx2$ADX[start.index]*ifelse(nifty.adx2$DIn[start.index] < nifty.adx2$DIp[start.index], 1, -1), 
                         stock.rsquare*nifty.adx2$ADX[start.index]*ifelse(nifty.adx2$DIn[start.index] < nifty.adx2$DIp[start.index], 1, -1),ifelse((ret+cons.ret)>0,1,0))
          result<-rbind.data.frame(result, temp.row, stringsAsFactors=FALSE)
          last.ret<-ret
          last.seq<-(end.index-start.index)
        }
        else if(direction == 0 & (i-start.index)>3 & (as.numeric(macd$bars[i])<as.numeric(macd$bars[i+1]))){
          printf("MACD level is %f and next level  %f and direction %d so exiting early at top %s",macd$bars[i], macd$bars[i+1], direction, as.character(index(macd)[i+1]))
          automata.state<-0
          end.index<-i+1
          bollinger.sd.value<-bollinger.sd(prices[2:nrow(prices),], as.Date(index(prices2)[start.index]), 20 )
          #Bollinger width in terms of percentage
          bollinger.width<-bollinger.sd.value/bollinger.ma(prices[2:nrow(prices),], as.Date(index(prices2)[start.index]), 20)
          bollinger.percentage<-bollinger.sd.value/bollinger.sd(prices[2:nrow(prices),], as.Date(index(prices2)[start.index]), 150 )
          ret<- -(as.numeric(prices2$Open[end.index+1]) - as.numeric(prices2$Open[start.index+1])) / as.numeric(prices2$Open[start.index+1])  #return are negated as its downtrend
          cons.ret<- -(as.numeric(prices2$High[end.index+1]) - as.numeric(prices2$Low[start.index+1])) / as.numeric(prices2$Low[start.index+1])   #Sold low for downtrend and bought high
          #Stock vitals
          temp.prices<-prices[paste(as.character(seq.start.date-stock.vitals.lookback), as.character(seq.start.date), sep="::"),]
          temp.nifty<-nifty[paste(as.character(seq.start.date-stock.vitals.lookback), as.character(seq.start.date), sep="::"),]
          # temp.prices
          # temp.nifty
          stock.beta<-beta(temp.prices, temp.nifty)
          stock.rsquare<-rsquare(temp.prices,temp.nifty)
          stock.ret.var<-(sd(diff(temp.prices$adj.close)/lag(temp.prices$adj.close), na.rm = TRUE) ^2)
          temp.nifty2<-nifty[paste(as.character(seq.start.date-nifty.vol.lookback), as.character(seq.start.date), sep="::"),]
          market.vol<-(sd(diff(temp.nifty2$Close)/lag(temp.nifty2$Close), na.rm = TRUE) ^2)
          
          temp.row<-list(ticker, stock.beta, stock.rsquare, stock.ret.var, as.character(index(macd)[start.index]), as.character(index(macd)[end.index]), direction, dirty.exit, (end.index-start.index), ret, cons.ret, last.ret, last.seq, drawdown, 
                         macd$macd[start.index], -macd$bars.firstderivative.sma[start.index], adx$ADX[start.index]*ifelse(adx$DIn[start.index] > adx$DIp[start.index], 1, -1), adx$firstderivative.sma[start.index]*ifelse(adx$DIn[start.index] > adx$DIp[start.index], 1, -1), adx2$ADX[start.index]*ifelse(adx2$DIn[start.index] > adx2$DIp[start.index], 1, -1),
                         smi$SMI[start.index], 100-rsi$EMA[start.index], bollinger.width, bollinger.percentage, market.vol, nifty.macd$macd[start.index], -nifty.macd$bars[start.index], nifty.smi$SMI[start.index],
                         nifty.adx$ADX[start.index]*ifelse(nifty.adx$DIn[start.index] > nifty.adx$DIp[start.index], 1, -1), stock.beta*nifty.adx$ADX[start.index]*ifelse(nifty.adx$DIn[start.index] > nifty.adx$DIp[start.index], 1, -1), 
                         stock.rsquare*nifty.adx$ADX[start.index]*ifelse(nifty.adx$DIn[start.index] > nifty.adx$DIp[start.index], 1, -1), nifty.adx2$ADX[start.index]*ifelse(nifty.adx2$DIn[start.index] > nifty.adx2$DIp[start.index], 1, -1), stock.beta*nifty.adx2$ADX[start.index]*ifelse(nifty.adx2$DIn[start.index] > nifty.adx2$DIp[start.index], 1, -1), 
                         stock.rsquare*nifty.adx2$ADX[start.index]*ifelse(nifty.adx2$DIn[start.index] > nifty.adx2$DIp[start.index], 1, -1),ifelse((ret+cons.ret)>0,1,0))
          result<-rbind.data.frame(result, temp.row, stringsAsFactors=FALSE)
          last.ret<-ret
          last.seq<-(end.index-start.index)
        }
      }
    }
    if(automata.state==0 & as.numeric(macd$bars[i])<0 & as.numeric(macd$bars[i+1])>0) {  #MACD crosses from below
      flag<-TRUE
      if(check.macd.level & ! as.numeric(macd$macd[i+1])<0){flag<-FALSE}  #MACD Shud be <0
      if(check.macd.threshold & ! abs(as.numeric(macd$macd[i+1]))>macd.threshold){flag<-FALSE}
      if(check.adx.level & as.numeric(adx$ADX[i+1])>adx.threshold & as.numeric(adx$DIn[i+1])>as.numeric(adx$DIp[i+1])) {flag<-FALSE} #strong negative trend
      if(flag){
        printf("starting trend %s , direction %d",as.Date(index(macd)[i]), 1)
        direction<-1
        automata.state<-1
        start.index<-i+1
        seq.start.date<-as.Date(index(prices2)[start.index])
        drawdown<-0
        macdbar.high<-0
        dirty.exit<-FALSE
      }
    }
    else if(automata.state==0 & as.numeric(macd$bars[i])>0 & as.numeric(macd$bars[i+1])<0) {  #MACD crosses from above
      flag<-TRUE
      if(check.macd.level & ! as.numeric(macd$macd[i+1])>0){flag<-FALSE}  #MACD Shud be >0
      if(check.macd.threshold & ! abs(as.numeric(macd$macd[i+1]))>macd.threshold){flag<-FALSE}
      if(check.adx.level & as.numeric(adx$ADX[i+1])>adx.threshold & as.numeric(adx$DIn[i+1]) < as.numeric(adx$DIp[i+1])) {flag<-FALSE} #strong positive trend
      if(flag){
        printf("starting trend %s , direction %d",as.Date(index(macd)[i]), 0)
        direction<-0
        automata.state<-1
        start.index<-i+1
        seq.start.date<-as.Date(index(prices2)[start.index])
        drawdown<-0
        macdbar.high<-0
        dirty.exit<-FALSE
      }
    }
    else if(automata.state==1  & direction ==1 & as.numeric(macd$bars[i])>0 & as.numeric(macd$bars[i+1])<0) {  #MACD crosses from above, closing signal for uptrend
      flag<-TRUE
      macdbar.sd<-sd(macd1[paste(max(as.Date(index(macd)[i])-200, as.Date(index(macd1)[1])), as.Date(index(macd)[i]), sep="::"),]$bars, na.rm=TRUE)  #sd for max last 200 days macd
      macdbar.sd<-sd(abs(macd1[paste(as.Date(index(macd1)[1]), as.character(cutoff.date), sep="::"),]$bars), na.rm=TRUE)
      if(check.macdbar.exit){
        if(! (macdbar.high>macdbar.sd * macdbar.exit.threshold)) { #Dont early exit
          flag<-FALSE
          printf("MACD bar high %f is lower than sd %f, so not exiting right now %s", macdbar.high, macdbar.sd*macdbar.exit.threshold, as.Date(index(macd)[i]))
          dirty.exit<-TRUE
        }
        else{
          printf("MACD bar high %f is higher than sd %f, so shud exit right now %s", macdbar.high, macdbar.sd*macdbar.exit.threshold, as.Date(index(macd)[i]))
        }
      }
      
      if(flag){
        automata.state<-0
        end.index<-i+1
        i<-i-1
        ret<-(as.numeric(prices2$Close[end.index]) - as.numeric(prices2$Close[start.index])) / as.numeric(prices2$Close[start.index])
        cons.ret<- (as.numeric(prices2$Low[end.index]) - as.numeric(prices2$High[start.index])) / as.numeric(prices2$High[start.index])   #bought high for uptrend and sold low
        
        temp.row<-list(ticker, as.character(index(macd)[start.index]), as.character(index(macd)[end.index]), direction, (end.index-start.index), ret, cons.ret)
        result<-rbind.data.frame(result, temp.row, stringsAsFactors=FALSE)
        last.ret<-ret
        last.seq<-(end.index-start.index)
      }
    }
    else if(automata.state==1 & direction==0 & as.numeric(macd$bars[i])<0 & as.numeric(macd$bars[i+1])>0) {  #MACD crosses from below, closing signal for downtrend
      flag<-TRUE
      macdbar.sd<-sd(macd1[paste(max(as.Date(index(macd)[i])-200, as.Date(index(macd1)[1])), as.Date(index(macd)[i]), sep="::"),]$bars, na.rm=TRUE)  #sd for max last 200 days macd
      # macdbar.sd<-sd(abs(macd1[paste(as.Date(index(macd1)[1]), as.character(cutoff.date), sep="::"),]$bars), na.rm=TRUE)
      if(check.macdbar.exit){
        if(! (macdbar.high>macdbar.sd * macdbar.exit.threshold)) { #Dont early exit
          flag<-FALSE
          printf("MACD bar high %f is lower than sd %f, so not exiting right now %s", macdbar.high, macdbar.sd*macdbar.exit.threshold, as.Date(index(macd)[i]))
          dirty.exit<-TRUE
        }
        else{
          printf("MACD bar high %f is higher than sd %f, so shud exit right now %s", macdbar.high, macdbar.sd*macdbar.exit.threshold, as.Date(index(macd)[i]))
        }
      }
      if(flag){
        automata.state<-0
        end.index<-i+1
        i<-i-1
        ret<- -(as.numeric(prices2$Close[end.index]) - as.numeric(prices2$Close[start.index])) / as.numeric(prices2$Close[start.index])  #return are negated as its downtrend
        cons.ret<- -(as.numeric(prices2$High[end.index]) - as.numeric(prices2$Low[start.index])) / as.numeric(prices2$Low[start.index])   #Sold low for downtrend and bought high
        
        temp.row<-list(ticker, as.character(index(macd)[start.index]), as.character(index(macd)[end.index]), direction, (end.index-start.index), ret, cons.ret)
        result<-rbind.data.frame(result, temp.row, stringsAsFactors=FALSE)
        last.ret<-ret
        last.seq<-(end.index-start.index)
      }
    }
    if(i==(nrow(macd)-2) & automata.state==1){  #capture ongoing trend
      automata.state<-0
      end.index<-i+2
      ret<-(as.numeric(prices2$Open[end.index]) - as.numeric(prices2$Open[start.index+1])) / as.numeric(prices2$Open[start.index+1])
      cons.ret<- 0
      if(direction==0){
        ret<- -ret
        cons.ret<- -cons.ret
      }
      
      if(direction==0){
        temp.row2<-list(ticker, as.character(index(macd)[start.index]), as.character(index(macd)[end.index]), direction, (end.index-start.index), ret, cons.ret)
      }
      if(direction==1){
        temp.row2<-list(ticker, as.character(index(macd)[start.index]), as.character(index(macd)[end.index]), direction, (end.index-start.index), ret, cons.ret)
      }
      ongoing.trend<<-rbind.data.frame(ongoing.trend, temp.row2, stringsAsFactors=FALSE)
      # i<- start.index  #Decrease i as this crossing potentially can also be the start of new pattern
    }
    i<-(i+1)
  }
  header<-c("stock.ticker", "start.date", "end.date", "direction",  "seq length", "returns", "Cons-Return")
  if(nrow(result)>0){colnames(result)<-header}
  if(nrow(ongoing.trend)>0){colnames(ongoing.trend)<<-header}
  return(result)
}

library(readxl)
file.loc<-"D:/Work/Stocks/crypto.xlsx"
tickers_100<-c('Bitcoin','Litecoin','Ethereum')
lookback<-500
additional.lookback<-100
nifty <-as.data.frame(read_excel(file.loc, sheet = "Bitcoin"))
nifty$Date<-as.POSIXct(strptime(nifty$Date, "%b %d, %Y"))
nifty$adj.close<-nifty$Close
nifty <- zoo(nifty[,-1], order.by=nifty[,1])
output<-data.frame(stringsAsFactors = FALSE)
output2<-data.frame(stringsAsFactors = FALSE)

for(i in 1:length(tickers_100)){
  ongoing.trend<-data.frame(stringsAsFactors = FALSE)
  price<-as.data.frame(read_excel(file.loc, sheet = i))
  price$Date<-as.POSIXct(strptime(price$Date, "%b %d, %Y"))
  price$adj.close<-price$Close
  price <- zoo(price[,-1], order.by=price[,1])
  result<-run.strategy(tickers_100[i], price, nifty, Sys.Date() - lookback, Sys.Date()-1)
  output<-rbind.data.frame(output, result,stringsAsFactors = FALSE)
  output2<-rbind.data.frame(output2, ongoing.trend,stringsAsFactors = FALSE)
}  

write.table(output, file = paste("D://Work/Stocks/crypto-", "results-simple-macd",".csv", sep=""), append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
write.table(output2, file = paste("D://Work/Stocks/crypto-", "ongoing-simple-macd",".csv", sep=""), append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)

cutoff.date<-"2017-09-30"
training<-output[output$end.date<cutoff.date,]
testing<-output[output$start.date>cutoff.date,]
write.table(training[training$last.seq>0,], "D://Work/Stocks/crypto-train1.csv",append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
write.table(testing, "D://Work/Stocks/crypto-test1.csv",append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
