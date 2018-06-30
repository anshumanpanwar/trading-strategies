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

entry.threhold1<-30
entry.threhold2<-70   #exit at 50%+- exit percentage
exit.thresold1<-50
exit.threhold2<-50
# rsi.lookback<-200

run.strategy<-function(ticker, prices, nifty, nifty.adx, nifty.smi, start.date, end.date){
  prices2<-normalize.prices(prices)
  printf("prices2 length %f", nrow(prices2))
  printf("startdate %s, end date %s ",start.date, end.date)
  rsi  <- RSI(prices[,c("Close")], maType="EMA")
  i<-match(getNextWeekDay(as.Date(start.date)), index(prices2))
  # i<-max(1,ifelse(is.na(match(getNextWeekDay(as.Date(start.date)), index(prices2))), 2,match(as.Date(start.date), index(prices2))))
  printf(" i SET AS %f",i)
  if(is.na(i)){
    printf("Returning Null as couldnt find i")
    return (NULL)
  }
  automata.state<-0
  direction<-1
  
  start.index<-i
  end.index<-i
  result<-data.frame(stringsAsFactors = FALSE)
  
  uptrend.entry<-0.0
  uptrend.exit<-0.0
  downtrend.entry<-0.0
  downtrend.exit<-0.0
  entry.level<-0.0
  exit.level<-0.0
  first.flag<-TRUE
  
  while(i<=(nrow(rsi)-1)){
    if(automata.state==0 & as.numeric(rsi[i,1])<entry.threhold1) {  #smi below uptrend entry levels
      printf("starting trend %s , direction %d",as.Date(index(prices2)[i]), 1)
      direction<-1
      automata.state<-1
      start.index<-i
      entry.level<-as.numeric(rsi[i,1])
    }
    else if(automata.state==0 & as.numeric(rsi[i,1])>entry.threhold1) {  #smi above downtrend entry levels
      printf("starting trend %s , direction %d",as.Date(index(prices2)[i]), 0)
      direction<-0
      automata.state<-1
      start.index<-i
      entry.level<-as.numeric(rsi[i,1])
    }
    else if(automata.state==1 & direction==1 & as.numeric(rsi[i,1])>exit.thresold1) {  #smi above exit levels
      automata.state<-0
      end.index<-i
      exit.level<-as.numeric(rsi[i,1])
      i<- (i-1)  #Decrease i as this crossing potentially can also be the start of new pattern
      
      temp.prices<-prices2[max(1, (i-smi.lookback)):(i-1),]
      temp.nifty<-nifty[max(1, (i-smi.lookback)):(i-1),]
      stock.ret.var<-(sd(diff(temp.prices$Close)/lag(temp.prices$Close), na.rm = TRUE) ^2)
      # stock.ret.var<-0
      # stock.beta<-0
      # stock.rsquare<-0
      stock.beta<-beta(temp.prices, temp.nifty)
      stock.rsquare<-rsquare(temp.prices,temp.nifty)
      
      ret<-(as.numeric(prices2$Open[end.index+1]) - as.numeric(prices2$Open[start.index+1])) / as.numeric(prices2$Open[start.index+1])
      cons.ret<- (as.numeric(prices2$Low[end.index+1]) - as.numeric(prices2$High[start.index+1])) / as.numeric(prices2$High[start.index+1])   #bought high for uptrend and sold low
      tmp<-tail(rsi[,1],120)
      adf.result1 <- adf.test(as.vector(tmp[!is.na(tmp[,1]),1]))$p.value
      pp.result1<- pp.test(as.vector(tmp[!is.na(tmp[,1]),1]))$p.value
      tmp<-tail(rsi[,1],240)
      adf.result2 <- adf.test(as.vector(tmp[!is.na(tmp[,1]),1]))$p.value
      pp.result2<- pp.test(as.vector(tmp[!is.na(tmp[,1]),1]))$p.value
      tmp<-tail(rsi[,1],300)
      adf.result3 <- adf.test(as.vector(tmp[!is.na(tmp[,1]),1]))$p.value
      pp.result3<- pp.test(as.vector(tmp[!is.na(tmp[,1]),1]))$p.value
      tmp<-tail(rsi[,1],400)
      adf.result4 <- adf.test(as.vector(tmp[!is.na(tmp[,1]),1]))$p.value
      pp.result4<- pp.test(as.vector(tmp[!is.na(tmp[,1]),1]))$p.value
      temp.row<-list(ticker, stock.ret.var, stock.beta, stock.rsquare, as.character(index(prices2)[start.index]), as.character(index(prices2)[end.index]), direction, (end.index-start.index), ret, cons.ret, drawdown, entry.level, exit.level, entry.percentage, exit.percentage, nifty.adx$ADX[start.index]*ifelse(nifty.adx$DIn[start.index] < nifty.adx$DIp[start.index], 1, -1), nifty.smi$SMI[start.index], adf.result1, pp.result1, adf.result2, pp.result2, adf.result3, pp.result3, adf.result4, pp.result4, ifelse((ret+cons.ret)>0,1,0))
      result<-rbind.data.frame(result, temp.row, stringsAsFactors=FALSE)
    }
    else if(automata.state==1 & direction==0 & as.numeric(rsi[i,1])<downtrend.exit) {  #smi below exit level
      automata.state<-0
      end.index<-i
      exit.level<-as.numeric(rsi[i,1])
      i<- (i-1)  #Decrease i as this crossing potentially can also be the start of new pattern
      temp.prices<-prices2[max(1, (i-smi.lookback)):(i-1),]
      temp.nifty<-nifty[max(1, (i-smi.lookback)):(i-1),]
      stock.ret.var<-(sd(diff(temp.prices$Close)/lag(temp.prices$Close), na.rm = TRUE) ^2)
      # stock.ret.var<-0
      # stock.beta<-0
      # stock.rsquare<-0
      stock.beta<-beta(temp.prices, temp.nifty)
      stock.rsquare<-rsquare(temp.prices,temp.nifty)
      
      ret<- -(as.numeric(prices2$Open[end.index+1]) - as.numeric(prices2$Open[start.index+1])) / as.numeric(prices2$Open[start.index+1])  #return are negated as its downtrend
      cons.ret<- -(as.numeric(prices2$High[end.index+1]) - as.numeric(prices2$Low[start.index+1])) / as.numeric(prices2$Low[start.index+1])   #Sold low for downtrend and bought high
      tmp<-tail(rsi[,1],100)
      adf.result1 <- adf.test(as.vector(tmp[!is.na(tmp[,1]),1]))$p.value
      pp.result1<- pp.test(as.vector(tmp[!is.na(tmp[,1]),1]))$p.value
      tmp<-tail(rsi[,1],200)
      adf.result2 <- adf.test(as.vector(tmp[!is.na(tmp[,1]),1]))$p.value
      pp.result2<- pp.test(as.vector(tmp[!is.na(tmp[,1]),1]))$p.value
      tmp<-tail(rsi[,1],300)
      adf.result3 <- adf.test(as.vector(tmp[!is.na(tmp[,1]),1]))$p.value
      pp.result3<- pp.test(as.vector(tmp[!is.na(tmp[,1]),1]))$p.value
      tmp<-tail(rsi[,1],400)
      adf.result4 <- adf.test(as.vector(tmp[!is.na(tmp[,1]),1]))$p.value
      pp.result4<- pp.test(as.vector(tmp[!is.na(tmp[,1]),1]))$p.value
      temp.row<-list(ticker, stock.ret.var, stock.beta, stock.rsquare, as.character(index(prices2)[start.index]), as.character(index(prices2)[end.index]), direction, (end.index-start.index), ret, cons.ret, drawdown, entry.level, exit.level, entry.percentage, exit.percentage, nifty.adx$ADX[start.index]*ifelse(nifty.adx$DIn[start.index] > nifty.adx$DIp[start.index], 1, -1), nifty.smi$SMI[start.index], adf.result1, pp.result1, adf.result2, pp.result2, adf.result3, pp.result3, adf.result4, pp.result4, ifelse((ret+cons.ret)>0,1,0))
      result<-rbind.data.frame(result, temp.row, stringsAsFactors=FALSE)
    }
    i<-(i+1)
  }
  header<-c("stock.ticker", "ret.var", "stock.beta", "stock.rsquare", "start.date", "end.date", "direction", "seq length", "returns", "Cons-Return", "drawdown", "entry.level", "exit.level", "entry.percentage", "exit.percentage", "NIFTY.ADX", "NIFTY.SMI", "adf.result1", "pp.result1", "adf.result2", "pp.result2", "adf.result3", "pp.result3", "adf.result4", "pp.result4", "viable")
  if(nrow(result)>0){colnames(result)<-header}
  return(result)
}

corp.action.file<-"D:/Work/Stocks/Corporate_Actions.csv"
corp.actions <- read.csv(corp.action.file, header=TRUE)
corp.actions <-set.corp.action.data(corp.actions)

tickers_100<-c('CNX_NIFTY.NS','ABB.NS','ACC.NS','ADANIPORTS.NS','AMBUJACEM.NS','ASHOKLEY.NS','ASIANPAINT.NS','AUROPHARMA.NS','AXISBANK.NS','BAJAJ-AUTO.NS','BAJFINANCE.NS','BAJAJFINSV.NS','BANKBARODA.NS','BEL.NS','BHEL.NS','BPCL.NS','BHARTIARTL.NS','INFRATEL.NS','BOSCHLTD.NS','BRITANNIA.NS','CADILAHC.NS','CIPLA.NS','COALINDIA.NS','COLPAL.NS','CONCOR.NS','CUMMINSIND.NS','DLF.NS','DABUR.NS','DIVISLAB.NS','DRREDDY.NS','EICHERMOT.NS','EMAMILTD.NS','GAIL.NS','GSKCONS.NS','GLAXO.NS','GLENMARK.NS','GODREJCP.NS','GRASIM.NS','HCLTECH.NS','HDFCBANK.NS','HAVELLS.NS','HEROMOTOCO.NS','HINDALCO.NS','HINDPETRO.NS','HINDUNILVR.NS','HINDZINC.NS','HDFC.NS','ITC.NS','ICICIBANK.NS','IDEA.NS','IBULHSGFIN.NS','IOC.NS','INDUSINDBK.NS','INFY.NS','JSWSTEEL.NS','KOTAKBANK.NS','LICHSGFIN.NS','LT.NS','LUPIN.NS','M&M.NS','MARICO.NS','MARUTI.NS','MOTHERSUMI.NS','NHPC.NS','NMDC.NS','NTPC.NS','ONGC.NS','OIL.NS','OFSS.NS','PETRONET.NS','PIDILITIND.NS','PEL.NS','PFC.NS','POWERGRID.NS','PGHH.NS','PNB.NS','RELIANCE.NS','RECLTD.NS','SHREECEM.NS','SRTRANSFIN.NS','SIEMENS.NS','SBIN.NS','SAIL.NS','SUNPHARMA.NS','TCS.NS','TATAMTRDVR.NS','TATAMOTORS.NS','TATAPOWER.NS','TATASTEEL.NS','TECHM.NS','TITAN.NS','TORNTPHARM.NS','UPL.NS','ULTRACEMCO.NS','UBL.NS','MCDOWELL-N.NS','WIPRO.NS','YESBANK.NS','ZEEL.NS')
# tickers_100<-c('CNX_NIFTY.NS')
lookback<-700
additional.lookback<-600
nifty <- adjust.prices("CNX_NIFTY", Sys.Date() - lookback- additional.lookback, Sys.Date()-3, corp.actions)
nifty.adx<-ADX(nifty[,c("High","Low","Close")], n=50, maType="EMA")
#SMI indicator-nifty
nifty.smi  <- SMI(nifty[,c("High", "Low", "Close")], maType="EMA")
entry<-c(1,3,5)
exit<-c(0,10)
periods<-c(200,300,400,500)
summary<-data.frame(stringsAsFactors = FALSE)
for(m in entry){
  entry.percentage<-m
  for (n in exit){
    exit.percentage<-n   #exit at 50%+- exit percentage
    for(p in periods){
      smi.lookback<-p
      output<-data.frame(stringsAsFactors = FALSE)
      printf("EXECUTING %d, %d, %d",m,n,p)
      for(i in 1:length(tickers_100)){
        prices <- adjust.prices(substr(tickers_100[i], 1, nchar(tickers_100[i])-3), Sys.Date() - lookback- additional.lookback, Sys.Date()-2,corp.actions )
        result<-run.strategy(tickers_100[i], prices, nifty, nifty.adx, nifty.smi, Sys.Date() - lookback, Sys.Date()-3)
        if(!is.null(result)){
          output<-rbind.data.frame(output, result,stringsAsFactors = FALSE)
        }
      }  
      # write.table(output, file = paste("D://Work/Stocks/smi-", m, "-", n, "-", p, "-", ".csv", sep=""), append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
      write.table(output, file = paste("D://Work/Stocks/smi-quick", ".csv", sep=""), append = TRUE, col.names = FALSE, sep = ",", row.names=FALSE)
      temp.list<-list(m, n, p, mean(output$returns), mean(output$`Cons-Return`), mean(output$`seq length`), length(output$`seq length`), mean(output$returns[output$direction==1]), mean(output$`seq length`[output$direction==1]), length(output$`seq length`[output$direction==1]), mean(output$returns[output$direction==0]), mean(output$`seq length`[output$direction==0]), length(output$`seq length`[output$direction==1]))
      summary<-rbind.data.frame(summary, temp.list,stringsAsFactors = FALSE)
      colnames(summary)<-c("Entry", "Exit", "Lookback", "avg return", "avg cons.ret", "Avg seq.len", "Count", "1 avg return", "1 Avg seq.len", "1 count", "0 avg return", "0 Avg seq.len", "0 count")
      # write.table(summary, "D://Work/Stocks/smi-summary2.csv",append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
    }
  }
}
colnames(summary)<-c("Entry", "Exit", "Lookback", "avg return", "avg cons.ret", "Avg seq.len", "Count", "1 avg return", "1 Avg seq.len", "1 count", "0 avg return", "0 Avg seq.len", "0 count")
# write.table(summary, "D://Work/Stocks/smi-summary2.csv",append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
# output<-data.frame(stringsAsFactors = FALSE)
# for(i in 1:length(tickers_100)){
#   prices <- adjust.prices(substr(tickers_100[i], 1, nchar(tickers_100[i])-3), Sys.Date() - lookback- additional.lookback, Sys.Date()-1,corp.actions )
#   result<-run.strategy(tickers_100[i], prices, nifty, nifty.adx, nifty.smi, Sys.Date() - lookback, Sys.Date()-1)
#   output<-rbind.data.frame(output, result,stringsAsFactors = FALSE)
# }  
# 
# write.table(output, file = paste("D://Work/Stocks/smi-", "lookback200",".csv", sep=""), append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)

# cutoff.date<-"2017-03-31"
# training<-output[output$end.date<cutoff.date,]
# testing<-output[output$start.date>cutoff.date,]
# write.table(training, "D://Work/Stocks/smi-train1.csv",append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
# write.table(testing, "D://Work/Stocks/smi-test1.csv",append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)




source('D:/Work/Stocks/util-functions.R')




data.dir<-"D:/Work/Stocks/data/"
Quandl.api_key("rAHKcgv_ymH7_o9s5nWN")

entry.threhold1<-30
entry.threhold2<-70   #exit at 50%+- exit percentage
exit.thresold1<-50
exit.threhold2<-50
holding.period<-7
corp.action.file<-"D:/Work/Stocks/Corporate_Actions.csv"
corp.actions <- read.csv(corp.action.file, header=TRUE)
corp.actions <-set.corp.action.data(corp.actions)
tickers_100<-c('CNX_NIFTY.NS','ABB.NS','ACC.NS','ADANIPORTS.NS','AMBUJACEM.NS','ASHOKLEY.NS','ASIANPAINT.NS','AUROPHARMA.NS','AXISBANK.NS','BAJAJ-AUTO.NS','BAJFINANCE.NS','BAJAJFINSV.NS','BANKBARODA.NS','BEL.NS','BHEL.NS','BPCL.NS','BHARTIARTL.NS','INFRATEL.NS','BOSCHLTD.NS','BRITANNIA.NS','CADILAHC.NS','CIPLA.NS','COALINDIA.NS','COLPAL.NS','CONCOR.NS','CUMMINSIND.NS','DLF.NS','DABUR.NS','DIVISLAB.NS','DRREDDY.NS','EICHERMOT.NS','EMAMILTD.NS','GAIL.NS','GSKCONS.NS','GLAXO.NS','GLENMARK.NS','GODREJCP.NS','GRASIM.NS','HCLTECH.NS','HDFCBANK.NS','HAVELLS.NS','HEROMOTOCO.NS','HINDALCO.NS','HINDPETRO.NS','HINDUNILVR.NS','HINDZINC.NS','HDFC.NS','ITC.NS','ICICIBANK.NS','IDEA.NS','IBULHSGFIN.NS','IOC.NS','INDUSINDBK.NS','INFY.NS','JSWSTEEL.NS','KOTAKBANK.NS','LICHSGFIN.NS','LT.NS','LUPIN.NS','M&M.NS','MARICO.NS','MARUTI.NS','MOTHERSUMI.NS','NHPC.NS','NMDC.NS','NTPC.NS','ONGC.NS','OIL.NS','OFSS.NS','PETRONET.NS','PIDILITIND.NS','PEL.NS','PFC.NS','POWERGRID.NS','PGHH.NS','PNB.NS','RELIANCE.NS','RECLTD.NS','SHREECEM.NS','SRTRANSFIN.NS','SIEMENS.NS','SBIN.NS','SAIL.NS','SUNPHARMA.NS','TCS.NS','TATAMTRDVR.NS','TATAMOTORS.NS','TATAPOWER.NS','TATASTEEL.NS','TECHM.NS','TITAN.NS','TORNTPHARM.NS','UPL.NS','ULTRACEMCO.NS','UBL.NS','MCDOWELL-N.NS','WIPRO.NS','YESBANK.NS','ZEEL.NS')
# tickers_100<-c('CNX_NIFTY.NS')
lookback<-2500
nifty <- Quandl("NSE/CNX_NIFTY", start_date=Sys.Date() - lookback, end_date=Sys.Date(), type = 'xts')
nifty.rsi<-RSI(nifty[,c("Close")], maType="EMA")
for(i in which.min(nifty.rsi>0):nrow(nifty.rsi)-holding.period){
  
}
