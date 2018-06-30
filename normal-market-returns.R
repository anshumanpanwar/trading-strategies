library(Quandl)
library(httr)
Quandl.api_key("rAHKcgv_ymH7_o9s5nWN")

tickers_100<-c('CNX_NIFTY.NS','ABB.NS','ACC.NS','ADANIPORTS.NS','AMBUJACEM.NS','ASHOKLEY.NS','ASIANPAINT.NS','AUROPHARMA.NS','AXISBANK.NS','BAJAJ-AUTO.NS','BAJFINANCE.NS','BAJAJFINSV.NS','BANKBARODA.NS','BEL.NS','BHEL.NS','BPCL.NS','BHARTIARTL.NS','INFRATEL.NS','BOSCHLTD.NS','BRITANNIA.NS','CADILAHC.NS','CIPLA.NS','COALINDIA.NS','COLPAL.NS','CONCOR.NS','CUMMINSIND.NS','DLF.NS','DABUR.NS','DIVISLAB.NS','DRREDDY.NS','EICHERMOT.NS','EMAMILTD.NS','GAIL.NS','GSKCONS.NS','GLAXO.NS','GLENMARK.NS','GODREJCP.NS','GRASIM.NS','HCLTECH.NS','HDFCBANK.NS','HAVELLS.NS','HEROMOTOCO.NS','HINDALCO.NS','HINDPETRO.NS','HINDUNILVR.NS','HINDZINC.NS','HDFC.NS','ITC.NS','ICICIBANK.NS','IDEA.NS','IBULHSGFIN.NS','IOC.NS','INDUSINDBK.NS','INFY.NS','JSWSTEEL.NS','KOTAKBANK.NS','LICHSGFIN.NS','LT.NS','LUPIN.NS','M&M.NS','MARICO.NS','MARUTI.NS','MOTHERSUMI.NS','NHPC.NS','NMDC.NS','NTPC.NS','ONGC.NS','OIL.NS','OFSS.NS','PETRONET.NS','PIDILITIND.NS','PEL.NS','PFC.NS','POWERGRID.NS','PGHH.NS','PNB.NS','RELIANCE.NS','RECLTD.NS','SHREECEM.NS','SRTRANSFIN.NS','SIEMENS.NS','SBIN.NS','SAIL.NS','SUNPHARMA.NS','TCS.NS','TATAMTRDVR.NS','TATAMOTORS.NS','TATAPOWER.NS','TATASTEEL.NS','TECHM.NS','TITAN.NS','TORNTPHARM.NS','UPL.NS','ULTRACEMCO.NS','UBL.NS','MCDOWELL-N.NS','VEDL.NS','WIPRO.NS','YESBANK.NS','ZEEL.NS')
# tickers_100<-c('CNX_NIFTY.NS')
lookback<-700
summary<-data.frame(stringsAsFactors = FALSE)
for(i in 1:length(tickers_100)){
  prices <- adjust.prices(substr(tickers_100[i], 1, nchar(tickers_100[i])-3), Sys.Date() - lookback, Sys.Date()-3,corp.actions )
  ret<-(as.numeric(prices$adj.close[nrow(prices)]) - as.numeric(prices$adj.close[1]))/as.numeric(prices$adj.close[1])
  printf("%s, %f, %d",tickers_100[i],ret, nrow(prices) )
  temp.list<-list(tickers_100[i],ret, nrow(prices))
  summary<-rbind.data.frame(summary, temp.list,stringsAsFactors = FALSE)
}
write.table(summary, "D://Work/Stocks/normal-market-retunrs.csv",append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
