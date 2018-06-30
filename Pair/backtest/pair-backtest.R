library(Quandl)
library(httr)
library(egcm)
library(zoo)
library(data.table)
library(quantmod)

Quandl.api_key("rAHKcgv_ymH7_o9s5nWN")
work.dir<-"D://Work/Stocks/pair/"
corp.action.file<-paste(work.dir, "Corporate_Actions.csv" , sep="")
data.dir<-"D:/Work/Stocks/data/"
sectors.file<-paste(work.dir, "customized-sectors.csv" , sep="")


printf <- function(...)print(sprintf(...))

read.marketdata <- function(stock.ticker, start_date, end_date) {
  prices<-data.frame(stringsAsFactors = FALSE)
  out <- tryCatch(
    {
      # Just to highlight: if you want to use more than one 
      # R expression in the "try" part then you'll have to 
      # use curly brackets.
      # 'tryCatch()' will return the last evaluated expression 
      # in case the "try" part was completed successfully
      
      prices <- as.xts(read.zoo(paste(data.dir, stock.ticker, ".csv", sep=""), header = TRUE, sep = ",", format = "%Y-%m-%d"))
      print("read zoo")
      # print(prices)
      
      if(index(prices)[nrow(prices)]<getLastWeekDay(end_date) | index(prices)[1]>getNextWeekDay(start_date)) {stop("File doesnt have sufficient data")}
      prices[paste(start_date, end_date, sep="::"),]
      # The return value of `prices` is the actual value 
      # that will be returned in case there is no condition 
      # (e.g. warning or error). 
      # You don't need to state the return value via `return()` as code 
      # in the "try" part is not wrapped insided a function (unlike that
      # for the condition handlers for warnings and error below)
    },
    error=function(cond) {
      #message(paste("File not found ", stock.ticker,"\n"))
      message("Here's the original error message:")
      message(cond)
      stock.ticker.modified=gsub("&","",stock.ticker)
      stock.ticker.modified=gsub("-","_",stock.ticker.modified)
      prices <- Quandl(paste("NSE/", stock.ticker.modified, sep=""), type="xts")
      write.zoo(prices, file=paste(data.dir, stock.ticker, ".csv", sep=""), sep=",")
      # Choose a return value in case of error
      return(prices[paste(start_date, end_date, sep="::"),])
    },
    warning=function(cond) {
      message(paste("File not found ", stock.ticker,"\n"))
      message(cond)
      # Choose a return value in case of warning
      message(cond)
      stock.ticker.modified=gsub("&","",stock.ticker)
      stock.ticker.modified=gsub("-","_",stock.ticker.modified)
      prices <- Quandl(paste("NSE/", stock.ticker.modified, sep=""), type="xts")
      write.zoo(prices, file=paste(data.dir, stock.ticker, ".csv", sep=""), sep=",")
      message("Price freshly extracted and written to the file\n")
      return(prices[paste(start_date, end_date, sep="::"),])
    },
    finally={
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you 
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally=<expression>' 
      message(paste("\nProcessed ticker:", stock.ticker))
    }
  )
  return(out)
}

getNextWeekDay<-function(date){
  if(weekdays(date) == "Saturday"){return (date+2)}
  if(weekdays(date) == "Sunday"){return (date+1)} 
  return(date)
}

getLastWeekDay<-function(date){
  if(weekdays(date) == "Saturday"){return (date-1)}
  if(weekdays(date) == "Sunday"){return (date-2)} 
  return(date)
}

adjust.prices<-function(stock.ticker, start_date, end_date, corp.actions){
  printf("Starting for stock %s", stock.ticker)
  stock.ticker.modified=gsub("&","",stock.ticker)
  stock.ticker.modified=gsub("-","_",stock.ticker.modified)
  data <- read.marketdata(stock.ticker, start_date, end_date)
  # data <- Quandl(paste("NSE/", stock.ticker.modified, sep=""), start_date=start_date, end_date=end_date, type="xts")
  data$adj.close<-data$Close
  stock.corp.actions<-corp.actions[corp.actions$corp.action.type=="Dividend" & corp.actions$Security.Name==stock.ticker & corp.actions$Ex.Date>=start_date,]
  if(nrow(stock.corp.actions)>0){
    for(i in 1:nrow(stock.corp.actions)){
      adj.factor<-as.numeric(data$Close[stock.corp.actions$Ex.Date[i]])/(stock.corp.actions$Value[i]+as.numeric(data$Close[stock.corp.actions$Ex.Date[i]]))
      printf("Dividend adj factor %f", adj.factor)
      print(adj.factor)
      data$adj.close[index(data)<stock.corp.actions$Ex.Date[i]]<- data$adj.close[index(data)<stock.corp.actions$Ex.Date[i]]*adj.factor
    }
  }
  stock.corp.actions<-corp.actions[corp.actions$corp.action.type=="Split" & corp.actions$Security.Name==stock.ticker & corp.actions$Ex.Date>=start_date,]
  if(nrow(stock.corp.actions)>0){
    for(i in 1:nrow(stock.corp.actions)){
      adj.factor<-stock.corp.actions$Value[i]
      printf("Split adj factor %f", adj.factor)
      data$adj.close[index(data)<stock.corp.actions$Ex.Date[i]]<- data$adj.close[index(data)<stock.corp.actions$Ex.Date[i]]/adj.factor
    }
  }
  stock.corp.actions<-corp.actions[corp.actions$corp.action.type=="Bonus" & corp.actions$Security.Name==stock.ticker & corp.actions$Ex.Date>=start_date,]
  if(nrow(stock.corp.actions)>0){
    for(i in 1:nrow(stock.corp.actions)){
      adj.factor<-stock.corp.actions$Value[i]
      printf("Bonus adj factor %f", adj.factor)
      data$adj.close[index(data)<stock.corp.actions$Ex.Date[i]]<- data$adj.close[index(data)<stock.corp.actions$Ex.Date[i]]/adj.factor
    }
  }
  return(data)
}

set.corp.action.data<-function(corp.actions){
  
  corp.action.types<-c("Split", "Dividend", "Bonus")
  corp.actions <- corp.actions[grep(paste0(corp.action.types, collapse = "|"), corp.actions$Purpose),]
  #check for date format in the ex.date in the corp actione file everytime it is updated as it changes frequently. %b stands for 3 char months, use %y (small case if year is 2 digit, upper case otherwise) 
  corp.actions$Ex.Date<-as.Date(corp.actions$Ex.Date, format = "%d-%b-%y")  
  corp.actions$Purpose<-as.character(corp.actions$Purpose)
  
  corp.actions$corp.action.type<-NULL
  corp.actions$Value<-NULL
  
  for(i in 1:nrow(corp.actions)){
    if(length(unlist(gregexpr(pattern ='Dividend',corp.actions$Purpose[i])))>0 & unlist(gregexpr(pattern ='Dividend',corp.actions$Purpose[i]))!=-1){
      div<-as.double(substr(corp.actions$Purpose[i], tail( unlist(gregexpr(pattern ='- ',corp.actions$Purpose[i])),n=1)+2, nchar(corp.actions$Purpose[i])))
      corp.actions$corp.action.type[i]<-"Dividend"
      corp.actions$Value[i]<-div
      printf("%s, %s, %s, %f", corp.actions$Security.Name[i], "Dividend", corp.actions$Ex.Date[i], div)
    }
    else if(length(unlist(gregexpr(pattern ='Split',corp.actions$Purpose[i])))>0 & unlist(gregexpr(pattern ='Split',corp.actions$Purpose[i]))!=-1){
      # printf("Entered split")
      split.factor<-as.double(substr(corp.actions$Purpose[i], head( unlist(gregexpr(pattern ='Rs.',corp.actions$Purpose[i])),n=1)+3, head( unlist(gregexpr(pattern ='/-',corp.actions$Purpose[i])),n=1)-1))/
        as.double(substr(corp.actions$Purpose[i], tail( unlist(gregexpr(pattern ='Rs.',corp.actions$Purpose[i])),n=1)+3, tail( unlist(gregexpr(pattern ='/-',corp.actions$Purpose[i])),n=1)-1))
      corp.actions$corp.action.type[i]<-"Split"
      corp.actions$Value[i]<-split.factor
      printf("%s, %s, %s, %f", corp.actions$Security.Name[i], "Split", corp.actions$Ex.Date[i], split.factor)
    }
    else if(length(unlist(gregexpr(pattern ='Bonus',corp.actions$Purpose[i])))>0 & unlist(gregexpr(pattern ='Bonus',corp.actions$Purpose[i]))!=-1){
      new.share<-as.double(substr(corp.actions$Purpose[i], tail( unlist(gregexpr(pattern =' ',corp.actions$Purpose[i])),n=1)+1, head( unlist(gregexpr(pattern =':',corp.actions$Purpose[i])),n=1)-1))
      per.share<-as.double(substr(corp.actions$Purpose[i], head( unlist(gregexpr(pattern =':',corp.actions$Purpose[i])),n=1)+1, nchar(corp.actions$Purpose[i]) ))
      corp.actions$corp.action.type[i]<-"Bonus"
      corp.actions$Value[i]<-(new.share+per.share)/per.share
      printf("%s, %s, %s, %f", corp.actions$Security.Name[i], "Bonus", corp.actions$Ex.Date[i], (new.share+per.share)/per.share)
    }
  }
  return(corp.actions)
}

normalize.prices<-function(prices){
  data<-(prices$Close)
  data$Close<-prices$adj.close
  data$Open<-prices$Open*(prices$adj.close/prices$Close)
  data$High<-prices$High*(prices$adj.close/prices$Close)
  data$Low<-prices$Low*(prices$adj.close/prices$Close)
  return(data)
}

printf <- function(...)print(sprintf(...))

corp.actions <- read.csv(corp.action.file, header=TRUE)
corp.actions <-set.corp.action.data(corp.actions)
# data<-adjust.prices("GODREJCP",'2016-01-01', '2017-05-23', corp.actions)

tickers_100<-c('ABB.NS','ACC.NS','ADANIPORTS.NS','AMBUJACEM.NS','ASHOKLEY.NS','ASIANPAINT.NS','AUROPHARMA.NS','AXISBANK.NS','BAJAJ-AUTO.NS','BAJFINANCE.NS','BAJAJFINSV.NS','BANKBARODA.NS','BEL.NS','BHEL.NS','BPCL.NS','BHARTIARTL.NS','INFRATEL.NS','BOSCHLTD.NS','BRITANNIA.NS','CADILAHC.NS','CIPLA.NS','COALINDIA.NS','COLPAL.NS','CONCOR.NS','CUMMINSIND.NS','DLF.NS','DABUR.NS','DIVISLAB.NS','DRREDDY.NS','EICHERMOT.NS','EMAMILTD.NS','GAIL.NS','GSKCONS.NS','GLAXO.NS','GLENMARK.NS','GODREJCP.NS','GRASIM.NS','HCLTECH.NS','HDFCBANK.NS','HAVELLS.NS','HEROMOTOCO.NS','HINDALCO.NS','HINDPETRO.NS','HINDUNILVR.NS','HINDZINC.NS','HDFC.NS','ITC.NS','ICICIBANK.NS','ICICIPRULI.NS','IDEA.NS','IBULHSGFIN.NS','IOC.NS','INDUSINDBK.NS','INFY.NS','INDIGO.NS','JSWSTEEL.NS','KOTAKBANK.NS','LICHSGFIN.NS','LT.NS','LUPIN.NS','M&M.NS','MARICO.NS','MARUTI.NS','MOTHERSUMI.NS','NHPC.NS','NMDC.NS','NTPC.NS','ONGC.NS','OIL.NS','OFSS.NS','PETRONET.NS','PIDILITIND.NS','PEL.NS','PFC.NS','POWERGRID.NS','PGHH.NS','PNB.NS','RELIANCE.NS','RECLTD.NS','SHREECEM.NS','SRTRANSFIN.NS','SIEMENS.NS','SBIN.NS','SAIL.NS','SUNPHARMA.NS','TCS.NS','TATAMTRDVR.NS','TATAMOTORS.NS','TATAPOWER.NS','TATASTEEL.NS','TECHM.NS','TITAN.NS','TORNTPHARM.NS','UPL.NS','ULTRACEMCO.NS','UBL.NS','MCDOWELL-N.NS','VEDL.NS','WIPRO.NS','YESBANK.NS','ZEEL.NS')
ignore_100<-c('ADANIPORTS.NS','ASIANPAINT.NS','AUROPHARMA.NS','BAJAJ-AUTO.NS','BAJAJFINSV.NS','BANKBARODA.NS','BHARTIARTL.NS','HEROMOTOCO.NS','ICICIPRULI.NS','M&M.NS','TORNTPHARM.NS','ULTRACEMCO.NS','MCDOWELL-N.NS')
tickers_200<-c('ABB.NS','ACC.NS','AIAENG.NS','ADANIENT.NS','ADANIPORTS.NS','ADANIPOWER.NS','ABFRL.NS','ABIRLANUVO.NS','AJANTPHARM.NS','APLLTD.NS','ALKEM.NS','AMARAJABAT.NS','AMBUJACEM.NS','APOLLOHOSP.NS','APOLLOTYRE.NS','ARVIND.NS','ASHOKLEY.NS','ASIANPAINT.NS','AUROPHARMA.NS','AXISBANK.NS','BAJAJ-AUTO.NS','BAJFINANCE.NS','BAJAJFINSV.NS','BANKBARODA.NS','BANKINDIA.NS','BATAINDIA.NS','BERGEPAINT.NS','BEL.NS','BHARATFIN.NS','BHARATFORG.NS','BHEL.NS','BPCL.NS','BHARTIARTL.NS','INFRATEL.NS','BIOCON.NS','BOSCHLTD.NS','BRITANNIA.NS','CESC.NS','CRISIL.NS','CADILAHC.NS','CANBK.NS','CASTROLIND.NS','CENTRALBK.NS','CENTURYTEX.NS','CHOLAFIN.NS','CIPLA.NS','COALINDIA.NS','COLPAL.NS','CONCOR.NS','COROMANDEL.NS','CROMPTON.NS','CUMMINSIND.NS','DLF.NS','DABUR.NS','DALMIABHA.NS','DHFL.NS','DISHTV.NS','DIVISLAB.NS','LALPATHLAB.NS','DRREDDY.NS','EDELWEISS.NS','EICHERMOT.NS','EMAMILTD.NS','ENDURANCE.NS','ENGINERSIN.NS','EXIDEIND.NS','FEDERALBNK.NS','FORTIS.NS','GAIL.NS','GMRINFRA.NS','GSKCONS.NS','GLAXO.NS','GLENMARK.NS','GODREJCP.NS','GODREJIND.NS','GRASIM.NS','GRUH.NS','GPPL.NS','GSPL.NS','HCLTECH.NS','HDFCBANK.NS','HAVELLS.NS','HEROMOTOCO.NS','HEXAWARE.NS','HINDALCO.NS','HINDPETRO.NS','HINDUNILVR.NS','HINDZINC.NS','HDFC.NS','ITC.NS','ICICIBANK.NS','ICICIPRULI.NS','IDBI.NS','IDFCBANK.NS','IDFC.NS','IRB.NS','IDEA.NS','IBULHSGFIN.NS','INDIANB.NS','INDHOTEL.NS','IOC.NS','IGL.NS','INDUSINDBK.NS','NAUKRI.NS','INFY.NS','INDIGO.NS','IPCALAB.NS','JSWENERGY.NS','JSWSTEEL.NS','JINDALSTEL.NS','JUBLFOOD.NS','JUBILANT.NS','KARURVYSYA.NS','KOTAKBANK.NS','L&TFH.NS','LTTS.NS','LICHSGFIN.NS','LT.NS','LUPIN.NS','MRF.NS','M&MFIN.NS','M&M.NS','MANAPPURAM.NS','MARICO.NS','MARUTI.NS','MINDTREE.NS','MOTHERSUMI.NS','MPHASIS.NS','MUTHOOTFIN.NS','NATCOPHARM.NS','NBCC.NS','NHPC.NS','NMDC.NS','NTPC.NS','NATIONALUM.NS','OBEROIRLTY.NS','ONGC.NS','OIL.NS','OFSS.NS','PCJEWELLER.NS','PIIND.NS','PAGEIND.NS','PETRONET.NS','PIDILITIND.NS','PEL.NS','PFC.NS','POWERGRID.NS','PRESTIGE.NS','PGHH.NS','PNB.NS','QUESS.NS','RBLBANK.NS','RAJESHEXPO.NS','RELCAPITAL.NS','RCOM.NS','RELIANCE.NS','RELINFRA.NS','RPOWER.NS','RECLTD.NS','SRF.NS','SHREECEM.NS','SRTRANSFIN.NS','SIEMENS.NS','SBIN.NS','SAIL.NS','STAR.NS','SPARC.NS','SUNPHARMA.NS','SUNTV.NS','SUZLON.NS','SYNDIBANK.NS','SYNGENE.NS','TV18BRDCST.NS','TVSMOTOR.NS','TATACHEM.NS','TATACOMM.NS','TCS.NS','TATAGLOBAL.NS','TATAMTRDVR.NS','TATAMOTORS.NS','TATAPOWER.NS','TATASTEEL.NS','TECHM.NS','RAMCOCEM.NS','THERMAX.NS','TITAN.NS','TORNTPHARM.NS','TORNTPOWER.NS','UPL.NS','ULTRACEMCO.NS','UNIONBANK.NS','UBL.NS','MCDOWELL-N.NS','VAKRANGEE.NS','VEDL.NS','VOLTAS.NS','WELSPUNIND.NS','WIPRO.NS','WOCKPHARMA.NS','YESBANK.NS','ZEEL.NS')
ignore_200<-c('BHARATFIN.NS', 'WOCKPHARMA.NS', 'GRUH.NS','BAJAJ-AUTO.NS', 'MCDOWELL-N.NS','CROMPTON.NS', 'ENDURANCE.NS', 'ICICIPRULI.NS', 'L&TFH.NS', 'LTTS.NS', 'M&MFIN.NS', 'M&M.NS', 'QUESS.NS', 'RBLBANK.NS')
tickers.with.futures<-c('OFSS.NS', 'ABIRLANUVO.NS', 'ACC.NS', 'ADANIENT.NS', 'AJANTPHARM.NS', 'ALBK.NS', 'AMARAJABAT.NS', 'ANDHRABANK.NS', 'APOLLOHOSP.NS', 'ARVIND.NS', 'AUROPHARMA.NS', 'AXISBANK.NS', 'BAJFINANCE.NS', 'BANKBARODA.NS', 'BANKINDIA.NS', 'BEML.NS', 'BHARATFIN.NS', 'BHARATFORG.NS', 'BHARTIARTL.NS', 'BOSCHLTD.NS', 'BPCL.NS', 'CADILAHC.NS', 'CESC.NS', 'CIPLA.NS', 'COLPAL.NS', 'CONCOR.NS', 'CGPOWER.NS', 'CUMMINSIND.NS', 'DABUR.NS', 'DCBBANK.NS', 'DISHTV.NS', 'DIVISLAB.NS', 'DLF.NS', 'DRREDDY.NS', 'EICHERMOT.NS', 'ASHOKLEY.NS', 'APOLLOTYRE.NS', 'ENGINERSIN.NS', 'EXIDEIND.NS', 'FEDERALBNK.NS', 'GRANULES.NS', 'HAVELLS.NS', 'HEXAWARE.NS', 'HINDPETRO.NS', 'HINDUNILVR.NS', 'HINDZINC.NS', 'ICIL.NS', 'IDBI.NS', 'BRITANNIA.NS', 'IDEA.NS', 'INDUSINDBK.NS', 'INFRATEL.NS', 'INFY.NS', 'BHEL.NS', 'JETAIRWAYS.NS', 'CAPF.NS', 'JINDALSTEL.NS', 'CENTURYTEX.NS', 'ADANIPORTS.NS', 'JSWENERGY.NS', 'JUSTDIAL.NS', 'KOTAKBANK.NS', 'KSCL.NS', 'L&TFH.NS', 'LT.NS', 'MARICO.NS', 'MARUTI.NS', 'MCDOWELL-N.NS', 'MINDTREE.NS', 'MRF.NS', 'NMDC.NS', 'ORIENTBANK.NS', 'COALINDIA.NS', 'PAGEIND.NS', 'PETRONET.NS', 'POWERGRID.NS', 'PTC.NS', 'RECLTD.NS', 'AMBUJACEM.NS', 'RELIANCE.NS', 'RPOWER.NS', 'SINTEX.NS', 'SYNDIBANK.NS', 'DHFL.NS', 'TATACHEM.NS', 'TATACOMM.NS', 'TATAELXSI.NS', 'EQUITAS.NS', 'TATASTEEL.NS', 'ASIANPAINT.NS', 'GODREJCP.NS', 'GODREJIND.NS', 'GRASIM.NS', 'HDFC.NS', 'BAJAJFINSV.NS', 'TITAN.NS', 'TORNTPHARM.NS', 'HDFCBANK.NS', 'TORNTPOWER.NS', 'HEROMOTOCO.NS', 'TV18BRDCST.NS', 'HINDALCO.NS', 'TVSMOTOR.NS', 'ULTRACEMCO.NS', 'VOLTAS.NS', 'BALRAMCHIN.NS', 'ICICIBANK.NS', 'IGL.NS', 'INDIANB.NS', 'WOCKPHARMA.NS', 'YESBANK.NS', 'INDIGO.NS', 'ZEEL.NS', 'BIOCON.NS', 'INFIBEAM.NS', 'CANBK.NS', 'BAJAJ_AUTO.NS', 'BATAINDIA.NS', 'IOC.NS', 'ITC.NS', 'CASTROLIND.NS', 'JISLJALEQS.NS', 'CEATLTD.NS', 'JSWSTEEL.NS', 'JUBLFOOD.NS', 'CHOLAFIN.NS', 'GLENMARK.NS', 'HDIL.NS', 'DALMIABHA.NS', 'IBULHSGFIN.NS', 'INDIACEM.NS', 'JPASSOCIAT.NS', 'MFSL.NS', 'MOTHERSUMI.NS', 'FORTIS.NS', 'GODFRYPHLP.NS', 'GSFC.NS', 'KTKBANK.NS', 'HCLTECH.NS', 'IBREALEST.NS', 'M&M.NS', 'IDFC.NS', 'IFCI.NS', 'NCC.NS', 'NTPC.NS', 'IRB.NS', 'RCOM.NS', 'RELCAPITAL.NS', 'SUNTV.NS', 'TCS.NS', 'M&MFIN.NS', 'MGL.NS', 'GAIL.NS', 'NIITTECH.NS', 'OIL.NS', 'MRPL.NS', 'PEL.NS', 'PFC.NS', 'MUTHOOTFIN.NS', 'PIDILITIND.NS', 'ONGC.NS', 'PNB.NS', 'PVR.NS', 'RNAVAL.NS', 'SAIL.NS', 'SBIN.NS', 'SHREECEM.NS', 'RELINFRA.NS', 'RAYMOND.NS', 'SIEMENS.NS', 'SOUTHBANK.NS', 'SRF.NS', 'SRTRANSFIN.NS', 'SUNPHARMA.NS', 'TATAGLOBAL.NS', 'TATAMOTORS.NS', 'TATAMTRDVR.NS', 'UNIONBANK.NS', 'VEDL.NS', 'STAR.NS', 'UJJIVAN.NS', 'ESCORTS.NS', 'UBL.NS', 'UPL.NS', 'WIPRO.NS', 'BALKRISIND.NS', 'CANFINHOME.NS', 'IDFCBANK.NS', 'KPIT.NS', 'TATAPOWER.NS', 'GMRINFRA.NS', 'LUPIN.NS', 'BEL.NS', 'LICHSGFIN.NS', 'ADANIPOWER.NS', 'TECHM.NS', 'MCX.NS', 'NBCC.NS', 'PCJEWELLER.NS', 'BERGEPAINT.NS', 'NESTLEIND.NS', 'VGUARD.NS', 'NHPC.NS', 'SUZLON.NS')
ignore.tickers.with.futures<-c('BHARATFIN.NS', 'WOCKPHARMA.NS','CGPOWER.NS', 'MGL.NS', 'ABIRLANUVO.NS', 'RNAVAL.NS')
ticker_new<-setdiff(tickers.with.futures, ignore.tickers.with.futures)
# ticker_new<-c('GODREJCP.NS','MARICO.NS','DABUR.NS', 'HDFC.NS')

remove_ticker<-c()

for(i in 1:length(ticker_new)){
  prices <- adjust.prices(substr(ticker_new[i], 1, nchar(ticker_new[i])-3), Sys.Date() - 365*4, Sys.Date(),corp.actions )
  showConnections()
  if(i==1) {
    all.data<-prices$adj.close
  }
  else {
    # all.data$temp <- c(rep(NA, nrow(all.data)-length(prices$adj.close)), prices$adj.close)
    # all.data<-merge(all.data,prices$adj.close,all=TRUE)
    if(index(prices)[1]>Sys.Date() - 365*4 +2){
      printf("ticker %s not sufficiently old for backtest start date %s required: %s", ticker_new[i], as.Date(index(prices)[1]), as.Date(Sys.Date() - 365*4))
      remove_ticker<-c(remove_ticker, ticker_new[i])
    }
    else{
      all.data<-merge(all.data,prices$adj.close,all=TRUE)
    }
    if((nrow(all.data)-length(prices$adj.close))!=0)
      printf("WARNING:Data length for %s is %f while should be %f",ticker_new[i],length(prices$adj.close),nrow(all.data))
    # all.data$temp<-prices$Close
  }
}
colnames(all.data) <- ticker_new[! ticker_new %in% remove_ticker]


printf("Replacing NA values")
all.data<-na.locf(all.data)
# colnames(all.data) <- ticker_new

write.zoo(all.data, file = paste(work.dir, "all-data-backtest-", format(Sys.Date(), "%Y%m%d"),".csv", sep=""), append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
all.data<-as.xts(read.zoo(file = paste(work.dir, "all-data-backtest-", format(Sys.Date(), "%Y%m%d"),".csv", sep=""),  sep = ",", header=TRUE, format="%Y-%m-%d", FUN=as.Date))

#Calculating RSI for all stocks

remove_ticker<-c()

for(i in 1:length(ticker_new)){
  prices <- adjust.prices(substr(ticker_new[i], 1, nchar(ticker_new[i])-3), Sys.Date() - 365*4-30, Sys.Date(),corp.actions )
  showConnections()
  if(i==1) {
    all.rsi<-RSI(prices$adj.close)[,1]
  }
  else {
    # all.data$temp <- c(rep(NA, nrow(all.data)-length(prices$adj.close)), prices$adj.close)
    # all.data<-merge(all.data,prices$adj.close,all=TRUE)
    if(index(prices)[1]>Sys.Date() - 365*4 -30+2){
      printf("ticker %s not sufficiently old for RSI start date %s required: %s", ticker_new[i], as.Date(index(prices)[1]), as.Date(Sys.Date() - 365*4-30))
      remove_ticker<-c(remove_ticker, ticker_new[i])
    }
    else{
      stock.rsi<-RSI(prices$adj.close)
      all.rsi<-merge(all.rsi, stock.rsi[,1], all=TRUE)
    }
    if((nrow(all.rsi)-length(stock.rsi))!=0)
      printf("WARNING:RSI length for %s is %f while should be %f",ticker_new[i],length(stock.rsi),nrow(all.rsi))
    # all.data$temp<-prices$Close
  }
}
colnames(all.rsi) <- ticker_new[! ticker_new %in% remove_ticker]
printf("Replacing NA values")
all.rsi<-merge(all.rsi, zoo( ,seq(start(all.rsi),end(all.rsi),by="day")) , all=TRUE)
all.rsi<-na.locf(all.rsi, na.rm=TRUE)
write.zoo(all.rsi, file = paste(work.dir, "all-rsi-backtest-", format(Sys.Date(), "%Y%m%d"),".csv", sep=""), append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
all.rsi<-as.xts(read.zoo(file = paste(work.dir, "all-rsi-backtest-", format(Sys.Date(), "%Y%m%d"),".csv", sep=""),  sep = ",", header=TRUE, format="%Y-%m-%d", FUN=as.Date))


#Calculating ADX for all stocks

remove_ticker<-c()

for(i in 1:length(ticker_new)){
  prices <- normalize.prices(adjust.prices(substr(ticker_new[i], 1, nchar(ticker_new[i])-3), Sys.Date() - 365*4-40, Sys.Date(),corp.actions ))
  showConnections()
  if(i==1) {
    all.adx<-ADX(prices, maType = "EMA")$ADX
  }
  else {
    # all.data$temp <- c(rep(NA, nrow(all.data)-length(prices$adj.close)), prices$adj.close)
    # all.data<-merge(all.data,prices$adj.close,all=TRUE)
    if(index(prices)[1]>Sys.Date() - 365*4 -40+2){
      printf("ticker %s not sufficiently old for ADX start date %s required: %s", ticker_new[i], as.Date(index(prices)[1]), as.Date(Sys.Date() - 365*4-40))
      remove_ticker<-c(remove_ticker, ticker_new[i])
    }
    else{
      stock.adx<-ADX(prices, maType = "EMA")
      all.adx<-merge(all.adx, stock.adx$ADX, all=TRUE)
    }
    if((nrow(all.adx)-length(stock.adx))!=0)
      printf("WARNING:ADX length for %s is %f while should be %f",ticker_new[i],length(stock.adx),nrow(all.adx))
    # all.data$temp<-prices$Close
  }
}
colnames(all.adx) <- ticker_new[! ticker_new %in% remove_ticker]
printf("Replacing NA values")
all.adx<-merge(all.adx, zoo( ,seq(start(all.adx),end(all.adx),by="day")) , all=TRUE)
all.adx<-na.locf(all.adx, na.rm=TRUE)
write.zoo(all.adx, file = paste(work.dir, "all-adx-backtest-", format(Sys.Date(), "%Y%m%d"),".csv", sep=""), append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
all.adx<-as.xts(read.zoo(file = paste(work.dir, "all-adx-backtest-", format(Sys.Date(), "%Y%m%d"),".csv", sep=""),  sep = ",", header=TRUE, format="%Y-%m-%d", FUN=as.Date))


sectors<-read.csv(sectors.file, header=TRUE)

period1<-365
period2<-182
backtest.holding.period<-90
r.gap<-1.5
r.cap<-4
halflife.threshold<-25
pvalue.threshold<-0.6
invest.amount<-750000
stock1<-NULL
stock2<-NULL
trading.dates<-index(all.data)
first.write.pair<-TRUE
first.write.trade<-TRUE

pairs.file<-paste(work.dir, "pairs-", format(Sys.Date(), "%Y%m%d"),".csv", sep="")
trades.file<-paste(work.dir, "trades-", format(Sys.Date(), "%Y%m%d"),".csv", sep="")

nifty.rsi<-RSI(Quandl('NSE/CNX_NIFTY', start_date=trading.dates[1] -60, end_date=tail(trading.dates, n=1), type="xts")$Close)
nifty.rsi<-merge(nifty.rsi, zoo( ,seq(start(nifty.rsi),end(nifty.rsi),by="day")) , all=TRUE)
nifty.rsi<-na.locf(nifty.rsi,na.rm = TRUE)

nifty.adx<-ADX(Quandl('NSE/CNX_NIFTY', start_date=trading.dates[1] -60, end_date=tail(trading.dates, n=1), type="xts"), maType = "EMA")
nifty.adx<-merge(nifty.adx, zoo( ,seq(start(nifty.adx),end(nifty.adx),by="day")) , all=TRUE)
nifty.adx<-na.locf(nifty.adx,na.rm = TRUE)


for(i in 1:(length(colnames(all.data))-1)){
  for(j in (i+1):length(colnames(all.data))){
    stock1<-colnames(all.data)[i]
    stock2<-colnames(all.data)[j]
    if(sectors$SECTOR[sectors$STOCK==stock1] == sectors$SECTOR[sectors$STOCK==stock2]){
      pair.summary<-data.frame(stringsAsFactors = FALSE)
      trade.summary<-data.frame(stringsAsFactors = FALSE)
      for(start.date in trading.dates[trading.dates>=as.Date(trading.dates[1]+period1) & trading.dates<=as.Date(trading.dates[length(trading.dates)]-backtest.holding.period)]){
        tryCatch({
          start.date<-as.Date(start.date)
          printf("trying %s and %s starting on %s", stock1, stock2, start.date)
          prices1<-as.data.frame(all.data[paste(start.date-period1,start.date, sep="::"), stock1])
          prices2<-as.data.frame(all.data[paste(start.date-period1,start.date, sep="::"), stock2])
          eg<-egcm(prices1, prices2, include.const=FALSE)
          o<-capture.output(summary(eg))
          vec<-strsplit(grep('ADF', o, value=TRUE)," ")
          adf<-as.numeric(vec[[1]][length(vec[[1]])])
          vec<-strsplit(grep('PP', o, value=TRUE)," ")
          pp<-as.numeric(vec[[1]][length(vec[[1]])])
          vec<-strsplit(grep('PGFF', o, value=TRUE)," ")
          pgff<-as.numeric(vec[[1]][length(vec[[1]])])
          vec<-strsplit(grep('ERSD', o, value=TRUE)," ")
          ersd<-as.numeric(vec[[1]][length(vec[[1]])])
          vec<-strsplit(grep('JOT', o, value=TRUE)," ")
          jot<-as.numeric(vec[[1]][length(vec[[1]])])
          vec<-strsplit(grep('SPR', o, value=TRUE)," ")
          spr<-as.numeric(vec[[1]][length(vec[[1]])])
          half.life<-log(2)/(1-eg$rho)
          r.last<-as.numeric(tail(eg$residuals,1))/eg$residuals.sd
          beta1<-eg$beta
          pair.list<-list(stock1, stock2, format(start.date, "%Y-%m-%d"), eg$beta, eg$residuals.sd, half.life, is.cointegrated(eg), adf, pp, pgff, ersd, jot, spr, r.last)
          
          prices1<-as.data.frame(all.data[paste(start.date-period2,start.date, sep="::"), stock1])
          prices2<-as.data.frame(all.data[paste(start.date-period2,start.date, sep="::"), stock2])
          eg2<-egcm(prices1, prices2, include.const=FALSE)
          o<-capture.output(summary(eg2))
          vec<-strsplit(grep('ADF', o, value=TRUE)," ")
          adf2<-as.numeric(vec[[1]][length(vec[[1]])])
          vec<-strsplit(grep('PP', o, value=TRUE)," ")
          pp2<-as.numeric(vec[[1]][length(vec[[1]])])
          vec<-strsplit(grep('PGFF', o, value=TRUE)," ")
          pgff2<-as.numeric(vec[[1]][length(vec[[1]])])
          vec<-strsplit(grep('ERSD', o, value=TRUE)," ")
          ersd2<-as.numeric(vec[[1]][length(vec[[1]])])
          vec<-strsplit(grep('JOT', o, value=TRUE)," ")
          jot2<-as.numeric(vec[[1]][length(vec[[1]])])
          vec<-strsplit(grep('SPR', o, value=TRUE)," ")
          spr2<-as.numeric(vec[[1]][length(vec[[1]])])
          half.life2<-log(2)/(1-eg2$rho)
          r.last2<-as.numeric(tail(eg2$residuals,1))/eg2$residuals.sd
          beta2<-eg2$beta
          pair.list<-c(pair.list, eg2$beta, eg2$residuals.sd, half.life2, is.cointegrated(eg2), adf2, pp2, pgff2, ersd2, jot2, spr2, r.last2)
          pair.list<-c(pair.list, as.numeric(all.rsi[start.date, stock1]), as.numeric(all.rsi[start.date, stock2]), as.numeric(all.adx[start.date, stock1]), as.numeric(all.adx[start.date, stock2]))
          pair.summary<-rbind.data.frame(pair.summary, pair.list, stringsAsFactors = FALSE)
          # print(pair.list)
          
          #check if we can enter a trade
          if((abs(r.last)>r.gap | abs(r.last2)>r.gap) & ((r.last<0 & r.last2<0) | (r.last>0 & r.last2>0)) & (abs(r.last)<r.cap & abs(r.last2)<r.cap) & (half.life<halflife.threshold & half.life2<halflife.threshold) & pp<pvalue.threshold & pp2<pvalue.threshold & adf<pvalue.threshold & adf2<pvalue.threshold){
            direction<-ifelse(r.last>0, 1 , 2)  #direction specifies the stock to be long
            entry.price1<-as.numeric(tail(prices1, 1))
            quantity1<-ifelse(direction==1,1,-1)* invest.amount/entry.price1
            entry.price2<-as.numeric(tail(prices2, 1))
            quantity2<-ifelse(direction==2,1,-1)* invest.amount/entry.price2
            profit<-0
            holding.period<-0
            clean.exit<-TRUE
            stop.loss.exit<-FALSE
            printf("Entered trade for stock %s and %s, date %s, price1 %f, price2 %f, qnt1 %f, qnt2 %f, rlast %f and %f, direction %d",stock1, stock2, start.date, entry.price1, entry.price2, quantity1, quantity2, r.last, r.last2, direction )
            #check for exit each day
            trade.list<-list()
            for(holding_date in trading.dates[trading.dates>=start.date & trading.dates<=as.Date(trading.dates[length(trading.dates)])]){
              holding_date<-as.Date(holding_date)
              # holding.prices1<-as.data.frame(all.data[paste(holding_date-period1,holding_date, sep="::"), stock1])
              # holding.prices2<-as.data.frame(all.data[paste(holding_date-period1,holding_date, sep="::"), stock2])
              # holding.eg1<-egcm(holding.prices1, holding.prices2, include.const=FALSE)
              # holding.r.last1<-as.numeric(tail(holding.eg1$residuals,1))/holding.eg1$residuals.sd
              # 
              # holding.prices1<-as.data.frame(all.data[paste(holding_date-period2,holding_date, sep="::"), stock1])
              # holding.prices2<-as.data.frame(all.data[paste(holding_date-period2,holding_date, sep="::"), stock2])
              # holding.eg2<-egcm(holding.prices1, holding.prices2, include.const=FALSE)
              # holding.r.last2<-as.numeric(tail(holding.eg2$residuals,1))/holding.eg2$residuals.sd
              
              holding.r.last1<-(as.numeric(all.data[holding_date,stock2]) - beta1*as.numeric(all.data[holding_date,stock1]))/eg$residuals.sd
              holding.r.last2<-(as.numeric(all.data[holding_date,stock2]) - beta2*as.numeric(all.data[holding_date,stock1]))/eg2$residuals.sd
              
              #See if we can exit
              if((r.last>0 & (holding.r.last1<0 | holding.r.last2<0))  |  (r.last<0 & (holding.r.last1>0 | holding.r.last2>0)) | (r.last<0 & (holding.r.last1<(-r.cap) | holding.r.last2<(-r.cap))) | (r.last>0 & (holding.r.last1<r.cap | holding.r.last2<r.cap)) | holding_date== as.Date(trading.dates[length(trading.dates)])){
                if(holding_date== as.Date(trading.dates[length(trading.dates)])){clean.exit<-FALSE }
                if((r.last<0 & (holding.r.last1<(-r.cap) | holding.r.last2<(-r.cap))) | (r.last>0 & (holding.r.last1<r.cap | holding.r.last2<r.cap))){stop.loss.exit<-TRUE}
                exit.price1<-as.numeric(all.data[holding_date,stock1])
                exit.price2<-as.numeric(all.data[holding_date,stock2])
                profit<-(exit.price1-entry.price1)*quantity1 + (exit.price2-entry.price2)*quantity2
                holding.period<-as.numeric(holding_date-start.date)
                long.stock<-ifelse(direction==1,stock1,stock2)
                short.stock<-ifelse(direction==1,stock2,stock1)
                printf("Exiting trade for stock %s and %s, date %s, price1 %f, price2 %f, qnt1 %f, qnt2 %f, rlast %f and %f, direction %d, holding period %d, profit %f and clean exit %d",stock1, stock2, holding_date, exit.price1, exit.price2, quantity1, quantity2, holding.r.last1, holding.r.last2, direction, holding.period, profit, clean.exit )
                trade.list<-list(as.character(start.date), as.character(holding_date), long.stock, short.stock, ifelse(direction==1,quantity1,quantity2), ifelse(direction==1,quantity2,quantity1),
                                 ifelse(direction==1,entry.price1,entry.price2), ifelse(direction==1,entry.price2,entry.price1) , ifelse(direction==1,exit.price1,exit.price2) , ifelse(direction==1,exit.price2,exit.price1),
                                 beta1, beta2, r.last, r.last2, holding.r.last1, holding.r.last2, pp, pp2, adf, adf2, pgff, pgff2, ersd, ersd2, jot, jot2, spr, spr2, half.life, half.life2, eg$residuals.sd, eg2$residuals.sd, holding.period, profit, clean.exit, stop.loss.exit,
                                 as.numeric(nifty.adx[as.character(start.date), "ADX"]), as.numeric(nifty.rsi[as.character(start.date), 1]), as.numeric(all.rsi[start.date, long.stock]), as.numeric(all.rsi[start.date, short.stock]), as.numeric(all.adx[start.date, long.stock]), as.numeric(all.adx[start.date, short.stock]))
                trade.summary<-rbind.data.frame(trade.summary, trade.list, stringsAsFactors = FALSE)
                break
              }
            }
          }
        }, error=function(e){printf("***ERROR : %s",conditionMessage(e))
          message(e)})
      }
      if(nrow(pair.summary)>0){colnames(pair.summary)<-c('stock1', 'stock2', 'date', 'beta1', 'residual_sd1', 'half_life1', 'is_conintegrated1', 'adf1', 'pp1', 'pgff1', 'ersd1', 'jot1', 'spr1', 'r.last1',
                                                         'beta2', 'residual_sd2', 'half_life2', 'is_conintegrated2', 'adf2', 'pp2', 'pgff2', 'ersd2', 'jot2', 'spr2', 'r.last2', 'rsi1', 'rsi2', 'adx1', 'adx2')}
      if(nrow(trade.summary)>0){colnames(trade.summary)<-c('start_date', 'end_date', 'long_stock', 'short_stock', 'long_qty', 'short_qty', 'entry_price_long', 'entry_price_short', 'exit_price_long', 'exit_price_short',
                                                           'beta1', 'beta2', 'entry_r1', 'entry_r2', 'end_r1', 'end_r2' , 'pp1', 'pp2', 'adf1', 'adf2', 'pgff', 'pgff2', 'ersd', 'ersd2', 'jot', 'jot2', 'spr', 'spr2', 'half_life1', 'half_life2', 'residual_sd1', 'residual_sd2', 'holding_period', 'profit', 'clean_exit', 'stop_loss',
                                                           'nifty_adx', 'nifty_rsi', 'long_rsi', 'short_rsi', 'long_adx', 'short_adx')}
      if(first.write.pair){
        if(nrow(pair.summary)>0) {
          write.table(pair.summary, file = pairs.file , append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
          first.write.pair=FALSE
        }
      }
      else{
        if(nrow(pair.summary)>0) {write.table(pair.summary, file = pairs.file , append = TRUE, col.names = FALSE, sep = ",", row.names=FALSE)}
      }
      if(first.write.trade){
        if(nrow(trade.summary)>0) {
          write.table(trade.summary, file = trades.file , append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
          first.write.trade=FALSE
        }
      }
      else{
        if(nrow(trade.summary)>0) {write.table(trade.summary, file = trades.file , append = TRUE, col.names = FALSE, sep = ",", row.names=FALSE)}
      }
      printf("****WRITING INTO FILE FOR STOCK %s and %s *****", stock1, stock2)
    }
  }    
}


