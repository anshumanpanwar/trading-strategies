library(Quandl)
library(httr)
library(egcm)
library(zoo)
library(stats)
library(TTR)
library(caret)
library(quantmod)


Quandl.api_key("rAHKcgv_ymH7_o9s5nWN")
# data <- Quandl("TC1/HDFC")
# data <- Quandl("NSE/ITC")
work.dir<-"C:/Work/trading-strategies/Stocks/"
corp.action.file<-paste("C:/Work/trading-strategies/Pair/","Corporate_Actions.csv" , sep="")
data.dir<-paste(work.dir,"data/" , sep="")
start.time<-Sys.time()

printf <- function(...)print(sprintf(...))

time.keeper<-function(task){
  end.time<-Sys.time()
  printf("++++ %s time taken", task)
  print(end.time-start.time)
  printf("Started at %s, ended at %s", start.time, end.time)
  start.time<<-end.time  #This is the way to subsitute a global variable
}

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
      message("Price freshly extracted and written to the file\n")
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

corp.actions <- read.csv(corp.action.file, header=TRUE,stringsAsFactors = FALSE)
corp.actions <-set.corp.action.data(corp.actions)
# data<-adjust.prices("GODREJCP",'2016-01-01', '2017-05-23', corp.actions)

tickers_100<-c('ABB.NS','ACC.NS','ADANIPORTS.NS','AMBUJACEM.NS','ASHOKLEY.NS','ASIANPAINT.NS','AUROPHARMA.NS','AXISBANK.NS','BAJAJ-AUTO.NS','BAJFINANCE.NS','BAJAJFINSV.NS','BANKBARODA.NS','BEL.NS','BHEL.NS','BPCL.NS','BHARTIARTL.NS','INFRATEL.NS','BOSCHLTD.NS','BRITANNIA.NS','CADILAHC.NS','CIPLA.NS','COALINDIA.NS','COLPAL.NS','CONCOR.NS','CUMMINSIND.NS','DLF.NS','DABUR.NS','DIVISLAB.NS','DRREDDY.NS','EICHERMOT.NS','EMAMILTD.NS','GAIL.NS','GSKCONS.NS','GLAXO.NS','GLENMARK.NS','GODREJCP.NS','GRASIM.NS','HCLTECH.NS','HDFCBANK.NS','HAVELLS.NS','HEROMOTOCO.NS','HINDALCO.NS','HINDPETRO.NS','HINDUNILVR.NS','HINDZINC.NS','HDFC.NS','ITC.NS','ICICIBANK.NS','ICICIPRULI.NS','IDEA.NS','IBULHSGFIN.NS','IOC.NS','INDUSINDBK.NS','INFY.NS','INDIGO.NS','JSWSTEEL.NS','KOTAKBANK.NS','LICHSGFIN.NS','LT.NS','LUPIN.NS','M&M.NS','MARICO.NS','MARUTI.NS','MOTHERSUMI.NS','NHPC.NS','NMDC.NS','NTPC.NS','ONGC.NS','OIL.NS','OFSS.NS','PETRONET.NS','PIDILITIND.NS','PEL.NS','PFC.NS','POWERGRID.NS','PGHH.NS','PNB.NS','RELIANCE.NS','RECLTD.NS','SHREECEM.NS','SRTRANSFIN.NS','SIEMENS.NS','SBIN.NS','SAIL.NS','SUNPHARMA.NS','TCS.NS','TATAMTRDVR.NS','TATAMOTORS.NS','TATAPOWER.NS','TATASTEEL.NS','TECHM.NS','TITAN.NS','TORNTPHARM.NS','UPL.NS','ULTRACEMCO.NS','UBL.NS','MCDOWELL-N.NS','VEDL.NS','WIPRO.NS','YESBANK.NS','ZEEL.NS')
ignore_100<-c('ADANIPORTS.NS','ASIANPAINT.NS','AUROPHARMA.NS','BAJAJ-AUTO.NS','BAJAJFINSV.NS','BANKBARODA.NS','BHARTIARTL.NS','HEROMOTOCO.NS','ICICIPRULI.NS','M&M.NS','TORNTPHARM.NS','ULTRACEMCO.NS','MCDOWELL-N.NS')
tickers_200<-c('ABB.NS','ACC.NS','AIAENG.NS','ADANIENT.NS','ADANIPORTS.NS','ADANIPOWER.NS','ABFRL.NS','ABIRLANUVO.NS','AJANTPHARM.NS','APLLTD.NS','ALKEM.NS','AMARAJABAT.NS','AMBUJACEM.NS','APOLLOHOSP.NS','APOLLOTYRE.NS','ARVIND.NS','ASHOKLEY.NS','ASIANPAINT.NS','AUROPHARMA.NS','AXISBANK.NS','BAJAJ-AUTO.NS','BAJFINANCE.NS','BAJAJFINSV.NS','BANKBARODA.NS','BANKINDIA.NS','BATAINDIA.NS','BERGEPAINT.NS','BEL.NS','BHARATFIN.NS','BHARATFORG.NS','BHEL.NS','BPCL.NS','BHARTIARTL.NS','INFRATEL.NS','BIOCON.NS','BOSCHLTD.NS','BRITANNIA.NS','CESC.NS','CRISIL.NS','CADILAHC.NS','CANBK.NS','CASTROLIND.NS','CENTRALBK.NS','CENTURYTEX.NS','CHOLAFIN.NS','CIPLA.NS','COALINDIA.NS','COLPAL.NS','CONCOR.NS','COROMANDEL.NS','CROMPTON.NS','CUMMINSIND.NS','DLF.NS','DABUR.NS','DALMIABHA.NS','DHFL.NS','DISHTV.NS','DIVISLAB.NS','LALPATHLAB.NS','DRREDDY.NS','EDELWEISS.NS','EICHERMOT.NS','EMAMILTD.NS','ENDURANCE.NS','ENGINERSIN.NS','EXIDEIND.NS','FEDERALBNK.NS','FORTIS.NS','GAIL.NS','GMRINFRA.NS','GSKCONS.NS','GLAXO.NS','GLENMARK.NS','GODREJCP.NS','GODREJIND.NS','GRASIM.NS','GRUH.NS','GPPL.NS','GSPL.NS','HCLTECH.NS','HDFCBANK.NS','HAVELLS.NS','HEROMOTOCO.NS','HEXAWARE.NS','HINDALCO.NS','HINDPETRO.NS','HINDUNILVR.NS','HINDZINC.NS','HDFC.NS','ITC.NS','ICICIBANK.NS','ICICIPRULI.NS','IDBI.NS','IDFCBANK.NS','IDFC.NS','IRB.NS','IDEA.NS','IBULHSGFIN.NS','INDIANB.NS','INDHOTEL.NS','IOC.NS','IGL.NS','INDUSINDBK.NS','NAUKRI.NS','INFY.NS','INDIGO.NS','IPCALAB.NS','JSWENERGY.NS','JSWSTEEL.NS','JINDALSTEL.NS','JUBLFOOD.NS','JUBILANT.NS','KARURVYSYA.NS','KOTAKBANK.NS','L&TFH.NS','LTTS.NS','LICHSGFIN.NS','LT.NS','LUPIN.NS','MRF.NS','M&MFIN.NS','M&M.NS','MANAPPURAM.NS','MARICO.NS','MARUTI.NS','MINDTREE.NS','MOTHERSUMI.NS','MPHASIS.NS','MUTHOOTFIN.NS','NATCOPHARM.NS','NBCC.NS','NHPC.NS','NMDC.NS','NTPC.NS','NATIONALUM.NS','OBEROIRLTY.NS','ONGC.NS','OIL.NS','OFSS.NS','PCJEWELLER.NS','PIIND.NS','PAGEIND.NS','PETRONET.NS','PIDILITIND.NS','PEL.NS','PFC.NS','POWERGRID.NS','PRESTIGE.NS','PGHH.NS','PNB.NS','QUESS.NS','RBLBANK.NS','RAJESHEXPO.NS','RELCAPITAL.NS','RCOM.NS','RELIANCE.NS','RELINFRA.NS','RPOWER.NS','RECLTD.NS','SRF.NS','SHREECEM.NS','SRTRANSFIN.NS','SIEMENS.NS','SBIN.NS','SAIL.NS','STAR.NS','SPARC.NS','SUNPHARMA.NS','SUNTV.NS','SUZLON.NS','SYNDIBANK.NS','SYNGENE.NS','TV18BRDCST.NS','TVSMOTOR.NS','TATACHEM.NS','TATACOMM.NS','TCS.NS','TATAGLOBAL.NS','TATAMTRDVR.NS','TATAMOTORS.NS','TATAPOWER.NS','TATASTEEL.NS','TECHM.NS','RAMCOCEM.NS','THERMAX.NS','TITAN.NS','TORNTPHARM.NS','TORNTPOWER.NS','UPL.NS','ULTRACEMCO.NS','UNIONBANK.NS','UBL.NS','MCDOWELL-N.NS','VAKRANGEE.NS','VEDL.NS','VOLTAS.NS','WELSPUNIND.NS','WIPRO.NS','WOCKPHARMA.NS','YESBANK.NS','ZEEL.NS')
ignore_200<-c('BHARATFIN.NS', 'WOCKPHARMA.NS', 'GRUH.NS','BAJAJ-AUTO.NS', 'MCDOWELL-N.NS','CROMPTON.NS', 'ENDURANCE.NS', 'ICICIPRULI.NS', 'L&TFH.NS', 'LTTS.NS', 'M&MFIN.NS', 'M&M.NS', 'QUESS.NS', 'RBLBANK.NS')
tickers.with.futures<-c('CNX_NIFTY.NS', 'NIFTY_BANK.NS','NIFTY_IT.NS','NIFTY_INFRA.NS','NIFTY_PSE.NS','NIFTY_CPSE.NS','NIFTY_MIDCAP_50.NS', 'ABIRLANUVO.NS', 'OFSS.NS' , 'ACC.NS', 'ADANIENT.NS', 'AJANTPHARM.NS', 'ALBK.NS', 'AMARAJABAT.NS', 'ANDHRABANK.NS', 'APOLLOHOSP.NS', 'ARVIND.NS', 'AUROPHARMA.NS', 'AXISBANK.NS', 'BAJFINANCE.NS', 'BANKBARODA.NS', 'BANKINDIA.NS', 'BEML.NS', 'BHARATFIN.NS', 'BHARATFORG.NS', 'BHARTIARTL.NS', 'BOSCHLTD.NS', 'BPCL.NS', 'CADILAHC.NS', 'CESC.NS', 'CIPLA.NS', 'COLPAL.NS', 'CONCOR.NS', 'CGPOWER.NS', 'CUMMINSIND.NS', 'DABUR.NS', 'DCBBANK.NS', 'DISHTV.NS', 'DIVISLAB.NS', 'DLF.NS', 'DRREDDY.NS', 'EICHERMOT.NS', 'ASHOKLEY.NS', 'APOLLOTYRE.NS', 'ENGINERSIN.NS', 'EXIDEIND.NS', 'FEDERALBNK.NS', 'GRANULES.NS', 'HAVELLS.NS', 'HEXAWARE.NS', 'HINDPETRO.NS', 'HINDUNILVR.NS', 'HINDZINC.NS', 'ICIL.NS', 'IDBI.NS', 'BRITANNIA.NS', 'IDEA.NS', 'INDUSINDBK.NS', 'INFRATEL.NS', 'INFY.NS', 'BHEL.NS', 'JETAIRWAYS.NS', 'CAPF.NS', 'JINDALSTEL.NS', 'CENTURYTEX.NS', 'ADANIPORTS.NS', 'JSWENERGY.NS', 'JUSTDIAL.NS', 'KOTAKBANK.NS', 'KSCL.NS', 'L&TFH.NS', 'LT.NS', 'MARICO.NS', 'MARUTI.NS', 'MCDOWELL-N.NS', 'MINDTREE.NS', 'MRF.NS', 'NMDC.NS', 'ORIENTBANK.NS', 'COALINDIA.NS', 'PAGEIND.NS', 'PETRONET.NS', 'POWERGRID.NS', 'PTC.NS', 'RECLTD.NS', 'AMBUJACEM.NS', 'RELIANCE.NS', 'RPOWER.NS', 'SINTEX.NS', 'SYNDIBANK.NS', 'DHFL.NS', 'TATACHEM.NS', 'TATACOMM.NS', 'TATAELXSI.NS', 'EQUITAS.NS', 'TATASTEEL.NS', 'ASIANPAINT.NS', 'GODREJCP.NS', 'GODREJIND.NS', 'GRASIM.NS', 'HDFC.NS', 'BAJAJFINSV.NS', 'TITAN.NS', 'TORNTPHARM.NS', 'HDFCBANK.NS', 'TORNTPOWER.NS', 'HEROMOTOCO.NS', 'TV18BRDCST.NS', 'HINDALCO.NS', 'TVSMOTOR.NS', 'ULTRACEMCO.NS', 'VOLTAS.NS', 'BALRAMCHIN.NS', 'ICICIBANK.NS', 'IGL.NS', 'INDIANB.NS', 'WOCKPHARMA.NS', 'YESBANK.NS', 'INDIGO.NS', 'ZEEL.NS', 'BIOCON.NS', 'INFIBEAM.NS', 'CANBK.NS', 'BAJAJ_AUTO.NS', 'BATAINDIA.NS', 'IOC.NS', 'ITC.NS', 'CASTROLIND.NS', 'JISLJALEQS.NS', 'CEATLTD.NS', 'JSWSTEEL.NS', 'JUBLFOOD.NS', 'CHOLAFIN.NS', 'GLENMARK.NS', 'HDIL.NS', 'DALMIABHA.NS', 'IBULHSGFIN.NS', 'INDIACEM.NS', 'JPASSOCIAT.NS', 'MFSL.NS', 'MOTHERSUMI.NS', 'FORTIS.NS', 'GODFRYPHLP.NS', 'GSFC.NS', 'KTKBANK.NS', 'HCLTECH.NS', 'IBREALEST.NS', 'M&M.NS', 'IDFC.NS', 'IFCI.NS', 'NCC.NS', 'NTPC.NS', 'IRB.NS', 'RCOM.NS', 'RELCAPITAL.NS', 'SUNTV.NS', 'TCS.NS', 'M&MFIN.NS', 'MGL.NS', 'GAIL.NS', 'NIITTECH.NS', 'OIL.NS', 'MRPL.NS', 'PEL.NS', 'PFC.NS', 'MUTHOOTFIN.NS', 'PIDILITIND.NS', 'ONGC.NS', 'PNB.NS', 'PVR.NS', 'RNAVAL.NS', 'SAIL.NS', 'SBIN.NS', 'SHREECEM.NS', 'RELINFRA.NS', 'RAYMOND.NS', 'SIEMENS.NS', 'SOUTHBANK.NS', 'SRF.NS', 'SRTRANSFIN.NS', 'SUNPHARMA.NS', 'TATAGLOBAL.NS', 'TATAMOTORS.NS', 'TATAMTRDVR.NS', 'UNIONBANK.NS', 'VEDL.NS', 'STAR.NS', 'UJJIVAN.NS', 'ESCORTS.NS', 'UBL.NS', 'UPL.NS', 'WIPRO.NS', 'BALKRISIND.NS', 'CANFINHOME.NS', 'IDFCBANK.NS', 'KPIT.NS', 'TATAPOWER.NS', 'GMRINFRA.NS', 'LUPIN.NS', 'BEL.NS', 'LICHSGFIN.NS', 'ADANIPOWER.NS', 'TECHM.NS', 'MCX.NS', 'NBCC.NS', 'PCJEWELLER.NS', 'BERGEPAINT.NS', 'NESTLEIND.NS', 'VGUARD.NS', 'NHPC.NS', 'SUZLON.NS')
ignore.tickers.with.futures<-c('BHARATFIN.NS', 'WOCKPHARMA.NS','CGPOWER.NS', 'MGL.NS', 'ABIRLANUVO.NS', 'RNAVAL.NS')
ticker_new<-setdiff(tickers.with.futures, ignore.tickers.with.futures)
# ticker_new<-c("CAPF.NS", "IDFCBANK.NS", "COLPAL.NS")

years<-5

nifty<-Quandl("NSE/CNX_NIFTY", start_date=Sys.Date() - 365*years, end_date=Sys.Date(), type='xts')

remove_ticker<-c()
for(i in 1:length(ticker_new)){
  prices <- adjust.prices(substr(ticker_new[i], 1, nchar(ticker_new[i])-3), Sys.Date() - 365*years, Sys.Date(),corp.actions )
  # showConnections()
  if(i==1) {
    all.data<-prices$adj.close
    all.adx<-ADX(normalize.prices(prices[paste(as.character(Sys.Date()-70), as.character(Sys.Date()), sep="/")]), maType = "EMA")$ADX
  }
  else {
    # all.data$temp <- c(rep(NA, nrow(all.data)-length(prices$adj.close)), prices$adj.close) 
    all.data<-merge(all.data,prices$adj.close,all=TRUE)
    stock.adx<-ADX(normalize.prices(prices[paste(as.character(Sys.Date()-70), as.character(Sys.Date()), sep="/")]), maType = "EMA")
    all.adx<-merge(all.adx, stock.adx$ADX, all=TRUE)
    if((nrow(all.data)-length(prices$adj.close))!=0)
      printf("WARNING:Data length for %s is %f while should be %f",ticker_new[i],length(prices$adj.close),nrow(all.data))
    # all.data$temp<-prices$Close
    if((nrow(all.adx)-length(stock.adx$ADX))!=0)
      printf("WARNING:ADX length for %s is %f while should be %f",ticker_new[i],length(stock.adx$ADX),nrow(all.adx))
    
  }
}
colnames(all.data) <- ticker_new
colnames(all.adx) <- ticker_new[! ticker_new %in% remove_ticker]
printf("Replacing NA values")
all.adx<-merge(all.adx, zoo( ,seq(start(all.adx),end(all.adx),by="day")) , all=TRUE)
all.adx<-na.locf(all.adx, na.rm=TRUE)

printf("Replacing NA values")
all.data<-na.locf(all.data)

#Cross verify if all Adjustments have been handled well
for(j in 1: ncol(all.data)){
  printf("Checking for %s with %d rows....", colnames(all.data)[j], nrow(all.data[,j]))
  for(i in 1:(nrow(all.data[,j])-1)){
    # printf("i is %d j is %d",i,j)
    if(is.na(all.data[i,j]) | is.na(all.data[i+1,j])) next;
    if((as.numeric(all.data[i,j])/as.numeric(all.data[i+1,j]))<0.8 | (as.numeric(all.data[i,j])/as.numeric(all.data[i+1,j]))>1.2)
      printf("Ratio for %s at date %s is %f",colnames(all.data)[j], as.character(index(all.data)[i]), (as.numeric(all.data[i,j])/as.numeric(all.data[i+1,j])))
  }
}

write.zoo(all.data, file = paste(work.dir, "all-data-", format(Sys.Date(), "%Y%m%d"),".csv", sep=""), append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
# write.zoo(all.data180, file = paste(work.dir,"all-data-180", format(Sys.Date(), "%Y%m%d"),".csv", sep=""), append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
# all.data<-read.csv(paste(work.dir,"all-data-", format(Sys.Date(), "%Y%m%d"),".csv", sep=""), header = TRUE, check.names=FALSE)

results<- data.frame(stringsAsFactors = FALSE)
periodsInYear<-12
for(i in colnames(all.data)){
  prices<-all.data[,i]
  returns<-NULL
  if(periodsInYear==12){
    returns<-monthlyReturn(prices,type = 'log', leading=FALSE)
  }
  if(periodsInYear==4){
    returns<-quarterlyReturn(prices,type = 'log', leading=FALSE)
  }
  if(periodsInYear==1){
    returns<-annualReturn(prices,type = 'log', leading=FALSE)
  }
  avg<-mean(returns[,1], na.rm=TRUE)
  stdev1<-sd(returns[,1], na.rm=TRUE)
  sharpe<-(avg-log(1+0.07/periodsInYear))/stdev1
  mar<-log(1+0.07/periodsInYear)
  avg<-mean(returns[,1], na.rm=TRUE)
  stdev2<-sqrt(sum((mar-returns[,1][returns[,1]<mar])^2, na.rm=TRUE)/max(1,nrow(returns[returns[,1]<mar,1])))
  sortino<-(avg-mar)/stdev2
  pos<-nrow(returns[,1][returns[,1]>=mar])
  neg<-nrow(returns[,1][returns[,1]<mar])
  alpha.and.beta<-alpha.beta(prices, nifty)
  alpha.and.beta.monthly<-alpha.beta.monthly(prices, nifty)
  results<-rbind(results, list(i , avg*periodsInYear, stdev1*(periodsInYear^0.5), sharpe*(periodsInYear^0.5), sortino*(periodsInYear^0.5), pos, neg, alpha.and.beta[1]*240, alpha.and.beta[2],alpha.and.beta.monthly[1]*12, alpha.and.beta.monthly[2]), stringsAsFactors = FALSE)
}
colnames(results)<-c( 'Stock', 'Avg', 'Stdev', 'Sharpe', 'Sortino', 'Pos', 'Neg', 'Alpha', 'beta', 'alpha.monthly' , 'beta.monthly')
write.csv(results, file = paste(work.dir, "stock-report-5Y-annual.csv", sep=""), row.names = FALSE, col.names = TRUE)


results<- data.frame(stringsAsFactors = FALSE)
periodsInYear<-12
totalYears<-15
lookbackYears<-2
testYears<-1
for(l in (lookbackYears+testYears):totalYears){
  dataEndDate<-as.Date(tail(index(tmp),n=1))
  lookbackStartDate<-dataEndDate-l*365
  lookbackEndDate<-lookbackStartDate+lookbackYears*365
  testEndDate<-lookbackStartDate+(lookbackYears+testYears)*365
  printf("Lookback from %s and %s , test ending %s", lookbackStartDate, lookbackEndDate, testEndDate)
  lookback.data<-all.data[paste(lookbackStartDate,lookbackEndDate, sep="/")]
  test.data<-all.data[paste(lookbackEndDate,testEndDate, sep="/")]
  nifty.ret1<-mean(monthlyReturn(lookback.data[,"CNX_NIFTY.NS"],type = 'log', leading=FALSE) , na.rm=TRUE)*12
  nifty.ret2<-mean(monthlyReturn(test.data[,"CNX_NIFTY.NS"],type = 'log', leading=FALSE) , na.rm=TRUE)*12
  for(i in colnames(all.data)){
    printf("stock %s",i)
    prices<-lookback.data[,i]
    if(anyNA(prices)){
      next
    }
    returns<-NULL
    if(periodsInYear==12){
      returns<-monthlyReturn(prices,type = 'log', leading=FALSE)
    }
    if(periodsInYear==4){
      returns<-quarterlyReturn(prices,type = 'log', leading=FALSE)
    }
    if(periodsInYear==1){
      returns<-annualReturn(prices,type = 'log', leading=FALSE)
    }
    avg<-mean(returns[,1], na.rm=TRUE)
    stdev1<-sd(returns[,1], na.rm=TRUE)
    sharpe<-(avg-log(1+0.07/periodsInYear))/stdev1
    mar<-log(1+0.07/periodsInYear)
    avg<-mean(returns[,1], na.rm=TRUE)
    stdev2<-sqrt(sum((mar-returns[,1][returns[,1]<mar])^2, na.rm=TRUE)/max(1,nrow(returns[returns[,1]<mar,1])))
    sortino<-(avg-mar)/stdev2
    pos<-nrow(returns[,1][returns[,1]>=mar])
    neg<-nrow(returns[,1][returns[,1]<mar])
    results.list<-list(i, format(testEndDate, "%Y"), avg*periodsInYear, stdev1*(periodsInYear^0.5), sharpe*(periodsInYear^0.5), sortino*(periodsInYear^0.5), pos, neg, avg*periodsInYear-nifty.ret1)
    
    
    prices<-test.data[,i]
    if(anyNA(prices)){
      next
    }
    returns<-NULL
    if(periodsInYear==12){
      returns<-monthlyReturn(prices,type = 'log', leading=FALSE)
    }
    if(periodsInYear==4){
      returns<-quarterlyReturn(prices,type = 'log', leading=FALSE)
    }
    if(periodsInYear==1){
      returns<-annualReturn(prices,type = 'log', leading=FALSE)
    }
    avg<-mean(returns[,1], na.rm=TRUE)
    stdev1<-sd(returns[,1], na.rm=TRUE)
    sharpe<-(avg-log(1+0.07/periodsInYear))/stdev1
    mar<-log(1+0.07/periodsInYear)
    avg<-mean(returns[,1], na.rm=TRUE)
    stdev2<-sqrt(sum((mar-returns[,1][returns[,1]<mar])^2, na.rm=TRUE)/max(1,nrow(returns[returns[,1]<mar,1])))
    sortino<-(avg-mar)/stdev2
    pos<-nrow(returns[,1][returns[,1]>=mar])
    neg<-nrow(returns[,1][returns[,1]<mar])
    results.list<-append(results.list,list(avg*periodsInYear, stdev1*(periodsInYear^0.5), sharpe*(periodsInYear^0.5), sortino*(periodsInYear^0.5), pos, neg, avg*periodsInYear-nifty.ret2))
    
    results<-rbind(results, results.list, stringsAsFactors = FALSE)
  }
}

colnames(results)<-c( 'Stock', 'StartDate', 'Avg', 'Stdev', 'Sharpe', 'Sortino', 'Pos', 'Neg', 'Alpha1', 'Avg2', 'Stdev2', 'Sharpe2', 'Sortino2', 'Pos2', 'Neg2' , 'Alpha1')
write.csv(results, file = paste(work.dir, "stock-report-backtest.csv", sep=""), row.names = FALSE, col.names = TRUE)
