library(Quandl)
library(httr)
library(egcm)
library(zoo)
library(stats)
library(TTR)
library(caret)
library(randomForest)
library(xgboost)

Quandl.api_key("rAHKcgv_ymH7_o9s5nWN")
# data <- Quandl("TC1/HDFC")
# data <- Quandl("NSE/ITC")
work.dir<-"C:/Work/trading-strategies/Pair/"
corp.action.file<-paste(work.dir,"Corporate_Actions.csv" , sep="")
data.dir<-paste(work.dir,"data/" , sep="")
sectors.file<-paste(work.dir, "customized-sectors.csv" , sep="")
pair.stats.file<-paste(work.dir, "pair-stats.csv" , sep="")
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


getMLModelScore<-function(stock1, stock2, end_date, model.list){
  period1<-182
  period2<-90
  
  end_date<-as.Date(end_date)
  printf("trying %s and %s starting on %s", stock1, stock2, end_date)
  prices1<-as.data.frame(all.data[paste(end_date-period1,end_date, sep="::"), stock1])
  prices2<-as.data.frame(all.data[paste(end_date-period1,end_date, sep="::"), stock2])
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
  half.life<-ifelse(half.life>100, 100, half.life)
  r.last<-as.numeric(tail(eg$residuals,1))/eg$residuals.sd
  beta1<-eg$beta
  
  prices1<-as.data.frame(all.data[paste(end_date-period2,end_date, sep="::"), stock1])
  prices2<-as.data.frame(all.data[paste(end_date-period2,end_date, sep="::"), stock2])
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
  half.life2<-ifelse(half.life2>100, 100, half.life2)
  r.last2<-as.numeric(tail(eg2$residuals,1))/eg2$residuals.sd
  beta2<-eg2$beta
  
  
  if(r.last>0){
    long.stock<-stock2
    short.stock<-stock1
  }else{
    long.stock<-stock1
    short.stock<-stock2
  }
  
  nifty_adx<-as.numeric(tail(nifty.adx,1)$ADX)
  nifty_rsi<-as.numeric(tail(nifty.rsi,1)[,1])
  long_adx<-as.numeric(tail(all.adx, 1)[,long.stock])
  short_adx<-as.numeric(tail(all.adx, 1)[,short.stock])
  long_rsi<-as.numeric(tail(all.rsi, 1)[,long.stock])
  short_rsi<-as.numeric(tail(all.rsi, 1)[,short.stock])
  sector<-sectors$SECTOR[sectors$STOCK==long.stock]
  pair<-ifelse(as.character(stock1)>as.character(stock2), paste(stock2,stock1,sep="-"), paste(stock1,stock2,sep="-") )
  perc_profitability<-pair.stats$perc_profitability[pair.stats$pair==pair]
  if(identical(perc_profitability, numeric(0))) {perc_profitability<-0.638}
  avg_profit<-pair.stats$avg_profit[pair.stats$pair==pair]
  if(identical(avg_profit, numeric(0))) {avg_profit<-0}
  total_profit<-pair.stats$total_profit[pair.stats$pair==pair]
  if(identical(total_profit, numeric(0))) {total_profit<-0}
  rlast_ratio<-abs((r.last-r.last2)/r.last)
  
  features<-c("entry_r1","entry_r2","pp1","pp2","adf1","adf2","pgff","pgff2","ersd","ersd2","jot","jot2","spr","spr2","half_life1","half_life2","nifty_adx","long_adx","short_adx","nifty_rsi","long_rsi", "short_rsi", "sector", "rlast_ratio", "perc_profitability", "avg_profit", "total_profit")
  
  # features<-c("entry_r1","entry_r2","pp1","pp2","adf1","adf2","pgff","pgff2","ersd","ersd2","jot","jot2","spr","spr2","half_life1","half_life2","nifty_adx","long_adx","short_adx","nifty_rsi","long_rsi", "short_rsi", "sector")
  trade<-list(abs(r.last), abs(r.last2), pp, pp2, adf, adf2, pgff, pgff2, ersd, ersd2, jot, jot2, spr, spr2, half.life, half.life2,
              nifty_adx, long_adx, short_adx, nifty_rsi, long_rsi, short_rsi, sector, rlast_ratio, perc_profitability, avg_profit, total_profit) 
  print(trade)
  trade.row<-rbind.data.frame(data.frame(stringsAsFactors = FALSE), trade, stringsAsFactors = FALSE)
  colnames(trade.row)<-features
  trade.row$sector<-factor(trade.row$sector, levels=model.rf$forest$xlevels$sector)
  # levels(trade.row$sector)<-levels(sectors$SECTOR)
  print(trade.row)
  print(as.numeric(trade.row$sector))
  print(str(trade.row))
  return.value<-list()
  # for(model in model.list){
  #   return.value<-c(return.value, as.numeric(predict(model, trade.row)))
  # }
  return.value<-c(return.value, as.numeric(predict(model.list[1], trade.row)))
  return.value<-c(return.value, family(logit1)$linkinv(as.numeric(predict(model.list[2], trade.row, response= TRUE))))
  return.value<-c(return.value, as.numeric(predict(model.list[3], data.matrix(trade.row))))
  
  return.value<-c(return.value, beta2)
  return.value<-c(return.value, r.last2)
  return.value<-c(return.value, eg2$residuals.sd)
  print(return.value)
  return(return.value)
}

printf <- function(...)print(sprintf(...))

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

remove_ticker<-c()
for(i in 1:length(ticker_new)){
  prices <- adjust.prices(substr(ticker_new[i], 1, nchar(ticker_new[i])-3), Sys.Date() - 365, Sys.Date(),corp.actions )
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
all.data180<-all.data[paste(as.character(Sys.Date()-180), as.character(Sys.Date()), sep="/")]

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


#Calculating RSI for all stocks

remove_ticker<-c()
xts.data<-all.data
for(i in 1:length(ticker_new)){
  # prices <- adjust.prices(substr(ticker_new[i], 1, nchar(ticker_new[i])-3), Sys.Date() - 70, Sys.Date(),corp.actions )
  prices<-xts.data[(nrow(xts.data)-50):nrow(xts.data), ticker_new[i]]
  showConnections()
  if(i==1) {
    all.rsi<-(RSI(prices))
  }
  else {
    stock.rsi<-RSI(prices)
    all.rsi<-merge(all.rsi, (stock.rsi), all=TRUE)
    if((nrow(all.rsi)-length(stock.rsi))!=0)
      printf("WARNING:RSI length for %s is %f while should be %f",ticker_new[i],length(stock.rsi),nrow(all.rsi))
    # all.data$temp<-prices$Close
  }
}
colnames(all.rsi) <- ticker_new[! ticker_new %in% remove_ticker]
printf("Replacing NA values")
all.rsi<-merge(all.rsi, zoo( ,seq(start(all.rsi),end(all.rsi),by="day")) , all=TRUE)
all.rsi<-na.locf(all.rsi, na.rm=TRUE)


#Nifty ADX
nifty.adx<-ADX(Quandl('NSE/CNX_NIFTY', start_date=Sys.Date() -60, end_date=Sys.Date(), type="xts"), maType = "EMA")
nifty.adx<-merge(nifty.adx, zoo( ,seq(start(nifty.adx),end(nifty.adx),by="day")) , all=TRUE)
nifty.adx<-na.locf(nifty.adx,na.rm = TRUE)
#Nifty RSI
nifty.rsi<-RSI(Quandl('NSE/CNX_NIFTY', start_date=Sys.Date() -60, end_date=Sys.Date(), type="xts")$Close)
nifty.rsi<-merge(nifty.rsi, zoo( ,seq(start(nifty.rsi),end(nifty.rsi),by="day")) , all=TRUE)
nifty.rsi<-na.locf(nifty.rsi,na.rm = TRUE)

time.keeper("Preparing all marketdata ")

model.file1<-paste(work.dir, "pair-logit.model", sep="" )
model1<-load(file=model.file1)

model.file2<-paste(work.dir, "pair-rf.model", sep="" )
model2<-load(file=model.file2)

model.file3<-paste(work.dir, "pair-xgb.model", sep="" )
model3<-load(file=model.file3)

main.output<-allpairs.egcm(all.data,
                           startdate = as.numeric(format(Sys.Date() - 365, "%Y%m%d")),
                           enddate = as.numeric(format(Sys.Date(), "%Y%m%d")), p.value=0.10, include.const=FALSE, na.action=na.omit
)
main.output$half.life<-log(2)/(1-main.output$rho)
main.output$r.last<-0.0
main.output$last.price1<-0.0
main.output$last.price2<-0.0
for(i in 1:nrow(main.output)){
  main.output$last.price1[i]<-all.data[nrow(all.data),as.character(main.output$series1[i])]
  main.output$last.price2[i]<-all.data[nrow(all.data),as.character(main.output$series2[i])]
  main.output$r.last[i]<-(all.data[nrow(all.data),as.character(main.output$series2[i])] - main.output$beta[i]*all.data[nrow(all.data),as.character(main.output$series1[i])] - main.output$alpha[i])/main.output$residuals.sd[i]
}
indices<-sample(x = 1:nrow(main.output), size = 2, replace = FALSE)
printf("CONSISTENCY CHECK 1")
main.output[indices, c(1,2)]

time.keeper("All pair EGCM full year ")

output<-allpairs.egcm(all.data,
                      startdate = as.numeric(format(Sys.Date() - 365, "%Y%m%d")),
                      enddate = as.numeric(format(Sys.Date(), "%Y%m%d")), p.value=0.10, i1test="adf", urtest = "adf", include.const=FALSE, na.action=na.omit
)
write.table(output, file = paste(work.dir,"summary-", format(Sys.Date(), "%Y%m%d"),"-adf.csv", sep=""), append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
main.output$r.p.adf.365<-output$r.p
main.output$is.cointegrated.adf.365<-output$is.cointegrated

printf("CONSISTENCY CHECK 2")
output[indices, c(1,2)]
time.keeper("All pair EGCM full year ADF ")

output<-allpairs.egcm(all.data180,
                      startdate = as.numeric(format(Sys.Date() - 180, "%Y%m%d")),
                      enddate = as.numeric(format(Sys.Date(), "%Y%m%d")), p.value=0.10, include.const=FALSE, na.action=na.omit
)

write.table(output, file = paste(work.dir,"summary-180-", format(Sys.Date(), "%Y%m%d"),".csv", sep=""), append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
main.output$r.p.pp.180<-output$r.p
main.output$is.cointegrated.pp.180<-output$is.cointegrated
main.output$beta180<-output$beta
main.output$r.last180<-0.0
main.output$residuals.sd180<-output$residuals.sd
main.output$half.life180<-log(2)/(1-output$rho)
for(i in 1:nrow(main.output)){
  main.output$r.last180[i]<-(all.data[nrow(all.data),as.character(main.output$series2[i])] - main.output$beta180[i]*all.data[nrow(all.data),as.character(main.output$series1[i])] - main.output$alpha[i])/main.output$residuals.sd180[i]
}
printf("CONSISTENCY CHECK 3")
output[indices, c(1,2)]
time.keeper("All pair EGCM Half year ")

output<-allpairs.egcm(all.data180,
                      startdate = as.numeric(format(Sys.Date() - 180, "%Y%m%d")),
                      enddate = as.numeric(format(Sys.Date(), "%Y%m%d")), p.value=0.10, i1test="adf", urtest = "adf", include.const=FALSE, na.action=na.omit
)
write.table(output, file = paste(work.dir,"summary-180-", format(Sys.Date(), "%Y%m%d"),"-adf.csv", sep=""), append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
main.output$r.p.adf.180<-output$r.p
main.output$is.cointegrated.adf.180<-output$is.cointegrated

printf("CONSISTENCY CHECK 4")
output[indices, c(1,2)]

# write.table(main.output, file = paste(work.dir,"summary-", format(Sys.Date(), "%Y%m%d"),".csv", sep=""), append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
time.keeper("All pair EGCM Half year ADF ")

sectors<-read.csv(sectors.file, header  = TRUE)
pair.stats<-read.csv(pair.stats.file, header  = TRUE)

main.output$score.365<-"na"
main.output$sq.score.365<-"na"
main.output$score.180<-"na"
main.output$sq.score.180<-"na"
main.output$model_score1<-"na"
main.output$model_score2<-"na"
main.output$model_score3<-"na"
main.output$beta3<-"na"
main.output$r.last3<-"na"
main.output$residual.sd3<-"na"

all.data<-as.xts(all.data)

for(i in 1:nrow(main.output)){
  printf("index %d, stock %s and %s", i, main.output$series1[i], main.output$series2[i])
  if(identical(as.character(sectors$SECTOR[sectors$STOCK==as.character(main.output$series1[i])]),character(0)) | identical(as.character(sectors$SECTOR[sectors$STOCK==as.character(main.output$series2[i])]),character(0))){
    next
  }
  if(sectors$SECTOR[sectors$STOCK==as.character(main.output$series1[i])] == sectors$SECTOR[sectors$STOCK==as.character(main.output$series2[i])]){
    tryCatch({
      printf("Scores %s, %s",main.output$series1[i], main.output$series2[i])
      lookback<-240
      prices1<-as.data.frame(all.data[,gsub("&|-",".",as.character(main.output$series1[i]))])[(nrow(all.data)-lookback+1):nrow(all.data),1]
      prices2<-as.data.frame(all.data[,gsub("&|-",".",as.character(main.output$series2[i]))])[(nrow(all.data)-lookback+1):nrow(all.data),1]
      eg<-egcm(prices1, prices2, include.const=FALSE)
      moves<-diff(eg$residuals, trim=TRUE)/eg$residuals.sd
      residuals<-eg$residuals[1:length(moves)]/eg$residuals.sd
      main.output$score.365[i]<-sum(moves*residuals)*100/length(moves)
      main.output$sq.score.365[i]<-sum(sign(residuals)*(moves*(residuals^2))) * 100 / length(moves)
      
      lookback<-120
      prices1<-as.data.frame(all.data180[,gsub("&|-",".",as.character(main.output$series1[i]))])[(nrow(all.data180)-lookback+1):nrow(all.data180),1]
      prices2<-as.data.frame(all.data180[,gsub("&|-",".",as.character(main.output$series2[i]))])[(nrow(all.data180)-lookback+1):nrow(all.data180),1]
      eg<-egcm(prices1, prices2, include.const=FALSE)
      moves<-diff(eg$residuals, trim=TRUE)/eg$residuals.sd
      residuals<-eg$residuals[1:length(moves)]/eg$residuals.sd
      main.output$score.180[i]<-sum(moves*residuals)*100/length(moves)
      main.output$sq.score.180[i]<-sum(sign(residuals)*(moves*(residuals^2))) * 100 / length(moves)
    }, error=function(e){printf("Ticker data not found for %s, %s", main.output$series1[i], main.output$series2[i])})
    if(abs(main.output$r.last180)>0){
      tryCatch({
        printf("Evaluating Model %s, %s",main.output$series1[i], main.output$series2[i])
        result.list<-getMLModelScore(as.character(main.output$series1[i]), as.character(main.output$series2[i]), Sys.Date(), list(model.rf, logit1, xgb))
        main.output$model_score1[i]<-result.list[[1]]
        main.output$model_score2[i]<-result.list[[2]]
        main.output$model_score3[i]<-result.list[[3]]
        main.output$beta3[i]<-result.list[[4]]
        main.output$r.last3[i]<-result.list[[5]]
        main.output$residual.sd3[i]<-result.list[[6]]
      }, error=function(e){printf("Model evaluation failed for %s and %s and \n%s", main.output$series1[i], main.output$series2[i], e)})
    }
  }
}
time.keeper("Applying additional statistics and ML models ")

tryCatch(
  {
    write.table(main.output, file = paste(work.dir,"summary-", format(Sys.Date(), "%Y%m%d"),".csv", sep=""), append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
  },error=function(cond) {
    #message(paste("File not found ", stock.ticker,"\n"))
    message("Here's the original error message:")
    message(cond)
    file.name<-paste(work.dir,"summary-", format(Sys.Date(), "%Y%m%d"),as.integer(runif(1, 1, 100)),".csv", sep="")
    write.table(main.output, file = file.name, append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
    message("summary written to following file...")
    file.name
  },warning=function(cond) {
    #message(paste("File not found ", stock.ticker,"\n"))
    message("Warning: Here's the original error message:")
    message(cond)
    file.name<-paste(work.dir,"summary-", format(Sys.Date(), "%Y%m%d"),as.integer(runif(1, 1, 100)),".csv", sep="")
    write.table(main.output, file = file.name, append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
    message("summary written to following file...")
    file.name
  })