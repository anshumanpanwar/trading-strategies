data.dir<-"D:/Work/Stocks/data/"

printf <- function(...)print(sprintf(...))

adjust.prices<-function(stock.ticker, start_date, end_date, corp.actions){
  printf("Starting for stock %s", stock.ticker)
  stock.ticker.modified=gsub("&","",stock.ticker)
  stock.ticker.modified=gsub("-","_",stock.ticker.modified)
  data <- read.marketdata(stock.ticker, start_date, end_date)
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

bollinger.sd<-function(prices, end_date, period){
  index<-match(end_date, index(prices))
  start_date<-index(prices)[index-min(index-1,period)]
  return (sd(prices$adj.close[paste(start_date,end_date, sep="::")]))
}

bollinger.ma<-function(prices, end_date, period){
  index<-match(end_date, index(prices))
  start_date<-index(prices)[index-min(index-1,period)]
  return (mean(prices$adj.close[paste(start_date,end_date, sep="::")]))
}

normalize.prices<-function(prices){
  data<-(prices$Close)
  data$Close<-prices$adj.close
  data$adj.close<-prices$adj.close
  data$Open<-prices$Open*(prices$adj.close/prices$Close)
  data$High<-prices$High*(prices$adj.close/prices$Close)
  data$Low<-prices$Low*(prices$adj.close/prices$Close)
  return(data)
}

get.macd<-function(prices, fast, slow, signal){
  #MACD construction
  macd  <- MACD( prices$adj.close, fast, slow, signal, maType="EMA", percent=TRUE )
  macd$bars<-macd$macd-macd$signal
  macd$bars.firstderivative<-diff(macd$bars)
  macd$bars.secondderivative<-diff(macd$bars.firstderivative)
  macd$bars.firstderivative.3sma<-SMA(macd$bars.firstderivative, n=3)
  macd$bars.secondderivative.3sma<-SMA(macd$bars.secondderivative, n=3)
  return(macd)
}

beta<-function(prices, nifty){
  # printf("in beta")
  prices
  if(nrow(prices)<2){return(NA)}
  # nifty <-nifty[paste(index(prices)[1],index(prices)[length(index(prices))], sep="::"),]
  nifty<-nifty[index(prices),]
  prices<-prices[index(nifty),]
  ret.prices<-diff(prices$adj.close)/lag(prices$adj.close)
  ret.nifty<-diff(nifty$Close)/lag(nifty$Close)
  ret.prices<-ret.prices[!is.na(ret.prices[,1]),]
  ret.nifty<-ret.nifty[!is.na(ret.nifty[,1]),]
  if(nrow(ret.prices)<nrow(ret.nifty)){ret.nifty<-tail(ret.nifty,nrow(ret.prices))}
  else if(nrow(ret.nifty)<nrow(ret.prices)){ret.prices<-tail(ret.prices,nrow(ret.nifty))}
  if(nrow(ret.prices)<=2){return(1)}
  return(as.numeric(coef(lm(ret.prices[2:nrow(ret.prices)]~ret.nifty[2:nrow(ret.nifty)]))[2]))
}
rsquare<-function(prices,nifty){
  # printf("in beta")
  prices
  if(nrow(prices)<2){return(NA)}
  nifty <- nifty[paste(index(prices)[1],index(prices)[length(index(prices))], sep="::"),]
  nifty<-nifty[index(prices),]
  prices<-prices[index(nifty),]
  ret.prices<-diff(prices$adj.close)/lag(prices$adj.close)
  ret.nifty<-diff(nifty$Close)/lag(nifty$Close)
  ret.prices<-ret.prices[!is.na(ret.prices[,1]),]
  ret.nifty<-ret.nifty[!is.na(ret.nifty[,1]),]
  if(nrow(ret.prices)<nrow(ret.nifty)){ret.nifty<-tail(ret.nifty,nrow(ret.prices))}
  if(nrow(ret.prices)<=2){return(1)}
  return(as.numeric(cor(ret.nifty$Close[2:nrow(ret.nifty)], ret.prices$adj.close[2:nrow(ret.prices)])))
}