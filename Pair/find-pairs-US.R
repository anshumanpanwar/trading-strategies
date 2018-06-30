library(Quandl)
library(devtools)
library(httr)
library(RCurl)
library(quantmod)
library(tseries)
library(zoo)
library(egcm)
library(sqldf)
Quandl.api_key("rAHKcgv_ymH7_o9s5nWN")

data.dir<-"D:/Work/Stocks/US/"
printf <- function(...)print(sprintf(...))

stock.list<-read.csv(paste(data.dir, "US-Stocks.csv", sep=""))
tickers<-stock.list$Ticker.symbol

for(i in 1:length(tickers)){
  # for(i in 1:3){
  printf("starting for %s",tickers[i])
  data <- Quandl.datatable('WIKI/PRICES', ticker=tickers[i], paginate=TRUE)
  data<-data[data$date>(Sys.Date()-365),]
  data<-xts(data$adj_close, data$date)
  colnames(data)<-c(tickers[i])
  if(i==1) {
    all.data<-data[,1]
    names(all.data)[ncol(all.data)]<-as.character(tickers[i])
  }
  else if(nrow(data)>0){
    # all.data$temp <- c(rep(NA, nrow(all.data)-length(prices$adj.close)), prices$adj.close) 
    all.data<-merge(all.data,data[,1],all=TRUE)
    names(all.data)[ncol(all.data)]<-as.character(tickers[i])
    if((nrow(all.data)-length(data[,1]))!=0)
      printf("WARNING:Data length for %s is %f while should be %f",tickers[i],length(data[,1]),nrow(all.data))
    # all.data$temp<-prices$Close
  }
}
all.data1<-all.data
# colnames(all.data) <- tickers
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

write.zoo(all.data, file = paste(data.dir, "data/all-data-US-", format(Sys.Date(), "%Y%m%d"),".csv", sep=""), append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)

#Drop the tickers with incomplete values like the new tickers
incomplete.tickers<-names(all.data)[which(is.na(all.data[1,]))]
printf("dropping following tickers %s", incomplete.tickers)
all.data<-all.data[ , !(names(all.data) %in% incomplete.tickers )]

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

main.output$series1.name<- stock.list$Security[match(main.output$series1,  stock.list$Ticker.symbol)]
main.output$series2.name<- stock.list$Security[match(main.output$series2,  stock.list$Ticker.symbol)]
main.output$series1.ind<- stock.list$GICSÿSector[match(main.output$series1,  stock.list$Ticker.symbol)]
main.output$series2.ind<- stock.list$GICSÿSector[match(main.output$series2,  stock.list$Ticker.symbol)]
main.output$series1.subind<- stock.list$GICS.Sub.Industry[match(main.output$series1,  stock.list$Ticker.symbol)]
main.output$series2.subind<- stock.list$GICS.Sub.Industry[match(main.output$series2,  stock.list$Ticker.symbol)]
#Reorder columns
main.output<-main.output[,c(1,2,30,31,3:29,32:35)]
# # data <- Quandl.datatable('WIKI/PRICES', paginate=TRUE)
# data1 <- Quandl.datatable('WIKI/PRICES', ticker='AMZN')

write.table(main.output, file = paste(data.dir, "data/Summary-US-", format(Sys.Date(), "%Y%m%d"),".csv", sep=""), append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)