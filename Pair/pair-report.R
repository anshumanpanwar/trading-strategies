library(egcm)
ticker1<-"IBULHSGFIN.NS"
ticker2<-"MUTHOOTFIN.NS"
file.dir<-"D:/Work/Stocks/"
write.dir<-"D:/Work/Stocks/pairs/"
printf <- function(...)print(sprintf(...))
file.list=list.files(path="D:/Work/Stocks/", pattern="all-data-20*")
lookback<-120
summary<-data.frame(stringsAsFactors = FALSE)
for(data.file in file.list){
  tokens<-strsplit(data.file,split="[- .]+")
  date<-as.Date(tokens[[1]][3], format="%Y%m%d")
  printf("Executing for %s", date)
  all.data<-read.csv(paste(file.dir, data.file, sep=""), row.names=1)
  tryCatch({
    lookback<-120
    prices1<-as.data.frame(all.data[,ticker1])[(nrow(all.data)-lookback+1):nrow(all.data),1]
    prices2<-as.data.frame(all.data[,ticker2])[(nrow(all.data)-lookback+1):nrow(all.data),1]
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
    temp.list<-list(format(date, "%Y-%m-%d"), eg$beta, eg$residuals.sd, log(2)/(1-eg$rho), is.cointegrated(eg), adf, pp, pgff, ersd, jot, spr)
    
    lookback<-240
    prices1<-as.data.frame(all.data[,ticker1])[(nrow(all.data)-lookback+1):nrow(all.data),1]
    prices2<-as.data.frame(all.data[,ticker2])[(nrow(all.data)-lookback+1):nrow(all.data),1]
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
    
    temp.list<-c(temp.list, eg$beta, eg$residuals.sd, log(2)/(1-eg$rho), is.cointegrated(eg), adf, pp, pgff, ersd, jot, spr)
    summary<-rbind.data.frame(summary, temp.list,stringsAsFactors = FALSE)
  }, error=function(e){printf("Ticker data not found for date %s", date)})
}
colnames(summary)<-c("Date", "Beta 120", "residual.sd 120", "half life 120", "is cointegrated 120", "adf 120", "pp 120", "pgff 120", "ersd 120", "jot 120", "spr 120", "Beta 240", "residual.sd 240", "half life 240", "is cointegrated 240", "adf 240", "pp 240", "pgff 240", "ersd 240", "jot 240", "spr 240")
output.file<-paste(write.dir, "pair-", ticker1, "-", ticker2, ".csv", sep="")
write.table(summary, output.file, append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
write(paste("\n\n Mean", round(mean(summary$`Beta 120`),3), round(mean(summary$`residual.sd 120`),3),  round(mean(summary$`half life 120`),3), round(mean(summary$`is cointegrated 120`),3), round(mean(summary$`adf 120`),3), round(mean(summary$`pp 120`),3), round(mean(summary$`pgff 120`),3), round(mean(summary$`ersd 120`),3), round(mean(summary$`jot 120`),3), round(mean(summary$`spr 120`),3),
            round(mean(summary$`Beta 240`),3), round(mean(summary$`residual.sd 240`),3), round(mean(summary$`half life 240`),3), round(mean(summary$`is cointegrated 240`),3), round(mean(summary$`adf 240`),3), round(mean(summary$`pp 240`),3), round(mean(summary$`pgff 240`),3), round(mean(summary$`ersd 240`),3), round(mean(summary$`jot 240`),3), round(mean(summary$`spr 240`),3), sep=","), file=output.file, append = TRUE)
write(paste("Std Dev", round(sd(summary$`Beta 120`),3), round(sd(summary$`residual.sd 120`),3), round(sd(summary$`half life 120`),3), round(sd(summary$`is cointegrated 120`),3), round(sd(summary$`adf 120`),3), round(sd(summary$`pp 120`),3), round(sd(summary$`pgff 120`),3), round(sd(summary$`ersd 120`),3), round(sd(summary$`jot 120`),3), round(sd(summary$`spr 120`),3),
            round(sd(summary$`Beta 240`),3), round(sd(summary$`residual.sd 240`),3), round(sd(summary$`half life 240`),3), round(sd(summary$`is cointegrated 240`),3), round(sd(summary$`adf 240`),3), round(sd(summary$`pp 240`),3), round(sd(summary$`pgff 240`),3), round(sd(summary$`ersd 240`),3), round(sd(summary$`jot 240`),3), round(sd(summary$`spr 240`),3), sep=","), file=output.file, append = TRUE)