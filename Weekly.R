all.data<-read.csv("D:/Work/Stocks/Weekly/all-data-Weekly-20180111.csv", row.names=1)
lookback<-240
prices1<-as.data.frame(all.data$GRASIM.NS)[(nrow(all.data)-lookback+1):nrow(all.data),1]
prices2<-as.data.frame(all.data$SBIN.NS)[(nrow(all.data)-lookback+1):nrow(all.data),1]
eg<-egcm(prices1, prices2, include.const=FALSE)
plot(eg)
summary(eg)