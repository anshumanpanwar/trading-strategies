library(egcm)
prices1 <- clean.data(getYahooData("KOTAKNIFTY.NS", start = as.numeric(format(Sys.Date() - 365, "%Y%m%d")),
                                   end = as.numeric(format(Sys.Date()-3, "%Y%m%d"))), "GODREJCP.NS")
prices2 <- clean.data(getYahooData("^NSEI", start = as.numeric(format(Sys.Date() - 365, "%Y%m%d")),
                                   end = as.numeric(format(Sys.Date()-3, "%Y%m%d"))), "HINDUNILVR.NS")
prices<-de <- merge(prices1, prices2, by=0, all=FALSE)
eg<-egcm(prices$Close, prices$Close.1, include.const=FALSE)
plot(eg)

summary(eg)