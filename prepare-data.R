library(devtools)
library(httr)
library(RCurl)
library(egcm)
printf <- function(...)print(sprintf(...))

clean.data<-function(data1, ticker){
  split.factor <-integer(1)
  start.index<-0
  split.found=FALSE
  
  temp<-as.vector(data1$Close)
  printf("Starting for ticker %s",ticker)
  for(i in length(temp):1){
    p1<-temp[i]
    if(i>1) p2<-temp[i-1]
    else p2<-p1
    if(is.na(p2)) next;
    if((p1/p2>1.2 | p1/p2<0.8) & !split.found & (i>1)){
      printf("Split found for %s at : %f, %f, %f, %f ", ticker, i, p1,p2, p1/p2)
      if(p2>p1){
        split.factor <- as.double(1/as.integer(p2/p1))
      }
      else{
        split.factor <- as.integer(round(p1/p2, digits = 0))
        printf("split factor %f", split.factor )
      }
      if("Split" %in% colnames(data1)) {
        if(!is.na(data1$Split[i])){
          split.factor<- 1/data1$Split[i]
          printf("***SPLIT ENTRYfound for %s at %i, split value %f, split factor %f",ticker, i, data1$Split[i], split.factor)
        }
      }
      if(split.factor==9 | split.factor==11) split.factor<-10
      if(split.factor>0.08 & split.factor<0.12) split.factor<-0.1
      if(split.factor==1) next;
      start.index<-i-1
      split.found=TRUE
    }
    else if(split.found){
      printf("before %f",data1$Close[i])
      data1$Close[i]<-p1*split.factor
      printf("after %f",data1$Close[i])
      printf("ratio is %f and split factor is %f",as.integer(round(p2/p1,digits=0)), split.factor )
      if(as.double(p2/p1)/split.factor>0.8 & as.double(p2/p1)/split.factor<1.2 & i>1){
        split.found=FALSE
        split.factor <-integer(1)
        printf("Split close for %s at : %f, %f, %f, %f",ticker, i, p1,p2,p1/p2)
      }
    }
  }
  if(split.found){
    printf("*****Warning split found but not closed for %s", ticker)
  }
  
  printf("finished for %s", ticker)
  return(data1)
}

# prices1<-getYahooData("ONGC.NS", start = as.numeric(format(Sys.Date() - 365, "%Y%m%d")),
#                       end = as.numeric(format(Sys.Date(), "%Y%m%d")))
# price<-clean.data(prices1,"ONGC.NS" )

tickers_100<-c('ABB.NS','ACC.NS','ADANIPORTS.NS','AMBUJACEM.NS','ASHOKLEY.NS','ASIANPAINT.NS','AUROPHARMA.NS','AXISBANK.NS','BAJAJ-AUTO.NS','BAJFINANCE.NS','BAJAJFINSV.NS','BANKBARODA.NS','BEL.NS','BHEL.NS','BPCL.NS','BHARTIARTL.NS','INFRATEL.NS','BOSCHLTD.NS','BRITANNIA.NS','CADILAHC.NS','CIPLA.NS','COALINDIA.NS','COLPAL.NS','CONCOR.NS','CUMMINSIND.NS','DLF.NS','DABUR.NS','DIVISLAB.NS','DRREDDY.NS','EICHERMOT.NS','EMAMILTD.NS','GAIL.NS','GSKCONS.NS','GLAXO.NS','GLENMARK.NS','GODREJCP.NS','GRASIM.NS','HCLTECH.NS','HDFCBANK.NS','HAVELLS.NS','HEROMOTOCO.NS','HINDALCO.NS','HINDPETRO.NS','HINDUNILVR.NS','HINDZINC.NS','HDFC.NS','ITC.NS','ICICIBANK.NS','ICICIPRULI.NS','IDEA.NS','IBULHSGFIN.NS','IOC.NS','INDUSINDBK.NS','INFY.NS','INDIGO.NS','JSWSTEEL.NS','KOTAKBANK.NS','LICHSGFIN.NS','LT.NS','LUPIN.NS','M&M.NS','MARICO.NS','MARUTI.NS','MOTHERSUMI.NS','NHPC.NS','NMDC.NS','NTPC.NS','ONGC.NS','OIL.NS','OFSS.NS','PETRONET.NS','PIDILITIND.NS','PEL.NS','PFC.NS','POWERGRID.NS','PGHH.NS','PNB.NS','RELIANCE.NS','RECLTD.NS','SHREECEM.NS','SRTRANSFIN.NS','SIEMENS.NS','SBIN.NS','SAIL.NS','SUNPHARMA.NS','TCS.NS','TATAMTRDVR.NS','TATAMOTORS.NS','TATAPOWER.NS','TATASTEEL.NS','TECHM.NS','TITAN.NS','TORNTPHARM.NS','UPL.NS','ULTRACEMCO.NS','UBL.NS','MCDOWELL-N.NS','VEDL.NS','WIPRO.NS','YESBANK.NS','ZEEL.NS')
ignore_100<-c('ADANIPORTS.NS','ASIANPAINT.NS','AUROPHARMA.NS','BAJAJ-AUTO.NS','BAJAJFINSV.NS','BANKBARODA.NS','BHARTIARTL.NS','HEROMOTOCO.NS','ICICIPRULI.NS','M&M.NS','TORNTPHARM.NS','ULTRACEMCO.NS','MCDOWELL-N.NS')
tickers_200<-c('ABB.NS','ACC.NS','AIAENG.NS','ADANIENT.NS','ADANIPORTS.NS','ADANIPOWER.NS','ABFRL.NS','ABIRLANUVO.NS','AJANTPHARM.NS','APLLTD.NS','ALKEM.NS','AMARAJABAT.NS','AMBUJACEM.NS','APOLLOHOSP.NS','APOLLOTYRE.NS','ARVIND.NS','ASHOKLEY.NS','ASIANPAINT.NS','AUROPHARMA.NS','AXISBANK.NS','BAJAJ-AUTO.NS','BAJFINANCE.NS','BAJAJFINSV.NS','BANKBARODA.NS','BANKINDIA.NS','BATAINDIA.NS','BERGEPAINT.NS','BEL.NS','BHARATFIN.NS','BHARATFORG.NS','BHEL.NS','BPCL.NS','BHARTIARTL.NS','INFRATEL.NS','BIOCON.NS','BOSCHLTD.NS','BRITANNIA.NS','CESC.NS','CRISIL.NS','CADILAHC.NS','CANBK.NS','CASTROLIND.NS','CENTRALBK.NS','CENTURYTEX.NS','CHOLAFIN.NS','CIPLA.NS','COALINDIA.NS','COLPAL.NS','CONCOR.NS','COROMANDEL.NS','CROMPTON.NS','CUMMINSIND.NS','DLF.NS','DABUR.NS','DALMIABHA.NS','DHFL.NS','DISHTV.NS','DIVISLAB.NS','LALPATHLAB.NS','DRREDDY.NS','EDELWEISS.NS','EICHERMOT.NS','EMAMILTD.NS','ENDURANCE.NS','ENGINERSIN.NS','EXIDEIND.NS','FEDERALBNK.NS','FORTIS.NS','GAIL.NS','GMRINFRA.NS','GSKCONS.NS','GLAXO.NS','GLENMARK.NS','GODREJCP.NS','GODREJIND.NS','GRASIM.NS','GRUH.NS','GPPL.NS','GSPL.NS','HCLTECH.NS','HDFCBANK.NS','HAVELLS.NS','HEROMOTOCO.NS','HEXAWARE.NS','HINDALCO.NS','HINDPETRO.NS','HINDUNILVR.NS','HINDZINC.NS','HDFC.NS','ITC.NS','ICICIBANK.NS','ICICIPRULI.NS','IDBI.NS','IDFCBANK.NS','IDFC.NS','IRB.NS','IDEA.NS','IBULHSGFIN.NS','INDIANB.NS','INDHOTEL.NS','IOC.NS','IGL.NS','INDUSINDBK.NS','NAUKRI.NS','INFY.NS','INDIGO.NS','IPCALAB.NS','JSWENERGY.NS','JSWSTEEL.NS','JINDALSTEL.NS','JUBLFOOD.NS','JUBILANT.NS','KARURVYSYA.NS','KOTAKBANK.NS','L&TFH.NS','LTTS.NS','LICHSGFIN.NS','LT.NS','LUPIN.NS','MRF.NS','M&MFIN.NS','M&M.NS','MANAPPURAM.NS','MARICO.NS','MARUTI.NS','MINDTREE.NS','MOTHERSUMI.NS','MPHASIS.NS','MUTHOOTFIN.NS','NATCOPHARM.NS','NBCC.NS','NHPC.NS','NMDC.NS','NTPC.NS','NATIONALUM.NS','OBEROIRLTY.NS','ONGC.NS','OIL.NS','OFSS.NS','PCJEWELLER.NS','PIIND.NS','PAGEIND.NS','PETRONET.NS','PIDILITIND.NS','PEL.NS','PFC.NS','POWERGRID.NS','PRESTIGE.NS','PGHH.NS','PNB.NS','QUESS.NS','RBLBANK.NS','RAJESHEXPO.NS','RELCAPITAL.NS','RCOM.NS','RELIANCE.NS','RELINFRA.NS','RPOWER.NS','RECLTD.NS','SRF.NS','SHREECEM.NS','SRTRANSFIN.NS','SIEMENS.NS','SBIN.NS','SAIL.NS','STAR.NS','SPARC.NS','SUNPHARMA.NS','SUNTV.NS','SUZLON.NS','SYNDIBANK.NS','SYNGENE.NS','TV18BRDCST.NS','TVSMOTOR.NS','TATACHEM.NS','TATACOMM.NS','TCS.NS','TATAGLOBAL.NS','TATAMTRDVR.NS','TATAMOTORS.NS','TATAPOWER.NS','TATASTEEL.NS','TECHM.NS','RAMCOCEM.NS','THERMAX.NS','TITAN.NS','TORNTPHARM.NS','TORNTPOWER.NS','UPL.NS','ULTRACEMCO.NS','UNIONBANK.NS','UBL.NS','MCDOWELL-N.NS','VAKRANGEE.NS','VEDL.NS','VOLTAS.NS','WELSPUNIND.NS','WIPRO.NS','WOCKPHARMA.NS','YESBANK.NS','ZEEL.NS')
ignore_200<-c('GRUH.NS','BAJAJ-AUTO.NS', 'MCDOWELL-N.NS','CROMPTON.NS', 'ENDURANCE.NS', 'ICICIPRULI.NS', 'L&TFH.NS', 'LTTS.NS', 'M&MFIN.NS', 'M&M.NS', 'QUESS.NS', 'RBLBANK.NS')

ticker_new<-setdiff(tickers_200, ignore_200)
for(i in 1:length(ticker_new)){
  prices <- clean.data(getYahooData(ticker_new[i], start = as.numeric(format(Sys.Date() - 365, "%Y%m%d")),
                                    end = as.numeric(format(Sys.Date(), "%Y%m%d"))),ticker_new[i] )
  if(i==1) {
    all.data<-as.data.frame(prices$Close)
  }
  else {
    all.data$temp <- c(rep(NA, nrow(all.data)-length(prices$Close)), prices$Close)  
    # all.data$temp<-prices$Close
  }
  colnames(all.data)[i] <- ticker_new[i]
}

for(i in 1:length(ticker_new)){
  prices <- clean.data(getYahooData(ticker_new[i], start = as.numeric(format(Sys.Date() - 180, "%Y%m%d")),
                                    end = as.numeric(format(Sys.Date(), "%Y%m%d"))),ticker_new[i] )
  if(i==1) {
    all.data180<-as.data.frame(prices$Close)
  }
  else {
    all.data180$temp <- c(rep(NA, nrow(all.data180)-length(prices$Close)), prices$Close)
  }
  colnames(all.data180)[i] <- ticker_new[i]
}
#Cross verify if all splits have been handled well

for(j in 1: ncol(all.data)){
  printf("Checking for %s with %d rows....", colnames(all.data)[j], nrow(all.data[j]))
  for(i in 1:(nrow(all.data[j])-1)){
    # printf("i is %d j is %d",i,j)
    if(is.na(all.data[i,j]) | is.na(all.data[i+1,j])) next;
    if((as.numeric(all.data[i,j])/as.numeric(all.data[i+1,j]))<0.8 | (as.numeric(all.data[i,j])/as.numeric(all.data[i+1,j]))>1.2)
      printf("Ratio for %s at date %s is %f",colnames(all.data)[j], rownames(all.data)[i], (all.data[i,j]/all.data[i+1,j]))
  }
}

write.table(all.data, file = paste("D://Work/Stocks/all-data-", format(Sys.Date(), "%Y%m%d"),".csv"), append = FALSE, col.names = TRUE, sep = ",", row.names=TRUE)

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

output<-allpairs.egcm(all.data,
                      startdate = as.numeric(format(Sys.Date() - 365, "%Y%m%d")),
                      enddate = as.numeric(format(Sys.Date(), "%Y%m%d")), p.value=0.10, i1test="adf", urtest = "adf", include.const=FALSE, na.action=na.omit
)
write.table(output, file = paste("D://Work/Stocks/summary-", format(Sys.Date(), "%Y%m%d"),"-adf.csv"), append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
main.output$r.p.adf.365<-output$r.p
main.output$is.cointegrated.adf.365<-output$is.cointegrated

output<-allpairs.egcm(all.data180,
                      startdate = as.numeric(format(Sys.Date() - 180, "%Y%m%d")),
                      enddate = as.numeric(format(Sys.Date(), "%Y%m%d")), p.value=0.10, include.const=FALSE, na.action=na.omit
)

write.table(output, file = paste("D://Work/Stocks/summary-180-", format(Sys.Date(), "%Y%m%d"),".csv"), append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
main.output$r.p.pp.180<-output$r.p
main.output$is.cointegrated.pp.180<-output$is.cointegrated
main.output$beta180<-output$beta
main.output$r.last180<-0.0
main.output$residuals.sd180<-output$residuals.sd
for(i in 1:nrow(main.output)){
  main.output$r.last180[i]<-(all.data[nrow(all.data),as.character(main.output$series2[i])] - main.output$beta180[i]*all.data[nrow(all.data),as.character(main.output$series1[i])] - main.output$alpha[i])/main.output$residuals.sd180[i]
}

output<-allpairs.egcm(all.data180,
                      startdate = as.numeric(format(Sys.Date() - 180, "%Y%m%d")),
                      enddate = as.numeric(format(Sys.Date(), "%Y%m%d")), p.value=0.10, i1test="adf", urtest = "adf", include.const=FALSE, na.action=na.omit
)
write.table(output, file = paste("D://Work/Stocks/summary-180-", format(Sys.Date(), "%Y%m%d"),"-adf.csv"), append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
main.output$r.p.adf.180<-output$r.p
main.output$is.cointegrated.adf.180<-output$is.cointegrated

write.table(main.output, file = paste("D://Work/Stocks/summary-", format(Sys.Date(), "%Y%m%d"),".csv"), append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)