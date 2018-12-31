library(Quandl)
library(devtools)
library(httr)
library(RCurl)
library(quantmod)
library(tseries)
library(zoo)
library(egcm)

getSymbols("HDFC.NS", src = "yahoo")
getSymbols('NASDAQ%3AAPPL',src='google')


Quandl.api_key("rAHKcgv_ymH7_o9s5nWN")
data <- Quandl("TC1/HDFC")
data <- Quandl("NSE/BAJFINANCE")
url->"https://www.quandl.com/api/v3/datasets/NSE/NIFTY_50/metadata.xml?api_key=rAHKcgv_ymH7_o9s5nWN"
url<-"https://www.quandl.com/api/v3/databases/MCX/data?download_type=complete&api_key=rAHKcgv_ymH7_o9s5nWN"
url<-paste("https://query1.finance.yahoo.com/v7/finance/download/%5ENSEI?period1=1492506574&period2=",as.numeric(as.POSIXct(Sys.Date(), format="%Y-%m-%d")),"&interval=1d&events=history&crumb=UExkD.rrAYn",sep="")
url<-"https://query1.finance.yahoo.com/v7/finance/download/HDFCBANK.NS?period1=1492869828&period2=1495461828&interval=1d&events=history&crumb=UExkD.rrAYn"
url<-"https://uk.finance.yahoo.com/quote/AAPL/history"
curl <- getCurlHandle()

dat <- read.csv(url)

curlSetOpt(.opts = list(proxy = 'dia2.santanderuk.gs.corp:80'), curl = curl)

ans <- getURL(url, curl = curl)
ans
write(ans,"D://Work/Stocks/pp.html")

download.file(url, "D://Work/Stocks/sample.zip", method="curl")

install.packages("egcm")


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



library(egcm)
prices1 <- clean.data(getYahooData("ULTRACEMCO.NS", start = as.numeric(format(Sys.Date() - 200, "%Y%m%d")),
                        end = as.numeric(format(Sys.Date()-3, "%Y%m%d"))), "GODREJCP.NS")
prices2 <- clean.data(getYahooData("^NSEI", start = as.numeric(format(Sys.Date() - 200, "%Y%m%d")),
                        end = as.numeric(format(Sys.Date()-3, "%Y%m%d"))), "HINDUNILVR.NS")
eg<-egcm(prices1$Close, prices2$Close, include.const=FALSE)
plot(eg)

all.data<-read.csv("C:/Work/trading-strategies/Pair/all-data-20180814.csv", row.names=1)
lookback<-60
prices1<-as.data.frame(all.data$ASIANPAINT.NS)[(nrow(all.data)-lookback+1):nrow(all.data),1]
prices2<-as.data.frame(all.data$BERGEPAINT.NS)[(nrow(all.data)-lookback+1):nrow(all.data),1]
eg<-egcm(prices1, prices2, include.const=FALSE)
plot(eg)
summary(eg)
moves<-diff(eg$residuals, trim=TRUE)/eg$residuals.sd
residuals<-eg$residuals[1:length(moves)]/eg$residuals.sd
score<-sum(moves*residuals)*100/length(moves)
squared.score<-sum(sign(residuals)*(moves*(residuals^2))) * 100 / length(moves)
printf("score= %f",score)
printf("squared score= %f",squared.score)


# write.table(data.frame(Quandl("NSE/HDFCBANK")[5:(lookback+4), c("Date")],rev(row.names(all.data)[(nrow(all.data)-lookback+1):nrow(all.data)]),rev(prices1), rev(prices2), prices11, prices21), file="D:/Work/Stocks/temp-data.csv", sep=",", row.names = FALSE)

residuals<-prices2-800/250*prices1
# residuals<-eg$residuals
adf.test(residuals)
pp.test(residuals)
kpss.test(residuals)
plot(residuals,type="n")
lines(residuals, type="o")
r<-(residuals-mean(residuals))/sd(residuals)
moving.window<-20
r<-(residuals[(moving.window):length(residuals)]-rollapply(residuals, width = moving.window, FUN = mean, align="left"))/rollapply(residuals, width = moving.window, FUN = sd, align="left")
plot(rollapply(residuals, width = moving.window, FUN = sd, align="left"), type="n")
lines(rollapply(residuals, width = moving.window, FUN = sd, align="left"), type="o")
plot(r,type="n")
lines(r, type="o")

res<-(prices2$Close - eg$beta*prices1$Close)/eg$residuals.sd
res

tickers<-c('3MINDIA.NS','8KMILES.NS','ABB.NS','ACC.NS','AIAENG.NS','APLAPOLLO.NS','AARTIIND.NS','ABAN.NS','ADANIENT.NS','ADANIPORTS.NS','ADANIPOWER.NS','ADANITRANS.NS','ABFRL.NS','ABIRLANUVO.NS','ADVENZYMES.NS','AEGISCHEM.NS','AHLUCONT.NS','AJANTPHARM.NS','AKZOINDIA.NS','APLLTD.NS','ALKEM.NS','ALBK.NS','ALLCARGO.NS','AMARAJABAT.NS','AMBUJACEM.NS','AMTEKAUTO.NS','ANANTRAJ.NS','ANDHRABANK.NS','APARINDS.NS','APOLLOHOSP.NS','APOLLOTYRE.NS','ARVIND.NS','ASAHIINDIA.NS','ASHOKLEY.NS','ASHOKA.NS','ASIANPAINT.NS','ASTRAZEN.NS','ASTRAL.NS','ATUL.NS','AUROPHARMA.NS','AVANTIFEED.NS','AXISBANK.NS','BASF.NS','BEML.NS','BFUTILITIE.NS','BGRENERGY.NS','BAJAJ-AUTO.NS','BAJAJCORP.NS','BAJAJELEC.NS','BAJFINANCE.NS','BAJAJFINSV.NS','BAJAJHIND.NS','BAJAJHLDNG.NS','BALKRISIND.NS','BALLARPUR.NS','BALMLAWRIE.NS','BALRAMCHIN.NS','BANKBARODA.NS','BANKINDIA.NS','BATAINDIA.NS','BERGEPAINT.NS','BEL.NS','BHARATFIN.NS','BHARATFORG.NS','BHEL.NS','BPCL.NS','BHARTIARTL.NS','INFRATEL.NS','BHUSANSTL.NS','BIOCON.NS','BIRLACORPN.NS','BLISSGVS.NS','BLUEDART.NS','BLUESTARCO.NS','BBTC.NS','BOMDYEING.NS','BOSCHLTD.NS','BRITANNIA.NS','CARERATING.NS','CCL.NS','CESC.NS','CGPOWER.NS','CRISIL.NS','CADILAHC.NS','CANFINHOME.NS','CANBK.NS','CAPF.NS','CAPLIPOINT.NS','CARBORUNIV.NS','CASTROLIND.NS','CEATLTD.NS','CENTRALBK.NS','CENTURYPLY.NS','CENTURYTEX.NS','CERA.NS','CHAMBLFERT.NS','CHENNPETRO.NS','CHOLAFIN.NS','CIPLA.NS','CUB.NS','COALINDIA.NS','COFFEEDAY.NS','COLPAL.NS','CONCOR.NS','COROMANDEL.NS','CORPBANK.NS','CROMPTON.NS','CUMMINSIND.NS','CYIENT.NS','DBREALTY.NS','DBCORP.NS','DCBBANK.NS','DCMSHRIRAM.NS','DLF.NS','DABUR.NS','DALMIABHA.NS','DEEPAKFERT.NS','DELTACORP.NS','DEN.NS','DENABANK.NS','DHFL.NS','DHANUKA.NS','DBL.NS','DISHTV.NS','DIVISLAB.NS','LALPATHLAB.NS','DRREDDY.NS','DREDGECORP.NS','EIDPARRY.NS','EIHOTEL.NS','EDELWEISS.NS','EICHERMOT.NS','EMAMILTD.NS','ENDURANCE.NS','ENGINERSIN.NS','EQUITAS.NS','EROSMEDIA.NS','ESCORTS.NS','ESSELPACK.NS','EVEREADY.NS','EXIDEIND.NS','FDC.NS','FAGBEARING.NS','FEDERALBNK.NS','FMGOETZE.NS','FINCABLES.NS','FINPIPE.NS','FSL.NS','FORTIS.NS','FCONSUMER.NS','FRETAIL.NS','GAIL.NS','GEPIL.NS','GHCL.NS','GMRINFRA.NS','GVKPIL.NS','GDL.NS','GATI.NS','GAYAPROJ.NS','GILLETTE.NS','GSKCONS.NS','GLAXO.NS','GLENMARK.NS','GODFRYPHLP.NS','GODREJCP.NS','GODREJIND.NS','GODREJPROP.NS','GRANULES.NS','GRASIM.NS','GESHIP.NS','GREAVESCOT.NS','GREENPLY.NS','GRINDWELL.NS','GRUH.NS','GUJALKALI.NS','GUJFLUORO.NS','GUJGASLTD.NS','GMDCLTD.NS','GNFC.NS','GPPL.NS','GSFC.NS','GSPL.NS','GULFOILLUB.NS','HCL-INSYS.NS','HCLTECH.NS','HDFCBANK.NS','HSIL.NS','HTMEDIA.NS','HATHWAY.NS','HAVELLS.NS','HEIDELBERG.NS','HEROMOTOCO.NS','HEXAWARE.NS','HFCL.NS','HIMATSEIDE.NS','HINDALCO.NS','HCC.NS','HINDCOPPER.NS','HMVL.NS','HINDPETRO.NS','HINDUNILVR.NS','HINDZINC.NS','HONAUT.NS','HDFC.NS','HDIL.NS','ITC.NS','ICICIBANK.NS','ICICIPRULI.NS','ICRA.NS','IDBI.NS','IDFCBANK.NS','IDFC.NS','IFCI.NS','IIFL.NS','IRB.NS','ITDCEM.NS','IDEA.NS','IGARASHI.NS','INDIACEM.NS','IBULHSGFIN.NS','IBREALEST.NS','INDIANB.NS','INDHOTEL.NS','IOC.NS','IOB.NS','ICIL.NS','INDOCO.NS','IGL.NS','INDUSINDBK.NS','INFIBEAM.NS','NAUKRI.NS','INFY.NS','INOXLEISUR.NS','INOXWIND.NS','INTELLECT.NS','INDIGO.NS','IPCALAB.NS','JBCHEPHARM.NS','JKCEMENT.NS','JKIL.NS','JBFIND.NS','JKLAKSHMI.NS','JKTYRE.NS','JMFINANCIL.NS','JMTAUTOLTD.NS','JSWENERGY.NS','JSWSTEEL.NS','JAGRAN.NS','JAICORPLTD.NS','JISLJALEQS.NS','JPASSOCIAT.NS','JETAIRWAYS.NS','JINDALPOLY.NS','JINDALSTEL.NS','JCHAC.NS','JUBLFOOD.NS','JUBILANT.NS','JUSTDIAL.NS','JYOTHYLAB.NS')
tickers2<-c('KPRMILL.NS','KNRCON.NS','KPIT.NS','KRBL.NS','KAJARIACER.NS','KALPATPOWR.NS','KANSAINER.NS','KTKBANK.NS','KARURVYSYA.NS','KSCL.NS','KEC.NS','KESORAMIND.NS','KITEX.NS','KOLTEPATIL.NS','KOTAKBANK.NS','KWALITY.NS','LTTS.NS','LICHSGFIN.NS','LAXMIMACH.NS','LAKSHVILAS.NS','LTI.NS','LT.NS','LUPIN.NS','MMTC.NS','MOIL.NS','MRF.NS','MAGMA.NS','MGL.NS','MTNL.NS','MAHINDCIE.NS','MHRIL.NS','MANAPPURAM.NS','MRPL.NS','MANPASAND.NS','MARICO.NS','MARKSANS.NS','MARUTI.NS','MAXINDIA.NS','MCLEODRUSS.NS','MERCK.NS','MINDTREE.NS','MINDACORP.NS','MINDAIND.NS','MONSANTO.NS','MOTHERSUMI.NS','MOTILALOFS.NS','MPHASIS.NS','MUTHOOTFIN.NS','NATCOPHARM.NS','NBCC.NS','NCC.NS','NHPC.NS','NIITTECH.NS','NLCINDIA.NS','NMDC.NS','NTPC.NS','NH.NS','NATIONALUM.NS','NBVENTURES.NS','NAVINFLUOR.NS','NAVKARCORP.NS','NAVNETEDUL.NS','NETWORK18.NS','NILKAMAL.NS','NITINFIRE.NS','OBEROIRLTY.NS','ONGC.NS','OIL.NS','OMAXE.NS','OFSS.NS','ORIENTCEM.NS','ORIENTBANK.NS','ORISSAMINE.NS','PCJEWELLER.NS','PIIND.NS','PNCINFRA.NS','PFS.NS','PTC.NS','PVR.NS','PAGEIND.NS','PARAGMILK.NS','PERSISTENT.NS','PETRONET.NS','PFIZER.NS','PHOENIXLTD.NS','PIDILITIND.NS','PEL.NS','POLARIS.NS','PFC.NS','POWERGRID.NS','PRAJIND.NS','PRESTIGE.NS','PRISMCEM.NS','PGHH.NS','PUNJLLOYD.NS','PNB.NS','QUESS.NS','RBLBANK.NS','RADICO.NS','RAIN.NS','RAJESHEXPO.NS','RALLIS.NS','RAMCOSYS.NS','RKFORGE.NS','RCF.NS','RATNAMANI.NS','RTNPOWER.NS','RAYMOND.NS','REDINGTON.NS','RELAXO.NS','RELCAPITAL.NS','RCOM.NS','RDEL.NS','RIIL.NS','RELIANCE.NS','RELINFRA.NS','RPOWER.NS','RELIGARE.NS','REPCOHOME.NS','ROLTA.NS','RUCHISOYA.NS','RECLTD.NS','SHK.NS','SJVN.NS','SKFINDIA.NS','SMLISUZU.NS','SREINFRA.NS','SRF.NS','SADBHAV.NS','SANOFI.NS','SCHNEIDER.NS','SHARDACROP.NS','SHILPAMED.NS','SHILPI.NS','SCI.NS','SHREECEM.NS','RENUKA.NS','SHRIRAMCIT.NS','SRTRANSFIN.NS','SIEMENS.NS','SNOWMAN.NS','SOBHA.NS','SOLARINDS.NS','SOMANYCERA.NS','SONATSOFTW.NS','SOUTHBANK.NS','SBIN.NS','STCINDIA.NS','SAIL.NS','STRTECH.NS','STAR.NS','SUDARSCHEM.NS','SPARC.NS','SUNPHARMA.NS','SUNTV.NS','SUNDARMFIN.NS','SUNDRMFAST.NS','SUNTECK.NS','SUPREMEIND.NS','SUVEN.NS','SUZLON.NS','SWANENERGY.NS','SYMPHONY.NS','SYNDIBANK.NS','SYNGENE.NS','TTKPRESTIG.NS','TVTODAY.NS','TV18BRDCST.NS','TVSMOTOR.NS','TVSSRICHAK.NS','TAKE.NS','TNPL.NS','TATACHEM.NS','TATACOFFEE.NS','TATACOMM.NS','TCS.NS','TATAELXSI.NS','TATAGLOBAL.NS','TATAINVEST.NS','TATAMTRDVR.NS','TATAMOTORS.NS','TATAPOWER.NS','TATASPONGE.NS','TATASTEEL.NS','TECHM.NS','TECHNO.NS','TEXRAIL.NS','RAMCOCEM.NS','THERMAX.NS','THOMASCOOK.NS','THYROCARE.NS','TIDEWATER.NS','TIMKEN.NS','TITAN.NS','TORNTPHARM.NS','TORNTPOWER.NS','TRENT.NS','TRIDENT.NS','TRITURBINE.NS','TUBEINVEST.NS','UCOBANK.NS','UFLEX.NS','UPL.NS','UJJIVAN.NS','ULTRACEMCO.NS','UNICHEMLAB.NS','UNIONBANK.NS','UNITECH.NS','UBL.NS','MCDOWELL-N.NS','VGUARD.NS','VIPIND.NS','VRLLOG.NS','VSTIND.NS','WABAG.NS','VAKRANGEE.NS','VTL.NS','VEDL.NS','VIDEOIND.NS','VIJAYABANK.NS','VINATIORGA.NS','VOLTAS.NS','WABCOINDIA.NS','WELCORP.NS','WELSPUNIND.NS','WHIRLPOOL.NS','WIPRO.NS','WOCKPHARMA.NS','WONDERLA.NS','YESBANK.NS','ZEEL.NS','ZEELEARN.NS','ZENSARTECH.NS','ZYDUSWELL.NS','ECLERX.NS')
tickers<- c(tickers,tickers2)
ignore<-c('ADVENZYMES.NS','BAJAJ-AUTO.NS','BEL.NS','CGPOWER.NS','CROMPTON.NS','DBL.NS','ENDURANCE.NS','EQUITAS.NS','FCONSUMER.NS','FRETAIL.NS','HCL-INSYS.NS','ICICIPRULI.NS','LTTS.NS','LTI.NS','MGL.NS','MAXINDIA.NS','PARAGMILK.NS','QUESS.NS','RBLBANK.NS','THYROCARE.NS','UJJIVAN.NS','MCDOWELL-N.NS')
ticker_new<-setdiff(tickers, ignore)

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
    
    if((p1/p2>1.2 | p1/p2<0.8) & !split.found & (i>1)){
      printf("Split found for %s at : %f, %f, %f, %f ", ticker, i, p1,p2, p1/p2)
      if(p2>p1){
        split.factor <- as.double(1/as.integer(p2/p1))
      }
      else{
        split.factor <- as.integer(round(p1/p2, digits = 0))
        printf("split factor %f", split.factor )
      }
      if(!is.na(data1$Split[i])){
        split.factor<- 1/data1$Split[i]
        printf("***SPLIT ENTRYfound for %s at %i, split value %f, split factor %f",ticker, i, data1$Split[i], split.factor)
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

for(i in 1:length(ticker_new)){
  prices <- clean.data(getYahooData(ticker_new[i], start = as.numeric(format(Sys.Date() - 365, "%Y%m%d")),
                          end = as.numeric(format(Sys.Date(), "%Y%m%d"))))
  if(i==1) {
    all.data<-as.data.frame(prices$Close)
  }
  else {
    all.data$temp<-prices$Close
  }
  colnames(all.data)[i] <- ticker_new[i]
}



tickers_100<-c('ABB.NS','ACC.NS','ADANIPORTS.NS','AMBUJACEM.NS','ASHOKLEY.NS','ASIANPAINT.NS','AUROPHARMA.NS','AXISBANK.NS','BAJAJ-AUTO.NS','BAJFINANCE.NS','BAJAJFINSV.NS','BANKBARODA.NS','BEL.NS','BHEL.NS','BPCL.NS','BHARTIARTL.NS','INFRATEL.NS','BOSCHLTD.NS','BRITANNIA.NS','CADILAHC.NS','CIPLA.NS','COALINDIA.NS','COLPAL.NS','CONCOR.NS','CUMMINSIND.NS','DLF.NS','DABUR.NS','DIVISLAB.NS','DRREDDY.NS','EICHERMOT.NS','EMAMILTD.NS','GAIL.NS','GSKCONS.NS','GLAXO.NS','GLENMARK.NS','GODREJCP.NS','GRASIM.NS','HCLTECH.NS','HDFCBANK.NS','HAVELLS.NS','HEROMOTOCO.NS','HINDALCO.NS','HINDPETRO.NS','HINDUNILVR.NS','HINDZINC.NS','HDFC.NS','ITC.NS','ICICIBANK.NS','ICICIPRULI.NS','IDEA.NS','IBULHSGFIN.NS','IOC.NS','INDUSINDBK.NS','INFY.NS','INDIGO.NS','JSWSTEEL.NS','KOTAKBANK.NS','LICHSGFIN.NS','LT.NS','LUPIN.NS','M&M.NS','MARICO.NS','MARUTI.NS','MOTHERSUMI.NS','NHPC.NS','NMDC.NS','NTPC.NS','ONGC.NS','OIL.NS','OFSS.NS','PETRONET.NS','PIDILITIND.NS','PEL.NS','PFC.NS','POWERGRID.NS','PGHH.NS','PNB.NS','RELIANCE.NS','RECLTD.NS','SHREECEM.NS','SRTRANSFIN.NS','SIEMENS.NS','SBIN.NS','SAIL.NS','SUNPHARMA.NS','TCS.NS','TATAMTRDVR.NS','TATAMOTORS.NS','TATAPOWER.NS','TATASTEEL.NS','TECHM.NS','TITAN.NS','TORNTPHARM.NS','UPL.NS','ULTRACEMCO.NS','UBL.NS','MCDOWELL-N.NS','VEDL.NS','WIPRO.NS','YESBANK.NS','ZEEL.NS')
ignore_100<-c('ADANIPORTS.NS','ASIANPAINT.NS','AUROPHARMA.NS','BAJAJ-AUTO.NS','BAJAJFINSV.NS','BANKBARODA.NS','BHARTIARTL.NS','HEROMOTOCO.NS','ICICIPRULI.NS','M&M.NS','TORNTPHARM.NS','ULTRACEMCO.NS','MCDOWELL-N.NS')
ticker_new<-setdiff(tickers_100, ignore_100)
tickers_indices<-c('NIFTY')
output<-allpairs.egcm(ticker_new,
                      startdate = as.numeric(format(Sys.Date() - 180, "%Y%m%d")),
                      enddate = as.numeric(format(Sys.Date(), "%Y%m%d")), p.value=0.10, include.const=FALSE
)
s <- summary(output)
write.table(output, file = "D://Work/Stocks/summary-20170504.csv", append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)


output<-allpairs.egcm(ticker_new,
                      startdate = as.numeric(format(Sys.Date() - 180, "%Y%m%d")),
                      enddate = as.numeric(format(Sys.Date(), "%Y%m%d")), p.value=0.10, i1test="adf", urtest = "adf", include.const=FALSE
)
write.table(output, file = "D://Work/Stocks/summary-20170504-adf.csv", append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)

