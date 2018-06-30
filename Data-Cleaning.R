library(egcm)
tickers_100<-c('ABB.NS','ACC.NS','ADANIPORTS.NS','AMBUJACEM.NS','ASHOKLEY.NS','ASIANPAINT.NS','AUROPHARMA.NS','AXISBANK.NS','BAJAJ-AUTO.NS','BAJFINANCE.NS','BAJAJFINSV.NS','BANKBARODA.NS','BEL.NS','BHEL.NS','BPCL.NS','BHARTIARTL.NS','INFRATEL.NS','BOSCHLTD.NS','BRITANNIA.NS','CADILAHC.NS','CIPLA.NS','COALINDIA.NS','COLPAL.NS','CONCOR.NS','CUMMINSIND.NS','DLF.NS','DABUR.NS','DIVISLAB.NS','DRREDDY.NS','EICHERMOT.NS','EMAMILTD.NS','GAIL.NS','GSKCONS.NS','GLAXO.NS','GLENMARK.NS','GODREJCP.NS','GRASIM.NS','HCLTECH.NS','HDFCBANK.NS','HAVELLS.NS','HEROMOTOCO.NS','HINDALCO.NS','HINDPETRO.NS','HINDUNILVR.NS','HINDZINC.NS','HDFC.NS','ITC.NS','ICICIBANK.NS','ICICIPRULI.NS','IDEA.NS','IBULHSGFIN.NS','IOC.NS','INDUSINDBK.NS','INFY.NS','INDIGO.NS','JSWSTEEL.NS','KOTAKBANK.NS','LICHSGFIN.NS','LT.NS','LUPIN.NS','M&M.NS','MARICO.NS','MARUTI.NS','MOTHERSUMI.NS','NHPC.NS','NMDC.NS','NTPC.NS','ONGC.NS','OIL.NS','OFSS.NS','PETRONET.NS','PIDILITIND.NS','PEL.NS','PFC.NS','POWERGRID.NS','PGHH.NS','PNB.NS','RELIANCE.NS','RECLTD.NS','SHREECEM.NS','SRTRANSFIN.NS','SIEMENS.NS','SBIN.NS','SAIL.NS','SUNPHARMA.NS','TCS.NS','TATAMTRDVR.NS','TATAMOTORS.NS','TATAPOWER.NS','TATASTEEL.NS','TECHM.NS','TITAN.NS','TORNTPHARM.NS','UPL.NS','ULTRACEMCO.NS','UBL.NS','MCDOWELL-N.NS','VEDL.NS','WIPRO.NS','YESBANK.NS','ZEEL.NS')
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


back.test<-function(stock1, stock2,training_start, training_end){
  printf("********TESTINF FOR %s and %s ********",stock1, stock2)
  prices1 <- clean.data(getYahooData(stock1, start = as.numeric(format(Sys.Date() - training_start, "%Y%m%d")),
                                     end = as.numeric(format(Sys.Date()-training_end, "%Y%m%d"))), stock1)
  prices2 <- clean.data(getYahooData(stock2, start = as.numeric(format(Sys.Date() - training_start, "%Y%m%d")),
                                     end = as.numeric(format(Sys.Date()-training_end, "%Y%m%d"))), stock2)
  eg<-egcm(prices1$Close, prices2$Close, include.const=FALSE)
  print(summary(eg))
  
  plot(egcm(clean.data(getYahooData(stock1, start = as.numeric(format(Sys.Date() - training_start, "%Y%m%d")),
                                    end = as.numeric(format(Sys.Date(), "%Y%m%d"))) ,stock1)$Close,
       clean.data(getYahooData(stock2, start = as.numeric(format(Sys.Date() - training_start, "%Y%m%d")),
                               end = as.numeric(format(Sys.Date(), "%Y%m%d"))) ,stock2)$Close,
       include.const=FALSE), main=paste(stock1, stock2, sep=" , "))
  # print("res1")
  # print(eg$residuals)
  # res<-as.data.frame(prices2$Close)-(as.data.frame(prices1$Close)*eg$beta)-eg$alpha
  # print("res2")
  # res
  prices1 <- clean.data(getYahooData(stock1, start = as.numeric(format(Sys.Date() - training_end, "%Y%m%d")),
                                     end = as.numeric(format(Sys.Date(), "%Y%m%d"))) ,stock1)
  prices2 <- clean.data(getYahooData(stock2, start = as.numeric(format(Sys.Date() - training_end, "%Y%m%d")),
                                     end = as.numeric(format(Sys.Date(), "%Y%m%d"))) ,stock2)
  res<-(as.data.frame(prices2$Close)-(as.data.frame(prices1$Close)*eg$beta)-eg$alpha)/eg$residuals.sd
  printf("Residuals SD from Model is %f",eg$residuals.sd)
  printf("Forward Residuals SD is %f",sd((as.data.frame(prices2$Close)-(as.data.frame(prices1$Close)*eg$beta)-eg$alpha)$Close))
  printf("Beta is %f",eg$beta)
  printf("Alpha is %f",eg$alpha)
  res$abs<-(as.data.frame(prices2$Close)-(as.data.frame(prices1$Close)*eg$beta)-eg$alpha)
  print(res)
 
  ret<-as.data.frame(matrix(vector(mode = 'numeric',length = length(res$Close)-1), nrow = length(res$Close)-1, ncol = 1))
  colnames(ret)<-c("returns")

  stock1.position<-0
  stock2.position<-0
  profit<-0
  total.stock1<-0
  total.stock2<-0
  entry.threshold<-2.5
  exit.threshold<-4
  zero.crossings<-0
  loss.crossings<-0
  threshold.crossings<-0
  hedge.ratio<-1/eg$beta
  exposure.stock1<-0
  exposure.stock2<-0
  for(i in 1:length(res$Close)){
    print(res[i,1])
    if(i>1){
      if(stock1.position!=0 | stock2.position!=0){
        ret$returns[i-1]<-((stock1.position*as.double(prices1$Close[i]) - stock1.position*as.double(prices1$Close[i-1]))/abs(stock1.position*as.double(prices1$Close[i-1])))
                          +((stock2.position*as.double(prices2$Close[i]) - stock2.position*as.double(prices2$Close[i-1]))/abs(stock2.position*as.double(prices2$Close[i-1])))

      }
      else{
        ret$returns[i-1]<-0
      }
    }
    if(i>1 & ((res[i-1,1]>0 && res[i,1]<=0) | (res[i-1,1]<0 && res[i,1]>=0))){
      profit<-profit+(stock1.position*as.double(prices1$Close[i])-total.stock1)+
        (stock2.position*as.double(prices2$Close[i])-total.stock2)
      printf("i is %f, res is %f so closing positions. stock 1 position %f, stock1 price %f, total.stock1 %f, 
             stock2 position %f, stock2 price %f, total.stock2 %f", i, res[i,1],stock1.position, prices1$Close[i], total.stock1, stock2.position, prices2$Close[i],total.stock2)
      printf("profit1 %f, profit2 %f, total %f",(stock1.position*as.double(prices1$Close[i])-total.stock1),(stock2.position*as.double(prices2$Close[i])-total.stock2),profit)
      stock1.position<-0
      stock2.position<-0
      total.stock1<-0
      total.stock2<-0
      # hedge.ratio<-eg$beta*as.double(prices1$Close[i])/as.double(prices2$Close[i])
      zero.crossings<-zero.crossings+1
    }
    else if(res[i,1]>entry.threshold & res[i,1]<exit.threshold){
      stock1.position<-stock1.position+1
      stock2.position<-stock2.position-hedge.ratio
      total.stock1<-total.stock1+as.double(prices1$Close[i])
      total.stock2<-total.stock2-(hedge.ratio*as.double(prices2$Close[i]))
      threshold.crossings<-threshold.crossings+1
      if(abs(exposure.stock1)<abs(total.stock1)) exposure.stock1<- total.stock1
      if(abs(exposure.stock2)<abs(total.stock2)) exposure.stock2<- total.stock2
      printf("i is %d, res is %f so increasing stock1 by 1 and decreasing stock2 by %f, total stock1 %f, total stock2 %f", i, res$Close[i],hedge.ratio, total.stock1, total.stock2)
      # printf("i is %f ",i)
      # printf("res is %f",res$Close[i])
      # printf("hedge ratio %f" ,hedge.ratio)
      # printf("total.stock2", total.stock2)
    }
    else if(res[i,1]< (-entry.threshold) & res[i,1]> (-exit.threshold)){
      stock1.position<-stock1.position-1
      stock2.position<-stock2.position+hedge.ratio
      total.stock1<-total.stock1-as.double(prices1$Close[i])
      total.stock2<-total.stock2+(hedge.ratio*as.double(prices2$Close[i]))
      threshold.crossings<-threshold.crossings+1
      if(abs(exposure.stock1)<abs(total.stock1)) exposure.stock1<- total.stock1
      if(abs(exposure.stock2)<abs(total.stock2)) exposure.stock2<- total.stock2
      printf("i is %f, res is %f so decreasing stock 1 by 1 and increasing stock 2 by %f, total stock1 %f, total stock2 %f", i, as.double(res$Close[i]),hedge.ratio, total.stock1, total.stock2)
    }
    else if(res[i,1]>exit.threshold | res[i,1]< (-exit.threshold) ){
      profit<-profit+(stock1.position*as.double(prices1$Close[i])-total.stock1)+
        (stock2.position*as.double(prices2$Close[i])-total.stock2)
      printf("i is %f, res is %f so closing positions **in LOSS. stock 1 position %f, stock1 price %f, total.stock1 %f, 
        stock2 position %f, stock2 price %f, total.stock2 %f", i, res[i,1],stock1.position, prices1$Close[i], total.stock1, stock2.position, prices2$Close[i],total.stock2)
      printf("profit1 %f, profit2 %f, total %f",(stock1.position*prices1$Close[i]-total.stock1),(stock2.position*prices2$Close[i]-total.stock2),profit)
      stock1.position<-0
      stock2.position<-0
      total.stock1<-0
      total.stock2<-0
      # hedge.ratio<-eg$beta*as.double(prices1$Close[i])/as.double(prices2$Close[i])
      loss.crossings<-loss.crossings+1
    }
  }
  
  printf("end summary: stock1 position: %f, stock 2 position %f, total invested stock1 %f, total invested stock 2 %f", stock1.position, stock2.position, total.stock1, total.stock2)
  unrealized.profit<-(stock1.position*as.double(prices1$Close[length(prices1$Close)])-total.stock1)+
    (stock2.position*as.double(prices2$Close[length(prices2$Close)])-total.stock2)
  printf("Current price stock1 %f, stock2 %f",as.double(prices1$Close[length(prices1$Close)]),as.double(prices2$Close[length(prices2$Close)]))
  printf("Current holdings stock1 %f, stock2 %f",stock1.position*as.double(prices1$Close[length(prices1$Close)]),stock2.position*as.double(prices2$Close[length(prices2$Close)]))
  printf("Booked profit %f, unrealized profit %f, total %f", profit, unrealized.profit, profit+unrealized.profit )
  printf("Highest exposure stock1 %f, stock2 %f, return %f", exposure.stock1, exposure.stock2, (profit+unrealized.profit)/(abs(exposure.stock1)+abs(exposure.stock2)))
  printf("Threshold crossings %d, zero crossings %d, loss crossings %d", threshold.crossings, zero.crossings, loss.crossings)
  printf("Total return %f, sharpe %f", sum(ret$returns), mean(ret$returns)/sd(ret$returns))
  # print(ret)
}


# #May 04 2017
# back.test("COALINDIA.NS","HINDALCO.NS",365,90)
# back.test("HDFCBANK.NS","HDFC.NS",365,90)
# back.test("BAJFINANCE.NS","IBULHSGFIN.NS",365,90)
# back.test("GAIL.NS","IOC.NS",365,90)
# back.test("COALINDIA.NS","JSWSTEEL.NS",365,90)
# back.test("HINDALCO.NS","JSWSTEEL.NS",365,90)
# back.test("HINDZINC.NS","JSWSTEEL.NS",365,90)
# back.test("IOC.NS","POWERGRID.NS",365,90)
# back.test("CUMMINSIND.NS","SIEMENS.NS",365,90)
# back.test("JSWSTEEL.NS","SAIL.NS",365,90)
# back.test("JSWSTEEL.NS","TATASTEEL.NS",365,90)
# back.test("HINDZINC.NS","VEDL.NS",365,90)
# back.test("JSWSTEEL.NS","VEDL.NS",365,90)
# back.test("SRTRANSFIN.NS","YESBANK.NS",365,90)

#May5
back.test("BANKBARODA.NS","UNIONBANK.NS",365,90)
back.test("LT.NS","OBEROIRLTY.NS",365,90)
back.test("BAJFINANCE.NS","MUTHOOTFIN.NS",365,90)
back.test("GRUH.NS","INDUSINDBK.NS",365,90)
back.test("GPPL.NS","INDHOTEL.NS",365,90)
back.test("BANKBARODA.NS","ICICIBANK.NS",365,90)
back.test("BANKBARODA.NS","HDFC.NS",365,90)
back.test("BAJAJFINSV.NS","EDELWEISS.NS",365,90)






