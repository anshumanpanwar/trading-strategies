library(Quandl)
library(httr)
library(tseries)
library(zoo)
library(ggplot2)
library(gridBase)
library(gridGraphics)
library(lubridate)
library(quantmod)

Quandl.api_key("rAHKcgv_ymH7_o9s5nWN")
corp.action.file<-"D:/Work/Stocks/Corporate_Actions.csv"

printf <- function(...)print(sprintf(...))

adjust.prices<-function(stock.ticker, start_date, end_date, corp.actions){
  printf("Starting for stock %s", stock.ticker)
  stock.ticker.modified=gsub("&","",stock.ticker)
  stock.ticker.modified=gsub("-","_",stock.ticker.modified)
  data <- Quandl(paste("NSE/", stock.ticker.modified, sep=""), start_date=start_date, end_date=end_date, type="xts")
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

heikin.ashi<-function(prices){
  data<-(prices$Close)
  data$Close<-prices$adj.close
  data$Open<-prices$Open*(prices$adj.close/prices$Close)
  data$High<-prices$High*(prices$adj.close/prices$Close)
  data$Low<-prices$Low*(prices$adj.close/prices$Close)
  
  data1<-as.data.frame(data$Close[2:nrow(data)])
  data1$Close<-(data$Close[2:nrow(data)]+data$Open[2:nrow(data)]+data$High[2:nrow(data)]+data$Low[2:nrow(data)])/4
  data1$Open<-(data$Close[1:nrow(data)-1]+data$Open[1:nrow(data)-1])/2
  data1$High<-apply(data[2:nrow(data), c("Open", "Close", "High")],1,max)
  data1$Low<-apply(data[2:nrow(data), c("Open", "Close", "Low")],1,min)
  # data1$High<-data$High
  # data1$Low<-data$Low
  
  data1<- data1[ , !(names(data1) %in%  c("V1"))]
  rownames(data1)<-index(prices)[2:nrow(prices)]
  return(data1)
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
  data$Open<-prices$Open*(prices$adj.close/prices$Close)
  data$High<-prices$High*(prices$adj.close/prices$Close)
  data$Low<-prices$Low*(prices$adj.close/prices$Close)
  return(data)
}

strength.score<-function(row, sd, avg, direction){
  printf("sd= %f and avg= %f", sd, avg)
  mid<-(as.numeric(row$Close)+as.numeric(row$Open))/2
  height<-abs(max(as.numeric(row$Close),as.numeric(row$Open),as.numeric(row$High))-
                min(as.numeric(row$Close),as.numeric(row$Open),as.numeric(row$Low)))
  dist<-(mid-avg)/sd
  if(direction==1) {dist<- (-dist)}
  score<-height/avg * ifelse(dist<=0, exp(0.5*dist)*0.5, -1+(3-1.5*exp(-dist)))
  return(score)
}



find.patterns<-function(ticker, prices, ha, nifty){
  min.pos.steps<-5    #minimum favorable steps to declare a trend as successful
  neg.steps.to.end<-2   #minimum steps to end a trend
  min.pos.steps.percent<-0.69    #minimum percentage of positive steps in a trend to declare it as positive
  risk.free.rate<-0.08
  min.sharpe.ratio<-1
  # macd.zero.signal.gap.threshold<-0.1  #the max gap between macd and signal line to decide crossing
  annual.trading.days<-245
  ret.annual.sd<-sqrt(annual.trading.days) * sd(diff(prices$adj.close)/prices$adj.close[-length(prices$adj.close)], na.rm = TRUE)
  min.return<-(min.sharpe.ratio * ret.annual.sd) + risk.free.rate
  #MACD construction
  macd  <- MACD( prices$adj.close, 12, 26, 9, maType="EMA", percent=TRUE )
  macd$bars<-macd$macd-macd$signal
  macd$bars.firstderivative<-diff(macd$bars)
  macd$bars.secondderivative<-diff(macd$bars.firstderivative)
  macd$bars.firstderivative.5sma<-SMA(macd$bars.firstderivative, n=5)
  macd$bars.secondderivative.3sma<-SMA(macd$bars.secondderivative, n=3)
  macd.zero.signal.gap.threshold<-sd(macd$bars, na.rm=TRUE)/2 #the max gap between macd and signal line to decide crossing
  
  #few vitals for stock
  stock.beta<-beta(prices, nifty)
  stock.rsquare<-rsquare(prices,nifty)
  stock.ret.var<-(sd(diff(prices$adj.close)/lag(prices$adj.close), na.rm = TRUE) ^2)
  
  direction<-0  #0 for short, 1 for long
  automata.state<-0 #0 for start, 1 for sequence of 2 found, 2 for closure
  current.start<-0
  sequence.start<-Sys.Date()
  first.candle.date<-Sys.Date()
  second.candle.date<-Sys.Date()
  sequence.start.index<-0
  i<-1
  pos.count<-0
  neg.count<-0
  seq.return<-0.0
  pos.percentage<-0.0
  header<-c("stock.ticker", "stock.beta", "stock.rsquare", "stock.ret.var", "pattern.start", "pattern.end", "direction", "pos.moves", "neg.moves", "total.moves",
            "seq.return", "pos.percentage", "bollinger.width", "bollinger.percentage","first.candle", "second.candle", "first.candle.stick", "second.candle.stick", 
            "first.bollinger.score", "second.bollinger.score", "first.bollinger.score.stick", "second.bollinger.score.stick", "first.neg.stick", "second.neg.stick", 
            "macd", "macd.bar", "macd.crossing", "macd.zero.signal", "macd.bar.dx", "macd.bar.d2x", "macd.bar.dx.sma", "macd.bar.d2x.sma", "score", "viable")
  result<-data.frame(stringsAsFactors = FALSE)
  while(i<=nrow(ha)){
    if(i==1){
      automata.state<-0
      i<-i+1
      next
    }
    # printf("i is %d", i)
    if(automata.state==0){
      if((as.numeric(ha$Close[i])-as.numeric(ha$Open[i])>0 & ((as.numeric(ha$Close[i-1])-as.numeric(ha$Open[i-1]))>0)) | 
         (as.numeric(ha$Close[i])-as.numeric(ha$Open[i])<0 & ((as.numeric(ha$Close[i-1])-as.numeric(ha$Open[i-1]))<0)) ){
        automata.state<-1
        current.start<-i
        sequence.start<-as.Date(rownames(ha)[i+1])  #first 2 days were indicators, position taken on 3rd day i+1
        date.before.first.candle<-as.Date(rownames(ha)[i-2])
        first.candle.date<-as.Date(rownames(ha)[i-1])
        second.candle.date<-as.Date(rownames(ha)[i])
        sequence.start.index<-i+1
        if((as.numeric(ha$Close[i])-as.numeric(ha$Open[i]))>0){
          direction<-1
        }
        else{
          direction<-0
        }
        printf("starting trend on date %s, start.index %d, i %d and direction %d", sequence.start, sequence.start.index, i, direction)
        pos.count<-0
      }
    }
    else if(automata.state==1){
      if((direction==1 & (as.numeric(ha$Close[i])-as.numeric(ha$Open[i])<0 & (as.numeric(ha$Close[i-1])-as.numeric(ha$Open[i-1]))<0)) | 
         (direction==0 & (as.numeric(ha$Close[i])-as.numeric(ha$Open[i])>0 & (as.numeric(ha$Close[i-1])-as.numeric(ha$Open[i-1]))>0))) {
        automata.state<-2 #closure
        # printf("pattern from %s to %d in direction %d",sequence.start, i-2, direction)
        neg.count<-neg.count-1 # to remove the first of the two closure reversals
        seq.return<- ((as.numeric(prices$adj.close[as.Date(rownames(ha)[i])]) - as.numeric(prices$adj.close[sequence.start]))/as.numeric(prices$adj.close[sequence.start]))
        if(direction==0) {seq.return<- (-seq.return)}
        pos.percentage<-pos.count/(pos.count + neg.count)
        if((pos.count + neg.count) ==0) {pos.percentage<-0.0}
        bollinger.sd.value<-bollinger.sd(prices[2:nrow(prices),], as.Date(rownames(ha)[match(as.character(sequence.start),rownames(ha))-2]), 20 )
        #Bollinger width in terms of percentage
        bollinger.width<-bollinger.sd.value/bollinger.ma(prices[2:nrow(prices),], as.Date(rownames(ha)[match(as.character(sequence.start),rownames(ha))-2]), 20 )
        bollinger.percentage<-bollinger.sd.value/bollinger.sd(prices[2:nrow(prices),], sequence.start, 150 )
        first.candle<- abs(as.numeric(ha$Close[sequence.start.index-2])-as.numeric(ha$Open[sequence.start.index-2]))/bollinger.sd.value
        second.candle<-abs(as.numeric(ha$Close[sequence.start.index-1])-as.numeric(ha$Open[sequence.start.index-1]))/bollinger.sd.value
        
        #MACD values
        macd.value<-as.numeric(macd$macd[as.Date(second.candle.date)])
        macd.bar<-as.numeric(macd$bars[as.Date(second.candle.date)])
        macd.bar.dx<-(as.numeric(macd$bars.firstderivative[as.Date(first.candle.date)]) + as.numeric(macd$bars.firstderivative[as.Date(second.candle.date)]))/2
        macd.bar.d2x<-as.numeric(macd$bars.secondderivative[as.Date(second.candle.date)])
        macd.bar.dx.sma<-as.numeric(macd$bars.firstderivative.5sma[as.Date(second.candle.date)])
        macd.bar.d2x.sma<-as.numeric(macd$bars.secondderivative.3sma[as.Date(second.candle.date)])
        
        #strength Score
        moving.avg<-bollinger.ma(prices[2:nrow(prices),], as.Date(rownames(ha)[match(as.character(sequence.start),rownames(ha))-2]), 20 )
        score<-strength.score(ha[sequence.start.index-1,], bollinger.sd.value, moving.avg, direction)+
          strength.score(ha[sequence.start.index-2,], bollinger.sd.value, moving.avg, direction) 
        
        if(direction==1){
          first.candle.stick<-( (as.numeric(ha$High[sequence.start.index-2])-max(as.numeric(ha$Close[sequence.start.index-2]), as.numeric(ha$Open[sequence.start.index-2]) ))
                                - (-as.numeric(ha$Low[sequence.start.index-2])+min(as.numeric(ha$Close[sequence.start.index-2]), as.numeric(ha$Open[sequence.start.index-2]) )))/
            bollinger.sd.value
          second.candle.stick<-( (as.numeric(ha$High[sequence.start.index-1])-max(as.numeric(ha$Close[sequence.start.index-1]), as.numeric(ha$Open[sequence.start.index-1]) ))
                                 - (-as.numeric(ha$Low[sequence.start.index-1])+min(as.numeric(ha$Close[sequence.start.index-1]), as.numeric(ha$Open[sequence.start.index-1]) )))/
            bollinger.sd.value
          first.neg.stick<- (-as.numeric(ha$Low[sequence.start.index-2])+min(as.numeric(ha$Close[sequence.start.index-2]), as.numeric(ha$Open[sequence.start.index-2])) )/
            bollinger.sd.value
          second.neg.stick<- (-as.numeric(ha$Low[sequence.start.index-1])+min(as.numeric(ha$Close[sequence.start.index-1]), as.numeric(ha$Open[sequence.start.index-1])) )/
            bollinger.sd.value
          first.bollinger.score <-(bollinger.ma(prices[2:nrow(prices),], as.Date(rownames(ha)[match(as.character(sequence.start),rownames(ha))-2]), 20 ) - (as.numeric(ha$Open[sequence.start.index-2])+as.numeric(ha$Close[sequence.start.index-2]))/2)/
            bollinger.sd.value
          second.bollinger.score <-(bollinger.ma(prices[2:nrow(prices),], as.Date(rownames(ha)[match(as.character(sequence.start),rownames(ha))-1]), 20 ) - (as.numeric(ha$Open[sequence.start.index-1])+as.numeric(ha$Close[sequence.start.index-1]))/2)/
            bollinger.sd.value
          #how many sd away is the upper tip of the stick for downtrend (lower tip for uptrend) from the MA line
          first.bollinger.score.stick <-(bollinger.ma(prices[2:nrow(prices),], as.Date(rownames(ha)[match(as.character(sequence.start),rownames(ha))-2]), 20 ) - as.numeric(ha$Low[sequence.start.index-2]))/
            bollinger.sd.value
          second.bollinger.score.stick <-(bollinger.ma(prices[2:nrow(prices),], as.Date(rownames(ha)[match(as.character(sequence.start),rownames(ha))-1]), 20 ) - as.numeric(ha$Low[sequence.start.index-1]))/
            bollinger.sd.value
          #macd crossing signal from below
          macd.crossing<-ifelse((macd.bar>-macd.zero.signal.gap.threshold & macd.bar<macd.zero.signal.gap.threshold), 1, 0)
          #both macd and signal are below zero levels
          macd.zero.signal<-ifelse(macd.crossing==1 & macd.value<0, 1, 0)
          
        }
        if(direction==0){
          first.candle.stick<-( (-as.numeric(ha$Low[sequence.start.index-2])+min(as.numeric(ha$Close[sequence.start.index-2]), as.numeric(ha$Open[sequence.start.index-2]) ))
                                -(as.numeric(ha$High[sequence.start.index-2])-max(as.numeric(ha$Close[sequence.start.index-2]), as.numeric(ha$Open[sequence.start.index-2]) )))/
            bollinger.sd.value
          second.candle.stick<-( (-as.numeric(ha$Low[sequence.start.index-1])+min(as.numeric(ha$Close[sequence.start.index-1]), as.numeric(ha$Open[sequence.start.index-1]) ))
                                 -(as.numeric(ha$High[sequence.start.index-1])-max(as.numeric(ha$Close[sequence.start.index-1]), as.numeric(ha$Open[sequence.start.index-1]) )))/
            bollinger.sd.value
          first.neg.stick<- (as.numeric(ha$High[sequence.start.index-2])-max(as.numeric(ha$Close[sequence.start.index-2]), as.numeric(ha$Open[sequence.start.index-2]) ))/
            bollinger.sd.value
          second.neg.stick<- (as.numeric(ha$High[sequence.start.index-1])-max(as.numeric(ha$Close[sequence.start.index-1]), as.numeric(ha$Open[sequence.start.index-1]) ))/
            bollinger.sd.value
          first.bollinger.score <-(-bollinger.ma(prices[2:nrow(prices),], as.Date(rownames(ha)[match(as.character(sequence.start),rownames(ha))-2]), 20 ) + (as.numeric(ha$Open[sequence.start.index-2])+as.numeric(ha$Close[sequence.start.index-2]))/2)/
            bollinger.sd.value
          second.bollinger.score <-(-bollinger.ma(prices[2:nrow(prices),], as.Date(rownames(ha)[match(as.character(sequence.start),rownames(ha))-1]), 20 ) + (as.numeric(ha$Open[sequence.start.index-1])+as.numeric(ha$Close[sequence.start.index-1]))/2)/
            bollinger.sd.value
          first.bollinger.score.stick <-(-bollinger.ma(prices[2:nrow(prices),], as.Date(rownames(ha)[match(as.character(sequence.start),rownames(ha))-2]), 20 ) + as.numeric(ha$High[sequence.start.index-2]))/
            bollinger.sd.value
          second.bollinger.score.stick <-(-bollinger.ma(prices[2:nrow(prices),], as.Date(rownames(ha)[match(as.character(sequence.start),rownames(ha))-1]), 20 ) + as.numeric(ha$High[sequence.start.index-1]))/
            bollinger.sd.value
          #macd crossing signal from below
          macd.crossing<-ifelse((macd.bar>-macd.zero.signal.gap.threshold & macd.bar<macd.zero.signal.gap.threshold), 1, 0)
          #both macd and signal are below zero levels
          macd.zero.signal<-ifelse(macd.crossing==1 & macd.value>0, 1, 0)
          #inverting MACD signals in case of downtrend
          macd.value<-(-macd.value)
          macd.bar<- (-macd.bar)
          macd.bar.dx<-(- macd.bar.dx)
          macd.bar.d2x<-(-macd.bar.d2x)
          macd.bar.dx.sma<-(-macd.bar.dx.sma)
          macd.bar.d2x.sma<-(-macd.bar.d2x.sma)
        }
        temp.row<-list(ticker, stock.beta, stock.rsquare, stock.ret.var, format(sequence.start, "%Y-%m-%d"), (rownames(ha)[i]), direction, as.numeric(pos.count), as.numeric(neg.count), as.numeric((pos.count+neg.count)), seq.return, pos.percentage, bollinger.width, bollinger.percentage, 
                       first.candle, second.candle, first.candle.stick, second.candle.stick, first.bollinger.score, second.bollinger.score, first.bollinger.score.stick, second.bollinger.score.stick, first.neg.stick, second.neg.stick,
                       macd.value, macd.bar,macd.crossing, macd.zero.signal, macd.bar.dx, macd.bar.d2x, macd.bar.dx.sma, macd.bar.d2x.sma, score )
        if(pos.count>=min.pos.steps #viable sequence
           & (pos.percentage > min.pos.steps.percent)
           & (seq.return > min.return * (pos.count + neg.count) / annual.trading.days)){
          temp.row<-append(temp.row,1)
          printf(unlist(temp.row))
          # printf("viable pattern from %s to %s in direction %d with pos steps %d, seq return %f, pos percentage %f",sequence.start, as.Date(rownames(ha)[i-2]), direction, pos.count, seq.return, pos.percentage)
          i<-i-1
        }
        else{
          temp.row<-append(temp.row,0)
          printf(unlist(temp.row))
          # printf("Unviable pattern from %s to %d in direction %d with pos steps %d, seq return %f, pos percentage %f",sequence.start, i-2, direction, pos.count, seq.return, pos.percentage)
          i<-current.start+1
        }
        result<-rbind.data.frame(result, temp.row, stringsAsFactors=FALSE)
        automata.state<-0
        pos.count<-0
        neg.count<-0
      }
      else{
        if(direction==1 & (as.numeric(ha$Close[i])-as.numeric(ha$Open[i])>0)){
          pos.count<- pos.count+1
        }
        else if(direction==1 & (as.numeric(ha$Close[i])-as.numeric(ha$Open[i])<0)) {
          neg.count<- neg.count+1
        }
        else if(direction==0 & (as.numeric(ha$Close[i])-as.numeric(ha$Open[i])<0)){
          pos.count<- pos.count+1
        }
        else if(direction==0 & (as.numeric(ha$Close[i])-as.numeric(ha$Open[i])>0)){
          neg.count<- neg.count+1
        }
        # printf("continuing sequence %d", i)
      }
    }
    i<-(i+1)
  }
  colnames(result)<-header
  return(result)
}

combine.patterns<-function(patterns, prices, ha){
  min.pos.steps<-5    #minimum favorable steps to declare a trend as successful
  neg.steps.to.end<-2   #minimum steps to end a trend
  min.pos.steps.percent<-0.50    #minimum percentage of positive steps in a trend to declare it as positive
  risk.free.rate<-0.08
  min.sharpe.ratio<-1
  # macd.zero.signal.gap.threshold<-0.1  #the max gap between macd and signal line to decide crossing
  annual.trading.days<-245
  ret.annual.sd<-sqrt(annual.trading.days) * sd(diff(prices$adj.close)/prices$adj.close[-length(prices$adj.close)], na.rm = TRUE)
  min.return<-(min.sharpe.ratio * ret.annual.sd) + risk.free.rate
  
  
  patterns$index<-c(1:nrow(patterns))
  viable.patterns<-patterns[patterns$viable==1,]
  
  remove.indices<-c()        #a vector of indices to be removed
  new.patterns<-data.frame(stringsAsFactors = FALSE)
  
  for(i in 1:nrow(viable.patterns)){
    trend<-viable.patterns[i,]
    patterns.before<-patterns[patterns$pattern.start>as.Date(trend$pattern.start)-30 & patterns$pattern.start<=as.Date(trend$pattern.start) & patterns$direction==trend$direction,]
    patterns.after<-patterns[patterns$pattern.start>=as.Date(trend$pattern.start) & patterns$pattern.start<as.Date(trend$pattern.start) +30 & patterns$direction==trend$direction,]
    for(j in 1:nrow(patterns.before)){
      for(k in 1:nrow(patterns.after)){
        # for(j in 1:2){
        #   for(k in 1:2){
        if(as.Date(patterns.after$pattern.end[k]) == as.Date(patterns.before$pattern.start[j])) {next}
        new.min.return<-min.return*as.numeric((as.Date(patterns.after$pattern.end[k]) - as.Date(patterns.before$pattern.start[j])))/annual.trading.days
        seq.return<- (as.numeric(prices$adj.close[as.Date(patterns.after$pattern.end[k])]) - as.numeric(prices$adj.close[as.Date(patterns.before$pattern.start[j])]))/as.numeric(prices$adj.close[as.Date(patterns.before$pattern.start[j])])
        if(trend$direction==0){seq.return<- -seq.return}
        pos.moves<-count.moves(ha, patterns.before$pattern.start[j], patterns.after$pattern.end[k], trend$direction)
        neg.moves<-count.moves(ha, patterns.before$pattern.start[j], patterns.after$pattern.end[k], 1-trend$direction)
        if(seq.return>=new.min.return & seq.return>trend$seq.return & pos.moves/(pos.moves+neg.moves)>min.pos.steps.percent){ #new viable pattern
          start<-patterns.before[j,]
          end<-patterns.after[k,]
          temp.row<-list(trend$stock.ticker, trend$stock.beta, trend$stock.rsquare, trend$stock.ret.var, start$pattern.start, end$pattern.end, trend$direction, pos.moves, neg.moves, pos.moves+neg.moves, seq.return, pos.moves/(pos.moves+neg.moves), start$bollinger.width, start$bollinger.percentage, 
                         start$first.candle, start$second.candle, start$first.candle.stick, start$second.candle.stick, start$first.bollinger.score, start$second.bollinger.score, start$first.bollinger.score.stick, start$second.bollinger.score.stick, start$first.neg.stick, start$second.neg.stick,
                         start$macd, start$macd.bar,start$macd.crossing, start$macd.zero.signal, start$macd.bar.dx, start$macd.bar.d2x, start$macd.bar.dx.sma, start$macd.bar.d2x.sma, start$score, 1 )
          new.patterns<-rbind.data.frame(new.patterns, temp.row, stringsAsFactors = FALSE)
          new.patterns
          remove.indices<-c(remove.indices, trend$index, start$index)
          remove.indices
        }
      }
    }
  }
  new.patterns$index<--9999
  header<-c("stock.ticker", "stock.beta", "stock.rsquare", "stock.ret.var", "pattern.start", "pattern.end", "direction", "pos.moves", "neg.moves", "total.moves",
            "seq.return", "pos.percentage", "bollinger.width", "bollinger.percentage","first.candle", "second.candle", "first.candle.stick", "second.candle.stick", 
            "first.bollinger.score", "second.bollinger.score", "first.bollinger.score.stick", "second.bollinger.score.stick", "first.neg.stick", "second.neg.stick", 
            "macd", "macd.bar", "macd.crossing", "macd.zero.signal", "macd.bar.dx", "macd.bar.d2x", "macd.bar.dx.sma", "macd.bar.d2x.sma", "score", "viable", "index")
  colnames(new.patterns)<-header
  result<-subset(patterns, !(index %in% remove.indices))
  result<-rbind.data.frame(result, new.patterns,stringsAsFactors = FALSE)
  return(result)
}

count.moves<-function(ha, start, end, sign){
  trend<-ha[rownames(ha)>=as.Date(start) & rownames(ha)<=as.Date(end),]
  count<-0
  for(i in 1:nrow(trend)){
    if(as.numeric(trend$Close[i])>as.numeric(trend$Open[i]) & sign==1){
      count<-count+1
    }
    else if(as.numeric(trend$Close[i])<as.numeric(trend$Open[i]) & sign==0){
      count<-count+1
    }
  }
  return(count)
}

get.macd<-function(prices, fast, slow, signal){
  #MACD construction
  macd  <- MACD( prices$adj.close, fast, slow, signal, maType="EMA", percent=TRUE )
  macd$bars<-macd$macd-macd$signal
  macd$bars.firstderivative<-diff(macd$bars)
  macd$bars.secondderivative<-diff(macd$bars.firstderivative)
  macd$bars.firstderivative.5sma<-SMA(macd$bars.firstderivative, n=5)
  macd$bars.secondderivative.3sma<-SMA(macd$bars.secondderivative, n=3)
  return(macd)
}

beta<-function(prices, nifty){
  # printf("in beta")
  prices
  nifty <-nifty[paste(index(prices)[1],index(prices)[length(index(prices))], sep="::"),]
  nifty<-nifty[index(prices),]
  prices<-prices[index(nifty),]
  ret.prices<-diff(prices$adj.close)/lag(prices$adj.close)
  ret.nifty<-diff(nifty$Close)/lag(nifty$Close)
  if(nrow(ret.prices)<=2){return(1)}
  return(as.numeric(coef(lm(ret.prices[2:nrow(ret.prices)]~ret.nifty[2:nrow(ret.nifty)]))[2]))
}
rsquare<-function(prices,nifty){
  # printf("in beta")
  prices
  nifty <- nifty[paste(index(prices)[1],index(prices)[length(index(prices))], sep="::"),]
  nifty<-nifty[index(prices),]
  prices<-prices[index(nifty),]
  ret.prices<-diff(prices$adj.close)/lag(prices$adj.close)
  ret.nifty<-diff(nifty$Close)/lag(nifty$Close)
  if(nrow(ret.prices)<=2){return(1)}
  return(as.numeric(cor(ret.nifty$Close[2:nrow(ret.nifty)], ret.prices$adj.close[2:nrow(ret.prices)])))
}

find.closures<-function(ticker, patterns, ha, prices, nifty){
  output<-data.frame(stringsAsFactors = FALSE)
  #constructs macd
  macd<-get.macd(prices, 12, 26, 9)
  macd.zero.signal.gap.threshold<-sd(macd$bars, na.rm=TRUE)/2 #the max gap between macd and signal line to decide crossing
  stock.beta<-patterns$stock.beta[1]
  stock.rsquare<-patterns$stock.rsquare[1]
  stock.ret.var<-patterns$stock.ret.var[1]
  
  for(i in 1:nrow(patterns)){
    pattern<-patterns[i,]
    start.date<-pattern$pattern.start
    end.date<-pattern$pattern.end
    start.index<-match(start.date, rownames(ha))
    end.index<-match(end.date, rownames(ha))
    pos.count<-0
    neg.count<-0
    for(j in start.index:(end.index-1)){
      if((pattern$direction==1 & as.numeric(ha$Close[j])-as.numeric(ha$Open[j])>0 ) | 
         (pattern$direction==0 & as.numeric(ha$Close[j])-as.numeric(ha$Open[j])<0 )) {
        pos.count<-pos.count+1
      }
      else{
        neg.count<-neg.count+1
      }
      
      if((pattern$direction==1 & (as.numeric(ha$Close[j])-as.numeric(ha$Open[j])<0 & (as.numeric(ha$Close[j+1])-as.numeric(ha$Open[j+1]))<0)) | 
         (pattern$direction==0 & (as.numeric(ha$Close[j])-as.numeric(ha$Open[j])>0 & (as.numeric(ha$Close[j+1])-as.numeric(ha$Open[j+1]))>0))) {
        printf("checking for %d. start.index= %d", j, start.index)
        if(nrow(patterns[patterns$pattern.end==rownames(ha)[j+1],]) >0 & j!=(end.index-1)){
          printf("End date %s already exists as viable for another pattern so skipping it", rownames(ha)[j+1])
          next
        }
        printf("end date %s is an unviable pattern. Start date %s", rownames(ha)[j+1], start.date)
        
        seq.return<- ((as.numeric(prices$adj.close[as.Date(rownames(ha)[j+1])]) - as.numeric(prices$adj.close[start.date]))/as.numeric(prices$adj.close[start.date]))
        if(pattern$direction==0) {seq.return<- (-seq.return)}
        
        #Ratio of sd for seq return/sd of return for last 150 days & excess beta
        index1<-match(as.Date(rownames(ha)[j]), index(prices))
        price.subset<-prices[max(1, index1-150):index1,]
        
        
        #sd and beta for seq returns
        seq.price.subset<-prices[paste(rownames(ha)[start.index],rownames(ha)[j+1], sep="::"),]
        if(nrow(seq.price.subset)<=2){
          printf("Seq length too smal to calculate sd and beta. Length %d", nrow(seq.price.subset))
          # seq.beta<-beta(price.subset)
          # seq.return.sd<-sd(diff(price.subset$adj.close)/lag(price.subset$adj.close), na.rm = TRUE)
          seq.return.sd<-sqrt(stock.ret.var)
        }
        else{
          seq.return.sd<-sd(diff(seq.price.subset$adj.close)/lag(seq.price.subset$adj.close), na.rm = TRUE)
          # seq.beta<-beta(seq.price.subset)
        }
        #excess of variance in retrns
        # seq.ret.variance.excess<-(seq.return.sd)^2-(sd(diff(price.subset$adj.close)/lag(price.subset$adj.close), na.rm = TRUE) ^2)
        # excess.beta<-seq.beta - beta(price.subset)
        seq.beta<-1
        excess.beta<-1
        seq.rsquare<-rsquare(seq.price.subset, nifty)
        seq.ret.variance.excess<-(seq.return.sd)^2-stock.ret.var
        
        pos.percentage<-pos.count/(pos.count + neg.count +1)  # +1 as we are looking ahead on 2nd candle stick at closure
        if((pos.count + neg.count) ==0) {pos.percentage<-0.0}
        bollinger.sd.value<-bollinger.sd(prices, as.Date(rownames(ha)[j+1]), 20 )
        #Bollinger width in terms of percentage
        bollinger.width<-bollinger.sd.value/bollinger.ma(prices, as.Date(rownames(ha)[j+1]), 20 )
        bollinger.percentage<-bollinger.sd.value/bollinger.sd(prices, as.Date(rownames(ha)[j+1]), 150 )
        first.candle<- abs(as.numeric(ha$Close[j])-as.numeric(ha$Open[j]))/bollinger.sd.value
        second.candle<-abs(as.numeric(ha$Close[j+1])-as.numeric(ha$Open[j+1]))/bollinger.sd.value
        first.candle.stick<-( (-as.numeric(ha$Low[j])+min(as.numeric(ha$Close[j]), as.numeric(ha$Open[j]) ))
                              +(as.numeric(ha$High[j])-max(as.numeric(ha$Close[j]), as.numeric(ha$Open[j]) )))/
          bollinger.sd.value
        second.candle.stick<-( (-as.numeric(ha$Low[j+1])+min(as.numeric(ha$Close[j+1]), as.numeric(ha$Open[j+1]) ))
                               +(as.numeric(ha$High[j+1])-max(as.numeric(ha$Close[j+1]), as.numeric(ha$Open[j+1]) )))/
          bollinger.sd.value
        
        #MACD values
        macd.value<-as.numeric(macd$macd[as.Date(rownames(ha)[j+1])])
        macd.bar<-as.numeric(macd$bars[as.Date(rownames(ha)[j+1])])
        macd.bar.dx<-(as.numeric(macd$bars.firstderivative[as.Date(rownames(ha)[j])]) + as.numeric(macd$bars.firstderivative[as.Date(rownames(ha)[j+1])]))/2
        macd.bar.d2x<-as.numeric(macd$bars.secondderivative[as.Date(rownames(ha)[j+1])])
        macd.bar.dx.sma<-as.numeric(macd$bars.firstderivative.5sma[as.Date(rownames(ha)[j+1])])
        macd.bar.d2x.sma<-as.numeric(macd$bars.secondderivative.3sma[as.Date(rownames(ha)[j+1])])
        
        if(pattern$direction==1){
          first.candle.stick.diff<-( (-as.numeric(ha$Low[j])+min(as.numeric(ha$Close[j]), as.numeric(ha$Open[j]) ))
                                -(as.numeric(ha$High[j])-max(as.numeric(ha$Close[j]), as.numeric(ha$Open[j]) )))/
                                  bollinger.sd.value
          second.candle.stick.diff<-( (-as.numeric(ha$Low[j+1])+min(as.numeric(ha$Close[j+1]), as.numeric(ha$Open[j+1]) ))
                                 -(as.numeric(ha$High[j+1])-max(as.numeric(ha$Close[j+1]), as.numeric(ha$Open[j+1]) )))/
                                  bollinger.sd.value
          first.bollinger.score <-(-bollinger.ma(prices, as.Date(rownames(ha)[j]), 20 ) + (as.numeric(ha$Open[j])+as.numeric(ha$Close[j]))/2)/
            bollinger.sd.value
          second.bollinger.score <-(-bollinger.ma(prices, as.Date(rownames(ha)[j+1]), 20 ) + (as.numeric(ha$Open[j+1])+as.numeric(ha$Close[j+1]))/2)/
            bollinger.sd.value
          #macd crossing signal from below
          macd.crossing<-ifelse((macd.bar>-macd.zero.signal.gap.threshold & macd.bar<macd.zero.signal.gap.threshold), 1, 0)
        }
        if(pattern$direction==0){
          first.candle.stick.diff<-((as.numeric(ha$High[j])-max(as.numeric(ha$Close[j]), as.numeric(ha$Open[j]) ))-
                                (-as.numeric(ha$Low[j])+min(as.numeric(ha$Close[j]), as.numeric(ha$Open[j]) )))/
                                bollinger.sd.value
          second.candle.stick.diff<-((as.numeric(ha$High[j+1])-max(as.numeric(ha$Close[j+1]), as.numeric(ha$Open[j+1]) ))- 
                                (-as.numeric(ha$Low[j+1])+min(as.numeric(ha$Close[j+1]), as.numeric(ha$Open[j+1]) )))/
                                bollinger.sd.value
          first.bollinger.score <-(bollinger.ma(prices, as.Date(rownames(ha)[j]), 20 ) - (as.numeric(ha$Open[j])+as.numeric(ha$Close[j]))/2)/
            bollinger.sd.value
          second.bollinger.score <-(bollinger.ma(prices, as.Date(rownames(ha)[j+1]), 20 ) - (as.numeric(ha$Open[j+1])+as.numeric(ha$Close[j+1]))/2)/
            bollinger.sd.value
          #macd crossing signal from below
          macd.crossing<-ifelse((macd.bar>-macd.zero.signal.gap.threshold & macd.bar<macd.zero.signal.gap.threshold), 1, 0)
          #inverting MACD signals in case of downtrend
          macd.value<-(-macd.value)
          macd.bar<- (-macd.bar)
          macd.bar.dx<-(- macd.bar.dx)
          macd.bar.d2x<-(-macd.bar.d2x)
          macd.bar.dx.sma<-(-macd.bar.dx.sma)
          macd.bar.d2x.sma<-(-macd.bar.d2x.sma)
        }
        temp.row<-list(ticker, stock.beta, start.date, (rownames(ha)[j+1]), pattern$direction, as.numeric(pos.count), as.numeric(neg.count+1), as.numeric((pos.count+neg.count+1)), seq.return, seq.return/(pos.count+neg.count+1), 
                       seq.return.sd, seq.ret.variance.excess, stock.rsquare, seq.rsquare, seq.beta, excess.beta, pos.percentage, bollinger.width, bollinger.percentage, 
                       first.candle, second.candle, first.candle.stick, second.candle.stick, first.candle.stick.diff, second.candle.stick.diff, first.bollinger.score, second.bollinger.score, 
                       macd.value, macd.bar,macd.crossing, macd.bar.dx, macd.bar.d2x, macd.bar.dx.sma, macd.bar.d2x.sma, ifelse(j==end.index-1, 1, 0) )  #unviable for all except the last sequence
        output<-rbind.data.frame(output, temp.row, stringsAsFactors=FALSE)
      }
    }
  }
  header<-c("stock.ticker", "stock.beta", "pattern.start", "pattern.end", "direction", "pos.moves", "neg.moves", "total.moves",
            "seq.return", "return.per.day", "seq.return.sd", "seq.ret.variance.excess", "stock.rsquare", "seq.rsquare", "seq.beta", "excess.beta", "pos.percentage", "bollinger.width", "bollinger.percentage","first.candle", "second.candle", "first.candle.stick", "second.candle.stick", 
             "first.candle.stick.diff", "second.candle.stick.diff","first.bollinger.score", "second.bollinger.score", 
            "macd", "macd.bar", "macd.crossing", "macd.bar.dx", "macd.bar.d2x", "macd.bar.dx.sma", "macd.bar.d2x.sma", "viable")
  colnames(output)<-header
  output<-output[!duplicated(output[c('pattern.start', 'pattern.end')]),]
  return(output)
}

# tickers_100<-c('ICICIPRULI.NS')
tickers_100<-c('ABB.NS','ACC.NS','ADANIPORTS.NS','AMBUJACEM.NS','ASHOKLEY.NS','ASIANPAINT.NS','AUROPHARMA.NS','AXISBANK.NS','BAJAJ-AUTO.NS','BAJFINANCE.NS','BAJAJFINSV.NS','BANKBARODA.NS','BEL.NS','BHEL.NS','BPCL.NS','BHARTIARTL.NS','INFRATEL.NS','BOSCHLTD.NS','BRITANNIA.NS','CADILAHC.NS','CIPLA.NS','COALINDIA.NS','COLPAL.NS','CONCOR.NS','CUMMINSIND.NS','DLF.NS','DABUR.NS','DIVISLAB.NS','DRREDDY.NS','EICHERMOT.NS','EMAMILTD.NS','GAIL.NS','GSKCONS.NS','GLAXO.NS','GLENMARK.NS','GODREJCP.NS','GRASIM.NS','HCLTECH.NS','HDFCBANK.NS','HAVELLS.NS','HEROMOTOCO.NS','HINDALCO.NS','HINDPETRO.NS','HINDUNILVR.NS','HINDZINC.NS','HDFC.NS','ITC.NS','ICICIBANK.NS','ICICIPRULI.NS','IDEA.NS','IBULHSGFIN.NS','IOC.NS','INDUSINDBK.NS','INFY.NS','INDIGO.NS','JSWSTEEL.NS','KOTAKBANK.NS','LICHSGFIN.NS','LT.NS','LUPIN.NS','M&M.NS','MARICO.NS','MARUTI.NS','MOTHERSUMI.NS','NHPC.NS','NMDC.NS','NTPC.NS','ONGC.NS','OIL.NS','OFSS.NS','PETRONET.NS','PIDILITIND.NS','PEL.NS','PFC.NS','POWERGRID.NS','PGHH.NS','PNB.NS','RELIANCE.NS','RECLTD.NS','SHREECEM.NS','SRTRANSFIN.NS','SIEMENS.NS','SBIN.NS','SAIL.NS','SUNPHARMA.NS','TCS.NS','TATAMTRDVR.NS','TATAMOTORS.NS','TATAPOWER.NS','TATASTEEL.NS','TECHM.NS','TITAN.NS','TORNTPHARM.NS','UPL.NS','ULTRACEMCO.NS','UBL.NS','MCDOWELL-N.NS','VEDL.NS','WIPRO.NS','YESBANK.NS','ZEEL.NS')
# tickers_100<-c('VEDL.NS','WIPRO.NS','YESBANK.NS','ZEEL.NS')
corp.actions <- read.csv(corp.action.file, header=TRUE)
corp.actions <-set.corp.action.data(corp.actions)
lookback<-1100
additional.lookback<-200
output<-data.frame(stringsAsFactors = FALSE)
closures<-data.frame(stringsAsFactors = FALSE)
nifty <- Quandl("NSE/CNX_NIFTY", start_date=Sys.Date() - lookback- additional.lookback, end_date= Sys.Date(), type="xts")

for(i in 1:length(tickers_100)){
  prices <- adjust.prices(substr(tickers_100[i], 1, nchar(tickers_100[i])-3), Sys.Date() - lookback, Sys.Date(),corp.actions )
  prices2 <- adjust.prices(substr(tickers_100[i], 1, nchar(tickers_100[i])-3), Sys.Date() - lookback- additional.lookback, Sys.Date(),corp.actions )
  ha<-heikin.ashi(prices)
  temp<-find.patterns(tickers_100[i],prices2,ha, nifty)
  temp<-combine.patterns(temp,prices2,ha)
  temp<-temp[!duplicated(temp[c('pattern.start', 'pattern.end')]),]   #removing duplicate patterns
  output<-rbind.data.frame(output, temp,stringsAsFactors = FALSE)
  temp2<-find.closures(tickers_100[i], output[output$viable==1,], ha , prices2, nifty)
  closures<-rbind.data.frame(closures, temp2,stringsAsFactors = FALSE)
  write.table(temp, file = paste("D://Work/Stocks/ha-append", format(Sys.Date(), "%Y%m%d"),".csv", sep=""), append = TRUE, col.names = FALSE, sep = ",", row.names=FALSE)
  write.table(temp2, file = paste("D://Work/Stocks/closures-append", format(Sys.Date(), "%Y%m%d"),".csv", sep=""), append = TRUE, col.names = FALSE, sep = ",", row.names=FALSE)
  # output<-rbind.data.frame(output, find.patterns(tickers_100[i],prices2,ha),stringsAsFactors = FALSE)
}

write.table(output, file = paste("D://Work/Stocks/ha-", format(Sys.Date(), "%Y%m%d"),".csv"), append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)
write.table(output, file = paste("D://Work/Stocks/closures-", format(Sys.Date(), "%Y%m%d"),".csv", sep=""), append = FALSE, col.names = TRUE, sep = ",", row.names=FALSE)