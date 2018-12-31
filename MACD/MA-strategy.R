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
  data <- Quandl(paste(stock.ticker.modified, sep=""), start_date=start_date, end_date=end_date, type="xts")
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

bollinger.sd<-function(prices, index, period){
  return (sd(prices$adj.close[max(index-period,1):index]))
}


min.pos.steps<-6    #minimum favorable steps to declare a trend as successful
neg.steps.to.end<-2   #minimum steps to end a trend
min.pos.steps.percent<-0.69    #minimum percentage of positive steps in a trend to declare it as positive
risk.free.rate<-0.08
min.sharpe.ratio<-1
annual.trading.days<-245
ret.annual.sd<-sqrt(annual.trading.days) * sd(diff(prices$adj.close)/prices$adj.close[-length(prices$adj.close)], na.rm = TRUE)
min.return<-(min.sharpe.ratio * ret.annual.sd) + risk.free.rate
direction<-0  #0 for short, 1 for long
  #0 for start, 1 for sequence of 2 found, 2 for closure
current.start<-0
sequence.start<-Sys.Date()
sequence.start.index<-0
i<-1
pos.count<-0
neg.count<-0
seq.return<-0.0
pos.percentage<-0.0
header<-c("stock.ticker", "pattern.start", "pattern.end", "direction", "pos.moves", "neg.moves", "total.moves",
          "seq.return", "pos.percentage", "bollinger.width", "bollinger.percentage","first.candle", "second.candle", "first.candle.stick", "second.candle.stick", 
           "first.neg.stick", "second.neg.stick", "viable")
result<-data.frame()
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
      sequence.start.index<-i+1
      if((as.numeric(ha$Close[i])-as.numeric(ha$Open[i]))>0){
        direction<-1
      }
      else{
        direction<-0
      }
      printf("starting trend %s and %d", sequence.start, direction)
      pos.count<-0
    }
  }
  else if(automata.state==1){
    if((direction==1 & (as.numeric(ha$Close[i])-as.numeric(ha$Open[i])<0 & (as.numeric(ha$Close[i-1])-as.numeric(ha$Open[i-1]))<0)) | 
      (direction==0 & (as.numeric(ha$Close[i])-as.numeric(ha$Open[i])>0 & (as.numeric(ha$Close[i-1])-as.numeric(ha$Open[i-1]))>0))) {
      automata.state<-2 #closure
      # printf("pattern from %s to %d in direction %d",sequence.start, i-2, direction)
      neg.count<-neg.count-1 # to remove the first of the two closure reversals
      seq.return<- ((as.numeric(prices$adj.close[as.Date(rownames(ha)[i-2])]) - as.numeric(prices$adj.close[sequence.start]))/as.numeric(prices$adj.close[sequence.start]))
      if(direction==0) {seq.return<- (-seq.return)}
      pos.percentage<-pos.count/(pos.count + neg.count)
      if((pos.count + neg.count) ==0) {pos.percentage<-0.0}
      bollinger.width<-bollinger.sd(prices[2:nrow(prices),], sequence.start.index-2, min(20, sequence.start.index-3) )
      bollinger.percentage<-bollinger.sd(prices[2:nrow(prices),], sequence.start.index-2, min(20, sequence.start.index-3) )/bollinger.sd(prices[2:nrow(prices),], sequence.start.index, min(150, sequence.start.index-1) )
      first.candle<- abs(as.numeric(ha$Close[sequence.start.index-2])-as.numeric(ha$Open[sequence.start.index-2]))/bollinger.width
      second.candle<-abs(as.numeric(ha$Close[sequence.start.index-1])-as.numeric(ha$Open[sequence.start.index-1]))/bollinger.width
      if(direction==1){
        first.candle.stick<-( (as.numeric(ha$High[sequence.start.index-2])-max(as.numeric(ha$Close[sequence.start.index-2]), as.numeric(ha$Open[sequence.start.index-2]) ))
                              - (-as.numeric(ha$Low[sequence.start.index-2])+min(as.numeric(ha$Close[sequence.start.index-2]), as.numeric(ha$Open[sequence.start.index-2]) )))/
                              bollinger.width
        second.candle.stick<-( (as.numeric(ha$High[sequence.start.index-1])-max(as.numeric(ha$Close[sequence.start.index-1]), as.numeric(ha$Open[sequence.start.index-1]) ))
                               - (-as.numeric(ha$Low[sequence.start.index-1])+min(as.numeric(ha$Close[sequence.start.index-1]), as.numeric(ha$Open[sequence.start.index-1]) )))/
                              bollinger.width
        first.neg.stick<- (-as.numeric(ha$Low[sequence.start.index-2])+min(as.numeric(ha$Close[sequence.start.index-2]), as.numeric(ha$Open[sequence.start.index-2])) )/
                              bollinger.width
        second.neg.stick<- (-as.numeric(ha$Low[sequence.start.index-1])+min(as.numeric(ha$Close[sequence.start.index-1]), as.numeric(ha$Open[sequence.start.index-1])) )/
                              bollinger.width
      }
      if(direction==0){
        first.candle.stick<-( (-as.numeric(ha$Low[sequence.start.index-2])+min(as.numeric(ha$Close[sequence.start.index-2]), as.numeric(ha$Open[sequence.start.index-2]) ))
                              -(as.numeric(ha$High[sequence.start.index-2])-max(as.numeric(ha$Close[sequence.start.index-2]), as.numeric(ha$Open[sequence.start.index-2]) )))/
                              bollinger.width
        second.candle.stick<-( (-as.numeric(ha$Low[sequence.start.index-1])+min(as.numeric(ha$Close[sequence.start.index-1]), as.numeric(ha$Open[sequence.start.index-1]) ))
                               -(as.numeric(ha$High[sequence.start.index-1])-max(as.numeric(ha$Close[sequence.start.index-1]), as.numeric(ha$Open[sequence.start.index-1]) )))/
                              bollinger.width
        first.neg.stick<- (as.numeric(ha$High[sequence.start.index-2])-max(as.numeric(ha$Close[sequence.start.index-2]), as.numeric(ha$Open[sequence.start.index-2]) ))/
                              bollinger.width
        second.neg.stick<- (as.numeric(ha$High[sequence.start.index-1])-max(as.numeric(ha$Close[sequence.start.index-1]), as.numeric(ha$Open[sequence.start.index-1]) ))/
                              bollinger.width
      }
      if(pos.count>min.pos.steps #viable sequence
         & (pos.percentage > min.pos.steps.percent)
         & (seq.return > min.return * (pos.count + neg.count) / annual.trading.days)){
        temp.row<-list("Stock ticker", sequence.start, as.Date(rownames(ha)[i-2]), direction, pos.count, neg.count, (pos.count+neg.count), seq.return, pos.percentage, bollinger.width, bollinger.percentage, 
                    first.candle, second.candle, first.candle.stick, second.candle.stick, first.neg.stick, second.neg.stick, 1 )
        printf(temp.row)
        # printf("viable pattern from %s to %s in direction %d with pos steps %d, seq return %f, pos percentage %f",sequence.start, as.Date(rownames(ha)[i-2]), direction, pos.count, seq.return, pos.percentage)
        i<-i-1
      }
      else{
        temp.row<-list("Stock ticker", sequence.start, as.Date(rownames(ha)[i-2]), direction, pos.count, neg.count, (pos.count+neg.count), seq.return, pos.percentage, bollinger.width, bollinger.percentage,
                    first.candle, second.candle, first.candle.stick, second.candle.stick, first.neg.stick, second.neg.stick, 0 )
        printf(temp.row)
        # printf("Unviable pattern from %s to %d in direction %d with pos steps %d, seq return %f, pos percentage %f",sequence.start, i-2, direction, pos.count, seq.return, pos.percentage)
        i<-current.start+1
      }
      result<-rbind(result, temp.row)
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

corp.actions <- read.csv(corp.action.file, header=TRUE)
corp.actions <-set.corp.action.data(corp.actions)
lookback<-500
prices <- adjust.prices("M&MFIN", Sys.Date() - lookback, Sys.Date()-1,corp.actions )
ha<-heikin.ashi(prices)

# last.open<-32.15
# last.close<-31.3
# today.close<-31.4
# today.open<-31.2
# today.high<-31.8
# today.low<-31.05


last.row<-data.frame((today.close+today.open+today.low+today.high)/4, (last.close+last.open)/2, today.high, today.low)
colnames(last.row)<-colnames(ha)
row.names(last.row)[1]<-'2017-07-12'
ha<-rbind(ha,last.row)

# ma1<-SMA(prices$adj.close,5)
# ma2<-SMA(prices$adj.close,10)
# ma3<-SMA(prices$adj.close,20)
# chart_Series(ha, TA=list("addTA(ma1, col=2)", "addTA(prices$adj.close, col=4, on=1)",
#                            "addTA(ma3, col=5, on=1)"), theme=chartTheme('white'))

chartSeries((ha), theme=chartTheme('white'))

addMACD(fast = 12, slow = 26, signal = 9 ) 
addMACD(fast = 20, slow = 40, signal = 9) 
addBBands() 
# addTA(log(prices$Total.Trade.Quantity)*mean(prices$adj.close)/log(mean(prices$Total.Trade.Quantity)), on = 1, col = "blue")

  lookback<-400
  prices <- adjust.prices("CNX_NIFTY", Sys.Date() - lookback, Sys.Date(),corp.actions )
  prices.normalized<-data.frame(normalize.prices(prices))[,c("Close", "Open","High", "Low")]
  # prices.normalized<-Quandl("BSE/BSEMID", start_date=Sys.Date() - lookback, end_date=Sys.Date(), type="xts")
  chartSeries(prices.normalized, theme=chartTheme('white'))
  addMACD(fast = 12, slow = 26, signal = 9) 
  # addMACD(fast = 5, slow = 15, signal = 7) 
  addBBands() 
  addSMI(n=13,slow=25,fast=2,signal=9,ma.type="EMA")
  # addSMI(n=40,slow=75,fast=6,signal=9,ma.type="EMA")
  # addADX(n = 40, maType="EMA", wilder=TRUE)
  addADX(n = 14, maType="EMA", wilder=TRUE)
addRSI()
addRSI(n=5)
addADX(n = 10, maType="EMA", wilder=TRUE)
addTA(stoch(normalize.prices(prices)[,c('High', 'Low', 'Close')], nFastK = 25, nFastD = 7, nSlowD = 7))
addTA(log(prices$Total.Trade.Quantity)*mean(prices$adj.close)/log(mean(prices$Total.Trade.Quantity)), on = 1, col = "blue")
# addTA(SMA(prices$adj.close, 10), on = 1, col = "blue")
# addTA(SMA(prices$adj.close, 30), on = 1, col = "green")
# addTA(SMA(prices$adj.close, 50), on = 1, col = "red")

corp.actions <- read.csv(corp.action.file, header=TRUE)
corp.actions <-set.corp.action.data(corp.actions)
lookback<-600
ma1.range<-10
ma2.range<-20
prices <- adjust.prices("CNX_NIFTY", Sys.Date() - lookback, Sys.Date(),corp.actions )
prices$ma1<-rollapply(prices$adj.close, width = ma1.range, FUN = mean, align="right")
prices$ma2<-rollapply(prices$adj.close, width = ma2.range, FUN = mean, align="right")
# prices$ma.diff<-c(rep(0,ma2.range-1),abs(prices$ma1[ma2.range:nrow(prices)]-prices$ma2[ma2.range:nrow(prices)]))
prices$ma.diff<-c(rep(0,ma2.range-1),(prices$ma1[ma2.range:nrow(prices)]-prices$ma2[ma2.range:nrow(prices)]))

df1<-as.data.frame(prices[ma2.range:nrow(prices),c("adj.close","ma1","ma2")])
df1$date1<-0
df1$date1<-(index(prices)[ma2.range:nrow(prices)])

p1 <- ggplot(df1, aes(x=df1$date1)) + 
  geom_line(aes(y = adj.close, colour = "Close")) + 
  geom_line(aes(y = ma1, colour = "short MA")) + 
  geom_line(aes(y = ma2, colour = "Long MA")) +
  ylab("Closing Prices") + xlab("") + 
  theme(legend.position="top") + 
  scale_colour_discrete(name="") + 
  ggtitle ("Price Series") 

print(p1, vp=viewport(layout.pos.row=1:7, layout.pos.col=1))

df2<-as.data.frame(prices[ma2.range:nrow(prices),c("ma.diff")])
df2$date1<-0
df2$date1<-(index(prices)[ma2.range:nrow(prices)])
df2$diff.mean<-mean(df2$ma.diff)

p2 <- ggplot(df2, aes(x=df1$date1)) + 
  geom_line(aes(y = diff.mean, colour = "avg")) + 
  geom_bar(aes(y = ma.diff),  stat = "identity") +
  ylab("MA Diff") + xlab("") + 
  theme(legend.position="top") + 
  scale_colour_discrete(name="") 
  

x <- min(df2$date1) 
ymin <- min(df2$ma.diff) 
ymax <- max(df2$ma.diff) 
y <- ymin + 0.96 * (ymax - ymin) 
l<-paste("SD=",sd(df2$ma.diff))
p2 <- p2 + annotate("text", x=x, y=y, label=l, colour="red", hjust=0, size=3) 
print(p2, vp=viewport(layout.pos.row=8:10, layout.pos.col=1))

candlecolors <- ifelse(prices[,'Close'] < prices[,'Open'], 'RED', 'GREEN')
plot.xts(prices, type='candles',  width=25000, candle.col=candlecolors, bar.col='BLACK')


chartSeries(prices,TA="addVo();addCCI()")  #add volume and Bollinger Bands from TTR

addMACD()   #  add MACD indicator to current chart

setTA()
chartSeries(prices, theme=chartTheme('white'))   #draws chart again, this time will all indicators present
