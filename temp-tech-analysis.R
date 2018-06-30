lookback<-150
prices <- adjust.prices("IDEA", Sys.Date() - lookback, Sys.Date(),corp.actions )

ha<-heikin.ashi(prices)

# last.open<-31.4
# last.close<-31.9
# today.close<-32.05
# today.open<-31.85
# today.high<-32.35
# today.low<-31.55

# last.open<-366.4
# last.close<-375.1
# today.close<-387.4
# today.open<-375.75
# today.high<-392
# today.low<-368.75
# 
# last.row<-data.frame((today.close+today.open+today.low+today.high)/4, (last.close+last.open)/2, today.high, today.low)
# colnames(last.row)<-colnames(ha)
# row.names(last.row)[1]<-'2017-07-12'
# ha<-rbind(ha,last.row)

chartSeries((ha), theme=chartTheme('white'))

addMACD(fast = 12, slow = 26, signal = 9 ) 
addMACD(fast = 20, slow = 40, signal = 9) 
addBBands() 
# addTA(log(prices$Total.Trade.Quantity)*mean(prices$adj.close)/log(mean(prices$Total.Trade.Quantity)), on = 1, col = "blue")

prices.normalized<-data.frame(normalize.prices(prices))
# last.row<-data.frame(today.close, today.open, today.high, today.low)
# colnames(last.row)<-colnames(prices.normalized)
# row.names(last.row)[1]<-'2017-07-12'
# prices.normalized<-rbind(prices.normalized,last.row)
chartSeries(prices.normalized, theme=chartTheme('white'))

addMACD(fast = 12, slow = 26, signal = 9) 
addMACD(fast = 20, slow = 40, signal = 9) 
addBBands() 


#y<-ifelse(x<=0, exp(0.5*x)*0.5, -1+(3-1.5*exp(-x)))