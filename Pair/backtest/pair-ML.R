library(sqldf)
library(randomForest)
library(pROC)
library(ROCR)
library(stats)
library(h2o)
library(caret)
library(xgboost)

work.dir<-"D:/Work/Stocks/pair/"
# pair.file<-paste(work.dir, "pairs-20180404.csv", sep="")
trade.file<-paste(work.dir, "trades-20180614.csv", sep="")
sectors.file<-paste(work.dir, "customized-sectors.csv" , sep="")

# pairs<-read.csv(pair.file, header = TRUE, stringsAsFactors = FALSE)
trades<-read.csv(trade.file, header = TRUE, stringsAsFactors = FALSE)
sectors<-read.csv(sectors.file, header=TRUE, stringsAsFactors = FALSE)

trades$start_date<-as.Date(trades$start_date, "%Y-%m-%d")
trades$end_date<-as.Date(trades$end_date, "%Y-%m-%d")
# query.string<-"select trades.*, pairs.residual_sd1, pairs.residual_sd2 from trades left join pairs on (((trades.long_stock=pairs.stock1 & trades.short_stock=pairs.stock2) | (trades.long_stock=pairs.stock2 & trades.short_stock=pairs.stock1)) & (trades.start_date=pairs.date))"
# temp<-sqldf(query.string)

# trades$residual.sd1<-0
# trades$residual.sd2<-0
# for(i in 1:nrow(trades)){
#   print(i)
#   trades$residual.sd1[i] <- pairs[((trades$long_stock[i]==pairs$stock1 & trades$short_stock[i]==pairs$stock2) | (trades$long_stock[i]==pairs$stock2 & trades$short_stock[i]==pairs$stock1)) & trades$start_date[i]==pairs$date, "residual_sd1"]
#   trades$residual.sd2[i] <- pairs[((trades$long_stock[i]==pairs$stock1 & trades$short_stock[i]==pairs$stock2) | (trades$long_stock[i]==pairs$stock2 & trades$short_stock[i]==pairs$stock1)) & trades$start_date[i]==pairs$date, "residual_sd2"]
# }

trades$duplicate<-FALSE
for(i in 2:nrow(trades)){
  print(i)
  trades$duplicate[i]<- ((trades$long_stock[i-1]==trades$long_stock[i] & trades$short_stock[i-1]==trades$short_stock[i]) | (trades$long_stock[i-1]==trades$short_stock[i] & trades$short_stock[i-1]==trades$long_stock[i])) & (trades$end_date[i-1]>trades$start_date[i])
}

trades<-trades[trades$duplicate==FALSE,]

trades$profitable<-!trades$stop_loss

trades$pair<-ifelse(as.character(trades$long_stock)>as.character(trades$short_stock), paste(trades$short_stock,trades$long_stock,sep="-"), paste(trades$long_stock,trades$short_stock,sep="-") )

cutoff.date<-as.Date(min(trades$start_date))+as.integer(as.integer(as.Date(max(trades$start_date))-as.Date(min(trades$start_date)))*0.9)
trades1<-trades[trades$start_date<=cutoff.date,]
# # smp_size <- floor(0.9 * nrow(trades1))
# ## set the seed to make your partition reproductible
# set.seed(730)
# train.ind <- createDataPartition(trades1$pair, p = .9, list =FALSE)
# # train.ind <- sample(seq_len(nrow(trades1)), size = smp_size)
# training <- trades1[train.ind, ]
training<-trades1[format(trades1$start_date,"%m")!="06",]

query.string<-"select pair, avg(profitable) as perc_profitability, avg(profit) as avg_profit,sum(profit) as total_profit, avg(holding_period) as avg_holding, count(*) as count from training group by pair"
summary<-sqldf(query.string)
# write.csv(summary, file=paste(work.dir, "pair-stats.csv", sep=""), col.names = TRUE)

# trades$life<-trades$holding_period
# trades$life[!trades$clean_exit]<-1000000
trades$rsi_diff<-trades$short_rsi-trades$long_rsi
trades$long_rel_rsi<-trades$long_rsi-trades$nifty_rsi
trades$short_rel_rsi<-trades$short_rsi-trades$nifty_rsi
# trades$long_stock<-as.factor(trades$long_stock)
# trades$short_stock<-as.factor(trades$short_stock)

trades$rlast_ratio<-abs((trades$entry_r1-trades$entry_r2)/trades$entry_r1)
trades$half_life_ratio<-(trades$half_life2-trades$half_life1)/trades$half_life1


lookback<-90
trades$start_date<-as.Date(trades$start_date)
trades$end_date<-as.Date(trades$end_date)
trades$adf1_mean<-0
trades$adf2_mean<-0
trades$pp1_mean<-0
trades$pp2_mean<-0
trades$pgff1_mean<-0
trades$pgff2_mean<-0
trades$ersd1_mean<-0
trades$ersd2_mean<-0
trades$jot1_mean<-0
trades$jot2_mean<-0
trades$spr1_mean<-0
trades$spr2_mean<-0
trades$half_life1_mean<-0
trades$half_life2_mean<-0
trades$sector<-NULL
trades$residual.ratio1<-0
trades$residual.ratio2<-0
trades$is.cointegrated1<-FALSE
trades$is.cointegrated2<-FALSE
col1.list<-c('adf1_mean','adf2_mean','pp1_mean','pp2_mean','pgff1_mean','pgff2_mean','ersd1_mean','ersd2_mean','jot1_mean','jot2_mean','spr1_mean','spr2_mean','half_life1_mean','half_life2_mean')
col2.list<-c('adf1','adf2','pp1','pp2','pgff1','pgff2','ersd1','ersd2','jot1','jot2','spr1','spr2','half_life1','half_life2')

trades$perc_profitability<-0
trades$avg_profit<-0
trades$total_profit<-0
trades$avg_holding<-0
trades$count<-0
trades$profitability3m<-0
trades$profitability6m<-0
trades$profitability1Y<-0
trades$count3m<-0
trades$count6m<-0
trades$count1Y<-0
trades$profit_count3m<-0
trades$profit_count6m<-0
trades$profit_count1Y<-0
trades$res_perc1<-0
trades$res_perc2<-0

for(i in 1:nrow(trades)){
  if(i%%1==0){print(i)}
  # stock1<-as.character(trades$long_stock[i])
  # stock2<-as.character(trades$short_stock[i])
  # v.date<-trades$start_date[i]
  # pair.row<-pairs[pairs$date==v.date & ((as.character(pairs$stock1)==stock1 & as.character(pairs$stock2)==stock2) | (as.character(pairs$stock2)==stock1 & as.character(pairs$stock1)==stock2)),]
  # if(as.character(pair.row$stock2)==stock1) {
  #   trades$residual.ratio1[i]<-pair.row$residual_sd1/trades$entry_price_long[i]
  #   trades$residual.ratio2[i]<-pair.row$residual_sd2/trades$entry_price_long[i]
  # }
  # else{
  #   trades$residual.ratio1[i]<-pair.row$residual_sd1/trades$entry_price_short[i]
  #   trades$residual.ratio2[i]<-pair.row$residual_sd2/trades$entry_price_short[i]
  # }
  # trades$is.cointegrated1<-pair.row$is_conintegrated1
  # trades$is.cointegrated2<-pair.row$is_conintegrated2
  # trades[i,col1.list]<-colMeans(pairs[((pairs$stock1==stock1 & pairs$stock2==stock2) | (pairs$stock1==stock2 & pairs$stock2==stock1)) &  pairs$date<=v.date & pairs$date>(v.date-lookback),col2.list])
  # trades$adf1_mean[i]<-mean(pairs$adf1[((pairs$stock1==stock1 & pairs$stock2==stock2) | (pairs$stock1==stock2 & pairs$stock2==stock1)) &  pairs$date<=v.date & pairs$date>(v.date-lookback)])
  # trades$adf2_mean[i]<-mean(pairs$adf2[((pairs$stock1==stock1 & pairs$stock2==stock2) | (pairs$stock1==stock2 & pairs$stock2==stock1)) &  pairs$date<=v.date & pairs$date>(v.date-lookback)])
  # trades$pp1_mean[i]<-mean(pairs$pp1[((pairs$stock1==stock1 & pairs$stock2==stock2) | (pairs$stock1==stock2 & pairs$stock2==stock1)) &  pairs$date<=v.date & pairs$date>(v.date-lookback)])
  # trades$pp2_mean[i]<-mean(pairs$pp2[((pairs$stock1==stock1 & pairs$stock2==stock2) | (pairs$stock1==stock2 & pairs$stock2==stock1)) &  pairs$date<=v.date & pairs$date>(v.date-lookback)])
  # trades$pgff1_mean[i]<-mean(pairs$pgff1[((pairs$stock1==stock1 & pairs$stock2==stock2) | (pairs$stock1==stock2 & pairs$stock2==stock1)) &  pairs$date<=v.date & pairs$date>(v.date-lookback)])
  # trades$pgff2_mean[i]<-mean(pairs$pgff2[((pairs$stock1==stock1 & pairs$stock2==stock2) | (pairs$stock1==stock2 & pairs$stock2==stock1)) &  pairs$date<=v.date & pairs$date>(v.date-lookback)])
  # trades$ersd1_mean[i]<-mean(pairs$ersd1[((pairs$stock1==stock1 & pairs$stock2==stock2) | (pairs$stock1==stock2 & pairs$stock2==stock1)) &  pairs$date<=v.date & pairs$date>(v.date-lookback)])
  # trades$ersd2_mean[i]<-mean(pairs$ersd2[((pairs$stock1==stock1 & pairs$stock2==stock2) | (pairs$stock1==stock2 & pairs$stock2==stock1)) &  pairs$date<=v.date & pairs$date>(v.date-lookback)])
  # trades$jot1_mean[i]<-mean(pairs$jot1[((pairs$stock1==stock1 & pairs$stock2==stock2) | (pairs$stock1==stock2 & pairs$stock2==stock1)) &  pairs$date<=v.date & pairs$date>(v.date-lookback)])
  # trades$jot2_mean[i]<-mean(pairs$jot2[((pairs$stock1==stock1 & pairs$stock2==stock2) | (pairs$stock1==stock2 & pairs$stock2==stock1)) &  pairs$date<=v.date & pairs$date>(v.date-lookback)])
  # trades$spr1_mean[i]<-mean(pairs$spr1[((pairs$stock1==stock1 & pairs$stock2==stock2) | (pairs$stock1==stock2 & pairs$stock2==stock1)) &  pairs$date<=v.date & pairs$date>(v.date-lookback)])
  # trades$spr2_mean[i]<-mean(pairs$spr2[((pairs$stock1==stock1 & pairs$stock2==stock2) | (pairs$stock1==stock2 & pairs$stock2==stock1)) &  pairs$date<=v.date & pairs$date>(v.date-lookback)])
  # trades$half_life1_mean[i]<-mean(pairs$half_life1[((pairs$stock1==stock1 & pairs$stock2==stock2) | (pairs$stock1==stock2 & pairs$stock2==stock1)) &  pairs$date<=v.date & pairs$date>(v.date-lookback)])
  # trades$half_life2_mean[i]<-mean(pairs$half_life2[((pairs$stock1==stock1 & pairs$stock2==stock2) | (pairs$stock1==stock2 & pairs$stock2==stock1)) &  pairs$date<=v.date & pairs$date>(v.date-lookback)])
  trades$sector[i]<-sectors$SECTOR[sectors$STOCK==trades$long_stock[i]]
  if(nrow(summary[summary$pair==trades$pair[i],])>0){
    trades$perc_profitability[i]<-summary$perc_profitability[summary$pair==trades$pair[i]]
    trades$avg_profit[i]<-summary$avg_profit[summary$pair==trades$pair[i]]
    trades$total_profit[i]<-summary$total_profit[summary$pair==trades$pair[i]]
  }else{
    printf("pair not found %s", trades$pair[i])
    trades$perc_profitability[i]<-mean(summary$perc_profitability)
    trades$avg_profit[i]<-mean(summary$avg_profit)
    trades$total_profit[i]<-0
  }
  # trades$profitability3m[i]<-nrow(trades[((trades$long_stock==trades$long_stock[i] & trades$short_stock==trades$short_stock[i]) | (trades$long_stock==trades$short_stock[i] & trades$short_stock==trades$long_stock[i])) & trades$profitable==TRUE & trades$end_date<trades$start_date[i] & trades$end_date>(trades$start_date[i]-90) ,])/
  #   nrow(trades[((trades$long_stock==trades$long_stock[i] & trades$short_stock==trades$short_stock[i]) | (trades$long_stock==trades$short_stock[i] & trades$short_stock==trades$long_stock[i])) & trades$end_date<trades$start_date[i] & trades$end_date>(trades$start_date[i]-90),])
  # trades$profitability6m[i]<-nrow(trades[((trades$long_stock==trades$long_stock[i] & trades$short_stock==trades$short_stock[i]) | (trades$long_stock==trades$short_stock[i] & trades$short_stock==trades$long_stock[i])) & trades$profitable==TRUE & trades$end_date<trades$start_date[i] & trades$end_date>(trades$start_date[i]-180) ,])/
  #   nrow(trades[((trades$long_stock==trades$long_stock[i] & trades$short_stock==trades$short_stock[i]) | (trades$long_stock==trades$short_stock[i] & trades$short_stock==trades$long_stock[i])) & trades$end_date<trades$start_date[i] & trades$end_date>(trades$start_date[i]-180),])
  # trades$profitability1Y[i]<-nrow(trades[((trades$long_stock==trades$long_stock[i] & trades$short_stock==trades$short_stock[i]) | (trades$long_stock==trades$short_stock[i] & trades$short_stock==trades$long_stock[i])) & trades$profitable==TRUE & trades$end_date<trades$start_date[i] & trades$end_date>(trades$start_date[i]-365) ,])/
  #   nrow(trades[((trades$long_stock==trades$long_stock[i] & trades$short_stock==trades$short_stock[i]) | (trades$long_stock==trades$short_stock[i] & trades$short_stock==trades$long_stock[i])) & trades$end_date<trades$start_date[i] & trades$end_date>(trades$start_date[i]-365),])
  # if(is.nan(trades$profitability3m[i])) {trades$profitability3m[i]<-0}
  # if(is.nan(trades$profitability6m[i])) {trades$profitability6m[i]<-0}
  # if(is.nan(trades$profitability1Y[i])) {trades$profitability1Y[i]<-0}
  # 
  # trades$profit_count3m[i]<-nrow(trades[((trades$long_stock==trades$long_stock[i] & trades$short_stock==trades$short_stock[i]) | (trades$long_stock==trades$short_stock[i] & trades$short_stock==trades$long_stock[i])) & trades$profitable==TRUE & trades$end_date<trades$start_date[i] & trades$end_date>(trades$start_date[i]-90) ,])
  # trades$profit_count6m[i]<-nrow(trades[((trades$long_stock==trades$long_stock[i] & trades$short_stock==trades$short_stock[i]) | (trades$long_stock==trades$short_stock[i] & trades$short_stock==trades$long_stock[i])) & trades$profitable==TRUE & trades$end_date<trades$start_date[i] & trades$end_date>(trades$start_date[i]-180) ,])
  # trades$profit_count1Y[i]<-nrow(trades[((trades$long_stock==trades$long_stock[i] & trades$short_stock==trades$short_stock[i]) | (trades$long_stock==trades$short_stock[i] & trades$short_stock==trades$long_stock[i])) & trades$profitable==TRUE & trades$end_date<trades$start_date[i] & trades$end_date>(trades$start_date[i]-365) ,])
  if(trades$entry_r1[i]>0 & trades$entry_r1[i]>0){
    trades$res_perc1[i]<-trades$residual_sd1[i]/trades$entry_price_short[i]
    trades$res_perc2[i]<-trades$residual_sd2[i]/trades$entry_price_short[i]
  }
  else if(trades$entry_r1[i]<0 & trades$entry_r1[i]<0){
    trades$res_perc1[i]<-trades$residual_sd1[i]/trades$entry_price_long[i]
    trades$res_perc2[i]<-trades$residual_sd2[i]/trades$entry_price_long[i]
  }

}
trades$entry_r1<-abs(trades$entry_r1)
trades$entry_r2<-abs(trades$entry_r2)
trades$sector<-as.factor(trades$sector)
trades$half_life1_mean[trades$half_life1_mean==Inf]<-2000
trades$half_life2_mean[trades$half_life2_mean==Inf]<-2000


trades1<-trades[trades$start_date<=cutoff.date,]
training<-trades1[format(trades1$start_date,"%m")!="06",]

testing<-trades[trades$start_date>cutoff.date,]
validation <- trades1[format(trades1$start_date,"%m")=="06",]

# features<-c("profitable","entry_r1","entry_r2","pp1","pp2","adf1","adf2","pgff","pgff2","ersd","ersd2","jot","jot2","spr","spr2","half_life1","half_life2","nifty_adx","nifty_rsi","long_rsi", "short_rsi","long_adx","short_adx","rsi_diff","long_rel_rsi","short_rel_rsi","sector", "perc_profitability")
features<-c("profitable","entry_r1","entry_r2","pp1","pp2","adf1","adf2","pgff","pgff2","ersd","ersd2","jot","jot2","spr","spr2","half_life1","half_life2","nifty_adx","long_adx","short_adx","nifty_rsi","long_rsi", "short_rsi", "sector", "rlast_ratio", "perc_profitability", "avg_profit", "total_profit", "res_perc1", "res_perc2")
# features<-c("profitable","sector","entry_r2","perc_profitability","avg_profit","total_profit","entry_r1","nifty_adx","nifty_rsi","long_adx","jot")
# features<-c("profitable","long_stock", "short_stock", "entry_r1","entry_r2","pp1","pp2","adf1","adf2","pgff","pgff2","ersd","ersd2","jot","jot2","spr","spr2","half_life1","half_life2","nifty_adx","nifty_rsi","long_rsi", "short_rsi","long_adx","short_adx","rsi_diff","long_rel_rsi","short_rel_rsi","pp1_mean","pp2_mean","adf1_mean","adf2_mean","pgff_mean","pgff2_mean","ersd_mean","ersd2_mean","jot_mean","jot2_mean","spr_mean","spr2_mean", "half_life1_mean","half_life2_mean")
# features<-c("stop_loss","sector", "entry_r1","entry_r2","pp1","pp2","adf1","adf2","pgff","pgff2","ersd","ersd2","jot","jot2","spr","spr2","half_life1","half_life2","nifty_adx","nifty_rsi","long_rsi", "short_rsi","long_adx","short_adx","rsi_diff","long_rel_rsi","short_rel_rsi")
# features<-c("profitable","sector", "entry_r1","entry_r2","pp1","pp2","adf1","adf2","pgff","pgff2","ersd","ersd2","jot","jot2","spr","spr2","half_life1","half_life2","nifty_adx","nifty_rsi","long_rsi", "short_rsi","long_adx","short_adx","rsi_diff","long_rel_rsi","short_rel_rsi")
# features<-c("profitable","entry_r1","entry_r2","pp1","pp2","adf1","adf2","pgff","pgff2","ersd","ersd2","jot","jot2","spr","spr2","half_life1","half_life2","nifty_adx","long_adx","short_adx","nifty_rsi","long_rsi", "short_rsi", "sector", "rlast_ratio", "perc_profitability", "avg_profit", "total_profit", "res_perc1", "res_perc2")



# features<-c("life","entry_r1","entry_r2","end_r1","end_r2","pp1","pp2","adf1","adf2","pgff","pgff2","ersd","ersd2","jot","jot2","spr","spr2","half_life1","half_life2","nifty_adx","nifty_rsi","long_rsi", "short_rsi","long_adx","short_adx","rsi_diff","long_rel_rsi","short_rel_rsi")

# model.rf <- randomForest(life~., data=training[, names(training) %in% features], ntree=10, importance=T, na.action = na.omit)
# model.rf <- randomForest(stop_loss~., data=training[, names(training) %in% features], ntree=500, importance=T, na.action = na.omit)
model.rf <- randomForest(profitable~., data=training[, names(training) %in% features], ntree=200, importance=T, na.action = na.omit, do.trace=10)
validation$predictions<-predict( model.rf, validation)
roc_obj <- roc(validation$profitable, validation$predictions)
auc(roc_obj)
print(model.rf)

pred <- prediction(validation$predictions, validation$profitable)
perf <- performance(pred,"tpr","fpr")
# changing params for the ROC plot - width, etc
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
# plotting the ROC curve
plot(perf,col="black",lty=3, lwd=3)

perf <- performance(pred,"prec","rec")  #generating performance measure table using ROCR package
df <- data.frame(cut = perf@alpha.values[[1]], prec = perf@y.values[[1]], rec = perf@x.values[[1]])  #Setting table into a dataframe
optimum<-df[which(df$prec>=0.65), ]
if(nrow(optimum)==0){   #In case we cant find a prob threshold for the required precision, we set the cutoff to Inf
  optimum<-rbind(optimum,list(Inf, 1, 0))
  colnames(optimum)<-c('cut', 'prec', 'rec')
}
optimum<-optimum[which.min(optimum$cut),] 
print(optimum)
plot(x = df$cut[-1],y=df$prec[-1])

imp<-data.frame(importance(model.rf))
imp[order(imp$IncNodePurity, decreasing = TRUE),]

testing$predictions<-predict( model.rf, testing)
printf("ratio of successful trades above cutoff for RF testing is %f and number is %d and %d out of total %d",nrow(testing[testing$predictions>optimum$cut & testing$profitable==TRUE,])/nrow(testing[testing$predictions>optimum$cut,]), nrow(testing[testing$predictions>optimum$cut & testing$profitable==TRUE,]), nrow(testing[testing$predictions>optimum$cut,]), nrow(testing))


# model.file<-paste(work.dir, "pair-rf", ".model", sep="")
# save(model.rf, file = model.file)

write.csv(testing, file=paste(work.dir, "testing.csv", sep=""), col.names = TRUE)


features<-c("profitable","entry_r1","entry_r2","pp1","pp2","adf1","adf2","pgff","pgff2","ersd","ersd2","jot","jot2","spr","spr2","half_life1","half_life2","nifty_adx","long_adx","short_adx","nifty_rsi","long_rsi", "short_rsi", "sector", "rlast_ratio", "perc_profitability", "avg_profit", "total_profit", "res_perc1", "res_perc2")
# features<-c("profitable","entry_r1","entry_r2","nifty_adx","long_adx","short_adx","nifty_rsi","long_rsi", "short_rsi", "sector", "rlast_ratio", "perc_profitability", "avg_profit", "total_profit")

logit1 <- glm(profitable ~.,family=binomial(link='logit'),data=training[, names(training) %in% features])
validation$predictions2<-family(logit1)$linkinv(predict( logit1, validation, response=TRUE))
roc_obj <- roc(validation$profitable, validation$predictions2)
auc(roc_obj)

pred <- prediction(validation$predictions2, validation$profitable)
perf <- performance(pred,"tpr","fpr")
# changing params for the ROC plot - width, etc
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
# plotting the ROC curve
plot(perf,col="black",lty=3, lwd=3)

perf <- performance(pred,"prec","rec")  #generating performance measure table using ROCR package
df <- data.frame(cut = perf@alpha.values[[1]], prec = perf@y.values[[1]], rec = perf@x.values[[1]])  #Setting table into a dataframe
optimum<-df[which(df$prec>=0.85), ]
if(nrow(optimum)==0){   #In case we cant find a prob threshold for the required precision, we set the cutoff to Inf
  optimum<-rbind(optimum,list(Inf, 1, 0))
  colnames(optimum)<-c('cut', 'prec', 'rec')
}
optimum<-optimum[which.min(optimum$cut),] 
print(optimum)
plot(x = df$cut[-1],y=df$prec[-1])

summary(logit1)

imp<-varImp(logit1, scale = FALSE)
imp

testing$predictions2<-predict( logit1, testing)
printf("ratio of successful trades above cutoff for RF testing is %f and number is %d and %d out of total %d",nrow(testing[testing$predictions2>optimum$cut & testing$profitable==TRUE,])/nrow(testing[testing$predictions2>optimum$cut,]), nrow(testing[testing$predictions2>optimum$cut & testing$profitable==TRUE,]), nrow(testing[testing$predictions2>optimum$cut,]), nrow(testing))


model.file<-paste(work.dir, "pair-logit", ".model", sep="")
# save(logit1, file = model.file)

write.csv(testing, file=paste(work.dir, "testing.csv", sep=""), col.names = TRUE)





#H20

features<-c("profitable","entry_r1","entry_r2","pp1","pp2","adf1","adf2","pgff","pgff2","ersd","ersd2","jot","jot2","spr","spr2","half_life1","half_life2","nifty_adx","long_adx","short_adx","nifty_rsi","long_rsi", "short_rsi", "sector", "rlast_ratio", "perc_profitability", "avg_profit", "total_profit", "profitability3m", "profitability6m", "profitability1Y", "profit_count3m", "profit_count6m", "profit_count1Y", "res_perc1", "res_perc2")

localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE,max_mem_size = "2G")

training_h2o <- as.h2o(training[, names(training) %in% features], destination_frame='training')
validation_h2o <- as.h2o(validation[, names(validation) %in% features], destination_frame='validation')
testing_h2o <- as.h2o(testing[, names(testing) %in% features], destination_frame='testing')

h2o.model <- 
  h2o.deeplearning(x = colnames(training_h2o)[! colnames(training_h2o) %in% c("profitable")],  # column numbers for predictors
                   y = c("profitable"),   # column number for label
                   training_frame = 'training', # data in H2O format
                   activation = "TanhWithDropout", # or 'Tanh'
                   input_dropout_ratio = 0.2, # % of inputs dropout
                   hidden_dropout_ratios = c(0.5,0.5), # % for nodes dropout
                   balance_classes = TRUE, 
                   hidden = c(100,100), # three layers of 50 nodes
                   epochs = 100, # max. no. of epochs
                   nfolds=5) 

validation$predictions3<-as.data.frame(h2o.predict(h2o.model, validation_h2o))[,3]
roc_obj <- roc(validation$profitable, validation$predictions3)
auc(roc_obj)

pred <- prediction(validation$predictions3, validation$profitable)
perf <- performance(pred,"tpr","fpr")
# changing params for the ROC plot - width, etc
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
# plotting the ROC curve
plot(perf,col="black",lty=3, lwd=3)

imp<-h2o.varimp(h2o.model)
print(imp)

perf <- performance(pred,"prec","rec")  #generating performance measure table using ROCR package
df <- data.frame(cut = perf@alpha.values[[1]], prec = perf@y.values[[1]], rec = perf@x.values[[1]])  #Setting table into a dataframe
optimum<-df[which(df$prec>=0.8), ]
if(nrow(optimum)==0){   #In case we cant find a prob threshold for the required precision, we set the cutoff to Inf
  optimum<-rbind(optimum,list(Inf, 1, 0))
  colnames(optimum)<-c('cut', 'prec', 'rec')
}
optimum<-optimum[which.min(optimum$cut),] 
print(optimum)

plot(x = df$cut[-1], y=df[-1, c("prec")], ylim=c(0,1))
points(x = df$cut[-1], y=df[-1,"rec"],type="p",col="red")


# summary(logit1)
# 
# imp<-varImp(logit1, scale = FALSE)
# imp

testing$predictions3<-as.data.frame(h2o.predict(h2o.model, testing_h2o))[,3]
printf("ratio of successful trades above cutoff for RF testing is %f and number is %d and %d out of total %d",nrow(testing[testing$predictions3>optimum$cut & testing$profitable==TRUE,])/nrow(testing[testing$predictions3>optimum$cut,]), nrow(testing[testing$predictions3>optimum$cut & testing$profitable==TRUE,]), nrow(testing[testing$predictions3>optimum$cut,]), nrow(testing))


#XGBOOST

features<-c("entry_r1","entry_r2","pp1","pp2","adf1","adf2","pgff","pgff2","ersd","ersd2","jot","jot2","spr","spr2","half_life1","half_life2","nifty_adx","long_adx","short_adx","nifty_rsi","long_rsi", "short_rsi", "sector", "rlast_ratio", "perc_profitability", "avg_profit", "total_profit", "profitability3m", "profitability6m", "profitability1Y", "profit_count3m", "profit_count6m", "profit_count1Y", "res_perc1", "res_perc2")
xgb <- xgboost(data = data.matrix(training[, names(training) %in% features]), 
               label = training$profitable
               # ,eta = 0.1,
               ,max_depth = 10, 
               nround=50,
               gamma=5
               # subsample = 0.5,
               # colsample_bytree = 0.5,
               # seed = 1,
               # eval_metric = "merror",
               # objective = "multi:softprob",
               # num_class = 12,
               # nthread = 3
)

validation$prediction_xgb<-predict(xgb, data.matrix(validation[, names(validation) %in% features]))
roc_obj <- roc(validation$profitable, validation$prediction_xgb)
auc(roc_obj)
RMSE(validation$profitable, validation$prediction_xgb)

pred <- prediction(validation$prediction_xgb, validation$profitable)
perf <- performance(pred,"tpr","fpr")
# changing params for the ROC plot - width, etc
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
# plotting the ROC curve
plot(perf,col="black",lty=3, lwd=3)

perf <- performance(pred,"prec","rec")  #generating performance measure table using ROCR package
df <- data.frame(cut = perf@alpha.values[[1]], prec = perf@y.values[[1]], rec = perf@x.values[[1]])  #Setting table into a dataframe
optimum<-df[which(df$prec>=0.70), ]
if(nrow(optimum)==0){   #In case we cant find a prob threshold for the required precision, we set the cutoff to Inf
  optimum<-rbind(optimum,list(Inf, 1, 0))
  colnames(optimum)<-c('cut', 'prec', 'rec')
}
optimum<-optimum[which.min(optimum$cut),] 
print(optimum)

plot(x = df$cut[-1], y=df[-1, c("prec")], ylim=c(0,1))
points(x = df$cut[-1], y=df[-1,"rec"],type="p",col="red")


testing$predictions_xgb<-predict(xgb, data.matrix(testing[, names(testing) %in% features]))
printf("ratio of successful trades above cutoff for RF testing is %f and number is %d and %d out of total %d",nrow(testing[testing$predictions_xgb>optimum$cut & testing$profitable==TRUE,])/nrow(testing[testing$predictions_xgb>optimum$cut,]), nrow(testing[testing$predictions_xgb>optimum$cut & testing$profitable==TRUE,]), nrow(testing[testing$predictions_xgb>optimum$cut,]), nrow(testing))
write.csv(testing, file=paste(work.dir, "testing.csv", sep=""), col.names = TRUE)

model.file<-paste(work.dir, "pair-xgb", ".model", sep="")
save(xgb, file = model.file)

#Modelling for holding period

trades$holding_period[trades$stop_loss==TRUE]<-1000
smp_size <- floor(0.80 * nrow(trades))
## set the seed to make your partition reproductible
set.seed(11)
train.ind <- sample(seq_len(nrow(trades)), size = smp_size)

training <- trades[train.ind, ]
testing <- trades[-train.ind, ]

# features<-c("holding_period","entry_r1","entry_r2","pp1","pp2","adf1","adf2","pgff","pgff2","ersd","ersd2","jot","jot2","spr","spr2","half_life1","half_life2","nifty_adx","nifty_rsi","long_rsi", "short_rsi","long_adx","short_adx","rsi_diff","long_rel_rsi","short_rel_rsi")
# features<-c("life","entry_r1","entry_r2","end_r1","end_r2","pp1","pp2","adf1","adf2","pgff","pgff2","ersd","ersd2","jot","jot2","spr","spr2","half_life1","half_life2","nifty_adx","nifty_rsi","long_rsi", "short_rsi","long_adx","short_adx","rsi_diff","long_rel_rsi","short_rel_rsi")
features<-c("holding_period","entry_r1","entry_r2","pp1","pp2","adf1","adf2","pgff","pgff2","ersd","ersd2","jot","jot2","spr","spr2","half_life1","half_life2","nifty_adx","nifty_rsi","long_rsi", "short_rsi","long_adx","short_adx","rsi_diff","long_rel_rsi","short_rel_rsi","pp1_mean","pp2_mean","adf1_mean","adf2_mean","pgff_mean","pgff2_mean","ersd_mean","ersd2_mean","jot_mean","jot2_mean","spr_mean","spr2_mean", "half_life1_mean","half_life2_mean")

# model.rf <- randomForest(life~., data=training[, names(training) %in% features], ntree=10, importance=T, na.action = na.omit)
model.rf <- randomForest(holding_period~., data=training[, names(training) %in% features], ntree=500, importance=T, na.action = na.omit)
testing$predictions<-predict( model.rf, testing)
roc_obj <- roc(testing$holding_period, testing$predictions)
auc(roc_obj)

pred <- prediction(testing$predictions, testing$holding_period)
perf <- performance(pred,"tpr","fpr")
# changing params for the ROC plot - width, etc
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
# plotting the ROC curve
plot(perf,col="black",lty=3, lwd=3)

perf <- performance(pred,"prec","rec")  #generating performance measure table using ROCR package
df <- data.frame(cut = perf@alpha.values[[1]], prec = perf@y.values[[1]], rec = perf@x.values[[1]])  #Setting table into a dataframe
optimum<-df[which(df$rec>=0.9), ]
if(nrow(optimum)==0){   #In case we cant find a prob threshold for the required precision, we set the cutoff to Inf
  optimum<-rbind(optimum,list(Inf, 1, 0))
  colnames(optimum)<-c('cut', 'prec', 'rec')
}
optimum<-optimum[1,] 
print(optimum)

imp<-data.frame(importance(model.rf))
imp[order(imp$IncNodePurity, decreasing = TRUE),]

write.csv(testing, file=paste(work.dir, "testing.csv", sep=""), col.names = TRUE)




