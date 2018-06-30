library(Quandl)
library(TTR)
library(h2o)
library(caret)

lookback<-6000
prices <- adjust.prices("NSE/CNX_NIFTY", Sys.Date() - lookback, Sys.Date(),corp.actions )
adx<-ADX(prices)
rsi<-RSI(prices$Close)
adx2<-ADX(prices, n=7)
rsi2<-RSI(prices$Close, n=7)
adx3<-ADX(prices, n=21)
rsi3<-RSI(prices$Close, n=21)

lookahead<-20
direction<-1
results<-data.frame()
for(i in 1:(length(index(adx))-lookahead)){
  date<-as.Date(index(adx)[i])
  if(is.na(adx3$ADX[i])){
    next
  }
  adx.value<-as.numeric(adx$ADX[i])
  rsi.value<-as.numeric(rsi[date,1])
  adx.direction<-ifelse(as.numeric(adx$DIp[i])>as.numeric(adx$DIn[i]),1,-1)
  adx.value2<-as.numeric(adx2$ADX[i])
  rsi.value2<-as.numeric(rsi2[date,1])
  adx.direction2<-ifelse(as.numeric(adx2$DIp[i])>as.numeric(adx2$DIn[i]),1,-1)
  adx.value3<-as.numeric(adx3$ADX[i])
  rsi.value3<-as.numeric(rsi3[date,1])
  adx.direction3<-ifelse(as.numeric(adx3$DIp[i])>as.numeric(adx3$DIn[i]),1,-1)
  
  lookahead.data<-prices$Close[paste(index(adx)[i+1],"/",index(adx)[i+lookahead],sep="")]
  nifty.start<-as.numeric(prices$Close[index(adx)[i]])
  nifty.high<-max(lookahead.data)
  nifty.low<-min(lookahead.data)
  change.high<-(nifty.high-nifty.start)/nifty.start
  change.low<-(nifty.low-nifty.start)/nifty.start
  high.when<-which.max(lookahead.data)
  low.when<-which.min(lookahead.data)
  row<-list(date, nifty.start, adx.value, adx.direction, rsi.value, adx.value2, adx.direction2, rsi.value2, adx.value3, adx.direction3, rsi.value3, nifty.high, nifty.low, change.high, change.low, high.when, low.when)
  results<-rbind.data.frame(results, row, stringsAsFactors = FALSE)
  
}
colnames(results)<-c('Date', 'nifty_start', 'adx', 'adx_dir', 'rsi', 'adx2', 'adx_dir2', 'rsi2', 'adx3', 'adx_dir3', 'rsi3', 'high', 'low', 'high_per', 'low_per', 'high_time', 'low_time')
results$Date<-as.Date(results$Date)
results$adx<-results$adx * results$adx_dir
results$adx2<-results$adx2 * results$adx_dir2
results$adx3<-results$adx3 * results$adx_dir3

high.threshold<-median(results$high_per)
low.threshold<- median(results$low_per)
results$label<-"long"  #0 means take short, 1 mean long
for(i in 1:nrow(results)){
  if(results$high_per[i]>high.threshold & results$low_per[i]>low.threshold){ 
    results$label[i]<-"long"
  }else if(results$high_per[i]<high.threshold & results$low_per[i]<low.threshold){
    results$label[i]<-"short"
  }else if(results$high_per[i]>high.threshold & results$low_per[i]<low.threshold){
    results$label[i]<-ifelse(results$high_time[i]<results$low_time[i], "long", "short")
  }else{
    results$label[i]<-"neutral"
  }
}
results$label<-as.factor(results$label)

train.index<-1:as.integer(0.8*nrow(results))
training <- results[ train.index,]
testing <- results[-train.index,]

# train.index <- createDataPartition(paste(results$label, sep="-"), p = .7, list =TRUE)
# training <- results[ train.index$Resample1,]
# testing <- results[-train.index$Resample1,]

# validation<-testing
validation.index <- createDataPartition(paste(testing$label, sep="-"), p = .5, list =TRUE)
validation <- testing[ validation.index$Resample1,]
testing <- testing[-validation.index$Resample1,]

features<-c("label","adx","rsi", "adx2","rsi2", "adx3","rsi3")

# model.rf <- randomForest(label~., data=training[, names(training) %in% features], ntree=500, importance=T, na.action = na.omit)
#h20
localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE,max_mem_size = "2G")

training_h2o <- as.h2o(training[, names(training) %in% features], destination_frame='training')
validation_h2o <- as.h2o(validation[, names(validation) %in% features], destination_frame='validation')
testing_h2o <- as.h2o(testing[, names(testing) %in% features], destination_frame='testing')

h2o.model <- 
  h2o.deeplearning(x = 1:6,  # column numbers for predictors
                   y = 7,   # column number for label
                   training_frame = 'training', # data in H2O format
                   activation = "TanhWithDropout", # or 'Tanh'
                   input_dropout_ratio = 0.2, # % of inputs dropout
                   hidden_dropout_ratios = c(0.5,0.5,0.5,0.5), # % for nodes dropout
                   balance_classes = TRUE, 
                   hidden = c(50,50,50,50), # three layers of 50 nodes
                   epochs = 100) # max. no. of epochs

# h2o.shutdown()

model<-h2o.deeplearning

validation$predictions<-predict( model, validation)
testing$predictions<-predict( model, testing)
rf.pred<-predict(model,testing,type="prob")

validation$predictions3<-as.data.frame(h2o.predict(h2o.model, validation_h2o))[,3]
#Multi class log loss function. Ranges from 0-inf
# multi.log.loss<-MultiLogLoss(rf.pred, as.character(testing$label))
#Accuracy
accuracy<-nrow(testing[as.character(testing$label)==as.character(testing$predictions),])/nrow(testing)
# Accuracy(testing$predictions, testing$label)
printf("Accuracy = %f", accuracy)

#generating classwise metrics and saving to file
metric.df<- data.frame(stringsAsFactors = FALSE)
for ( i in as.character(levels(testing[, 'label']) )) {
  subindex<-testing[as.character(testing$label)==i,]$index
  submatrix<-rf.pred[subindex,]
  # acc<-Accuracy(testing[testing$label==i,]$predictions, testing[testing$label==i,]$label)
  acc<-nrow(testing[as.character(testing$label)==as.character(testing$predictions) & as.character(testing$label)==i,])/nrow(testing[as.character(testing$label)==i,])
  # log.loss<-LogLoss(submatrix[,i], rep(1, nrow(testing[testing$label==i,]))) 
  prec<-nrow(testing[as.character(testing$predictions)==i & as.character(testing$predictions)==as.character(testing$label),])/nrow(testing[as.character(testing$predictions)==i,])
  print(i)
  roc_obj <- roc(ifelse(testing$label==i,1,0), rf.pred[,i])
  auc(roc_obj)
  metric.df<-rbind(metric.df, list(as.character(i), nrow(testing[as.character(testing$label)==i,]), acc, prec, auc(roc_obj)), stringsAsFactors = FALSE)
}
colnames(metric.df)<-c('label Code', 'Count', 'Accuracy', 'Precision', 'auc')
print(metric.df)
importance(model)

#Finding prob cutoff

pred.val<-predict(model,validation,type="prob")  #probability matrix on validation set
roc.df<- data.frame(stringsAsFactors = FALSE)
for ( i in as.character(levels(validation$label)) ) {
  print(i)
  #If customized thresold is found set it otherwise use default 
  threshold<-0.6
  
  predicted<-as.vector(pred.val[,i ])
  #labels are target in form of 0 1 (one vs all - for one class at a time)
  labels<-as.character(validation$label)
  labels[labels==i]<-1
  labels[labels!="1"]<-0
  # which(predicted>0)
  # 
  pred <- prediction( predicted, labels)  #prediction object using ROCR package
  perf <- performance(pred,"prec","rec")  #generating performance measure table using ROCR package
  df <- data.frame(cut = perf@alpha.values[[1]], prec = perf@y.values[[1]], rec = perf@x.values[[1]])  #Setting table into a dataframe
  optimum<-df[which(df$prec>=threshold), ]
  if(nrow(optimum)==0){   #In case we cant find a prob threshold for the required precision, we set the cutoff to Inf
    optimum<-rbind(optimum,list(Inf, 1, 0))
    colnames(optimum)<-c('cut', 'prec', 'rec')
  }
  optimum<-optimum[nrow(optimum),]   #Picking the performance measure satisfying our targetted precision, it will be a particular point on roc curve
  #generating other measures at our selected point on the roc curve
  recall<-optimum$rec
  cutoff<-optimum$cut
  precision<-optimum$prec
  actual.neg<-length(which(labels==0))
  actual.pos<-length(which(labels==1))
  # pred.positive<-length(which(predicted>=cutoff))
  # pred.negative<-length(which(predicted<cutoff))
  true.positive<-round(recall*actual.pos, digits=0)
  false.negative<-actual.pos-true.positive
  pred.positive<-round(true.positive/precision, digits=0)
  pred.negative<-actual.pos+actual.neg-pred.positive
  true.negative<-pred.negative-false.negative
  false.positive<-pred.positive-true.positive
  
  roc.df<-rbind(roc.df, list(as.character(i), nrow(validation[as.character(validation$label)==i,]), cutoff, recall, precision,  actual.pos, actual.neg, 
                             true.positive, true.negative, pred.positive, pred.negative, false.positive, false.negative), stringsAsFactors = FALSE)
}
colnames(roc.df)<-c('label', 'Count', "cutoff", "recall", "precision" , "actual.positive", "actual.negative",
                    "true.positive", "true.negative", "pred.positive", "pred.negative", "false.positive", "false.negative")
print(roc.df)

#Adjusting the prediction probabilities according to our precision cut-offs we calculated above
drop.classes<-c(roc.df[roc.df$cutoff==Inf, "label"], roc.df[!(roc.df$label %in% levels(training$label)), "label"])   #need to drop classes for which cut-off is inf 
printf("following classes dropped: ")
print(drop.classes)
rf.pred1<-rf.pred
rf.pred1<- rf.pred1[ , !(colnames(rf.pred1) %in% drop.classes)]
#Comparing if the columns in the rf.pred matrix are aligned with the labels code in rows for roc.df before performing a matrix-vector multiplication
roc.df<-roc.df[roc.df$cutoff<=1, ]
if(sum((colnames(rf.pred1)==roc.df[roc.df$cutoff!=0,"label"])==FALSE) >0 ) { #checking for mis-alignment
  stop("Vector roc.df label codes arent aligned with the matrix column names so cant perform matrix multiplication. Hence Exiting ")
}
roc.vector<-1/roc.df$cutoff
rf.pred1<-sweep(rf.pred1,MARGIN=2,roc.vector,`*`)   #Performing vector-matrix multipplication. Vector here is inverse of cutoffs
#To achieve minimum precision, we only provide prediction if at least one of the classes have a weighted score above 1, else it is categorized to unknown
testing$predicted<- apply(rf.pred1, 1, function(row){ if(sum(row>=1)>=1) colnames(rf.pred1)[which.max(row)] else "Unknown"})  

#Accuracy with modified procedure 
accuracy<-nrow(testing[as.character(testing$label)==testing$predicted,])/nrow(testing)
printf("Accuracy = %f", accuracy)

#generating classwise metrics and saving to file
metric.df<- data.frame(stringsAsFactors = FALSE)
for ( i in c(as.character(levels(testing$label)),"Unknown") ) {
  acc<-nrow(testing[as.character(testing$label)==as.character(testing$predicted) & as.character(testing$label)==i,])/nrow(testing[as.character(testing$label)==i,])
  print(i)
  prec<-nrow(testing[testing$predicted==i & testing$predicted==as.character(testing$label),])/nrow(testing[testing$predicted==i,])
  cutoff<-if(length(as.numeric(roc.df[roc.df$label==i, "cutoff"]))==0) 0 else as.numeric(roc.df[roc.df$label==i, "cutoff"])
  metric.df<-rbind(metric.df, list(as.character(i), nrow(testing[testing$label==i,]), nrow(testing[testing$predicted==i,]), cutoff, acc, prec), stringsAsFactors = FALSE)
}
precision<-nrow(testing[testing$label==testing$predicted & !(testing$predicted=="Unknown"),])/nrow(testing[!(testing$predicted=="Unknown"),])
colnames(metric.df)<-c('Label', 'Count', 'Count in predictions', 'Prob Threshold', 'Accuracy', 'Precision')
print(metric.df)
print(paste("\n\nPrecision = ", precision,"\n"))
print(paste("\n\nAccuracy = ", accuracy,"\n"))




pred <- prediction(validation$predictions, validation$profitable)
perf <- performance(pred,"tpr","fpr")
# changing params for the ROC plot - width, etc
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
# plotting the ROC curve
plot(perf,col="black",lty=3, lwd=3)

perf <- performance(pred,"prec","rec")  #generating performance measure table using ROCR package
df <- data.frame(cut = perf@alpha.values[[1]], prec = perf@y.values[[1]], rec = perf@x.values[[1]])  #Setting table into a dataframe
optimum<-df[which(df$prec>=0.7), ]
if(nrow(optimum)==0){   #In case we cant find a prob threshold for the required precision, we set the cutoff to Inf
  optimum<-rbind(optimum,list(Inf, 1, 0))
  colnames(optimum)<-c('cut', 'prec', 'rec')
}
optimum<-optimum[which.min(optimum$cut),] 
print(optimum)
plot(x = df$cut[-1],y=df$prec[-1])

imp<-data.frame(importance(model))
imp[order(imp$IncNodePurity, decreasing = TRUE),]

testing$predictions<-predict( model, testing)
printf("ratio of successful trades above cutoff for RF testing is %f and number is %d and %d out of total %d",nrow(testing[testing$predictions>optimum$cut & testing$profitable==TRUE,])/nrow(testing[testing$predictions>optimum$cut,]), nrow(testing[testing$predictions>optimum$cut & testing$profitable==TRUE,]), nrow(testing[testing$predictions>optimum$cut,]), nrow(testing))


