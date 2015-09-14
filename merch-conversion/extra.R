set.seed(42)
trainingRows<-createDataPartition(merch$dropoutRate, p=0.75, list=FALSE)
head(trainingRows) # view the samples of row numbers
merch_df <-as.data.frame(merch)
merch_train<-merch_df[trainingRows,]
dim(merch_train)
merch_test<-merch_df[-trainingRows,]

# Choose any one of the cross validation approaches
#create multiple resampled splits
set.seed(42)
repeatedSplits <-createDataPartition(merch$dropoutRate, p=0.75, times=3)
str(repeatedSplits)
merch_train <-merch[repeatedSplits[[1]]]
dim(merch_train)


# Penalized Models
ridgeGrid <- expand.grid(lambda = seq(0, .1, length = 15))
set.seed(100)
ridgeTune <- train(x = trainingData, y = merch_train$dropoutRate,
                   method = "ridge",
                   tuneGrid = ridgeGrid,
                   trControl = ctrl)
,
preProc = c("center", "scale"))
ridgeTune
print(update(plot(ridgeTune), xlab = "Penalty"))
testResults$ridge <- predict(ridgeTune, testData)

# Elasticnet
enetGrid <- expand.grid(lambda = c(0, 0.01, .1), 
                        fraction = seq(.05, 1, length = 20))
set.seed(100)
enetTune <- train(x = trainingData, y = merch_train$dropoutRate,
                  method = "enet",
                  tuneGrid = enetGrid,
                  trControl = ctrl,
                  preProc = c("center", "scale"))
enetTune
plot(enetTune)
testResults$Enet <- predict(enetTune, testData)


#######################################################################
#Cust
# Choose any one of the cross validation approaches
set.seed(42)
trainingRows<-createDataPartition(cust$dropoutRate, p=0.75, list=FALSE)
head(trainingRows) # view the samples of row numbers
cust_df <-as.data.frame(cust)
cust_train<-cust_df[trainingRows,]
dim(cust_train)

#create multiple splits
set.seed(42)
repeatedSplits <-createDataPartition(cust$dropoutRate, p=0.75, times=3)
str(repeatedSplits)
cust_train <-cust[repeatedSplits[[1]]]
dim(cust_train)

