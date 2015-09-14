# Import Data
# Merchant Data
system.time(merch <- fread("./data/Merch_Acct_Level.csv"))
str(merch)
cols <- c( "rcvr_cntry_code", "rcvr_acct_type", "is_large_enterprise_y_n", "large_merch_y_n",
           "rcvr_tof", "rcvr_segment", "rcvr_industry", "rcvr_region", "cust_acq_chnl_desc",
           "merch_tpv_tier")
merch[,(cols):=lapply(.SD, as.factor),.SDcols=cols]
merch$starts<-as.double(merch$starts)
merch$dones<-as.double(merch$dones)
merch$coverage <-as.double(merch$coverage)
merch$tpv <- as.double(merch$tpv)
merch$num_txns <- as.double(merch$num_txns)
merch$rcvr_acct_cre_dt <- as.Date(merch$rcvr_acct_cre_dt, "%m/%d/%Y")
summary(merch)

merch[is.na(starts),][,.(rcvr_id,starts,dones)]
merch[is.na(dones),][,.(rcvr_id,starts,dones)]
merch<-merch[!is.na(starts)]
dim(merch)

#Create new variables
#merch <- merch[, donesOverStarts := as.double(dones/starts)]
merch<- merch[, dropoutRate := 1-as.double(dones/starts)]

# Visualize all variables
# Note slow performance
#featurePlot(x = as.data.frame(merch)[, 2:17],
#            y = merch$dropoutRate,
#            plot = "pairs")

# List all factor variables and its levels
merch_factors<-merch[,sapply(merch,function(x)is.factor(x))]
merch_factors
sapply(merch, levels)

# Select interesting factors and create dummyVars
# Choose one of the two options below
#listoffactors <- cols
listoffactors <- c( "rcvr_cntry_code", "rcvr_acct_type", "rcvr_tof", "rcvr_segment", "rcvr_industry", "rcvr_region", "cust_acq_chnl_desc","merch_tpv_tier")
transFormula <-as.formula(paste("dropoutRate ~ ",paste(listoffactors,collapse="+")))
dummies <-dummyVars(transFormula, data=merch)
dummyCols <- predict(dummies, newdata = merch)
merch[ , (listoffactors) := NULL]
merch <- cbind(merch, dummyCols)
dim(merch)

# Remove variables with near zero variance
nzv<-nearZeroVar(merch, freqCut = 95/5, uniqueCut = 10, saveMetrics = TRUE, 
                 foreach = FALSE, allowParallel = TRUE)
nzv[nzv$nzv,][1:10,]
nzv<-nearZeroVar(merch)
nzv
merch[ , (nzv) := NULL]
dim(merch)
str(merch)

#Remove irrelevant columns
Cols.chosen <- c("rcvr_segment.?", "rcvr_industry.?", "merch_tpv_tier.Unknown")
merch<-merch[,(Cols.chosen):=NULL]

# Identify columns and remove rows with NA
apply(merch, 2, sd)
for (i in seq_along(merch)) set(merch, i=which(is.na(merch[[i]])), j=i, value=0)
dim(merch)

# Remove highly correlated variables
corThresh <- 0.9
tokeep <- which(sapply(merch,is.numeric))
merchcor<-cor(merch[ , tokeep, with=FALSE], use="na.or.complete")
corrplot::corrplot(merchcor, order = "hclust",  tl.cex = .35)
highCorr <- findCorrelation(merchcor, corThresh)
highCorr
merch[ , (highCorr) := NULL]
dim(merch)

#Cleanup variable names
names(merch)
attr(merch, "names") <- gsub("[ ]", "", names(merch)) 
attr(merch, "names") <- gsub("[+]", ".pl", names(merch)) 
attr(merch, "names") <- gsub("[-]", "_", names(merch)) 
attr(merch, "names") <- gsub("[<]", "lt.", names(merch)) 
names(merch)

# Optional to write to a file
#write.csv(merch, file = "merch.csv")

#Explore using H2O
#merch.hex <- as.h2o(localH2O, merch)
# Now explore the data at http://localhost:54321
qplot(dropoutRate, data=merch)

# Continue analysis with caret
# Split the data into training and test sets
set.seed(42)
trainingRows<-createDataPartition(merch$dropoutRate, p=0.75, list=FALSE)
head(trainingRows) # view the samples of row numbers
merch_train <- merch[c(trainingRows),]
merch_test <- merch[-c(trainingRows),]
dim(merch_train)
dim(merch_test)

#For k-fold cross validation
set.seed(42)
cvSplits<-createFolds(merch$dropoutRate, k=7, returnTrain=TRUE)
str(cvSplits)
ctrl <- trainControl(method = "cv", index = cvSplits)
str(ctrl)

# Create a dataset with all potential 'x' values
Cols.chosen2<-c("num_txns","coverage",
"rcvr_cntry_code.GB","rcvr_cntry_code.US",
"rcvr_acct_type.Business","rcvr_acct_type.Consumer",
"rcvr_tof.1Y_3Y","rcvr_tof.3_12M",
"rcvr_tof.3Y.pl","rcvr_segment.03CS",
"rcvr_segment.04YS","rcvr_segment.05P2Preceiver",
"rcvr_industry.fashion","rcvr_region.CEMEA",
"rcvr_region.EMEA","rcvr_region.NA",
"cust_acq_chnl_desc.NaturalSearch","cust_acq_chnl_desc.Organic",
"cust_acq_chnl_desc.PaidIM","merch_tpv_tier.lt.10K")
trainingData <- merch_train[, Cols.chosen2, with = FALSE]
testData<-merch_test[, Cols.chosen2, with = FALSE]

# Ordinary Linear Regression  
lmFit1<- lm(merch_train$dropoutRate ~ ., data= trainingData)
summary(lmFit1) 
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lmFit1, las=1)
par(opar)
#visreg(lmFit1)
lm1Imp <- varImp(lmFit1, scale = FALSE)
lm1Imp

lmPred1<-predict(lmFit1, merch_test)
head(lmPred1)
lmValues1<- data.frame(obs = merch_test$dropoutRate, pred = lmPred1)
defaultSummary(lmValues1)

# Tune the model
set.seed(100)
lmTune0 <- train(x = trainingData, y = merch_train$dropoutRate,
                 method = "lm",
                 trControl = ctrl)
lmTune0   
testResults <- data.frame(obs = merch_test$dropoutRate,
                          Linear_Regression = predict(lmTune0, testData))
testResults


#Partial Least Squares
set.seed(100)
plsTune <- train(x = trainingData, y = merch_train$dropoutRate,
                 method = "pls",
                 tuneGrid = expand.grid(ncomp = 1:15),
                 trControl = ctrl)
plsTune
testResults$PLS <- predict(plsTune, testData)


set.seed(100)
pcrTune <- train(x = trainingData, y = merch_train$dropoutRate,
                 method = "pcr",
                 tuneGrid = expand.grid(ncomp = 1:15),
                 trControl = ctrl)
pcrTune                  
testResults$PCR <- predict(pcrTune, testData)

plsResamples <- plsTune$results
plsResamples$Model <- "PLS"
pcrResamples <- pcrTune$results
pcrResamples$Model <- "PCR"
plsPlotData <- rbind(plsResamples, pcrResamples)
plsPlotData

xyplot(RMSE ~ ncomp,
       data = plsPlotData,
       #aspect = 1,
       xlab = "# Components",
       ylab = "RMSE (Cross-Validation)",
       auto.key = list(columns = 2),
       groups = Model,
       type = c("o", "g"))

plsImp <- varImp(plsTune, scale = FALSE)

plot(plsImp, top = 10, scales = list(y = list(cex = .95)))

head(testResults)
qplot(obs, Linear_Regression, data = testResults)
qplot(obs, PLS, data = testResults)
qplot(obs, PCR, data = testResults)



