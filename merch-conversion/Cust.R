
#Customer Data
system.time(cust <- fread("./data/Cons_Acct_Level.csv"))
str(cust)

cols <- c("sndr_acct_type", "sndr_tof",  "has_bc_y_n","cust_age_grp_code","cust_occup_code",
          "cust_house_owned_status_code", "cust_marital_status_code", "cust_gendr_code",
          "cust_est_income_range_code", "cust_est_income_range_desc",
          "cust_marital_status_desc","cust_house_owned_status_desc", "cust_engagmnt_seg_key",
          "cust_engagmnt_sub_lvl_3", "cust_txn_activity_sub_seg") 
cust[,(cols):=lapply(.SD, as.factor),.SDcols=cols]
cust$starts<-as.double(cust$starts)
cust$dones<-as.double(cust$dones)
cust$actv_bank_accts_on_file_cnt<-as.double(cust$actv_bank_accts_on_file_cnt)
cust$actv_conf_bank_accts_cnt<-as.double(cust$actv_conf_bank_accts_cnt)
cust$actv_cc_on_file_cnt <-as.double(cust$actv_cc_on_file_cnt)
cust$actv_conf_cc_on_file_cnt<-as.double(cust$actv_conf_cc_on_file_cnt)
str(cust)
summary(cust)

#Create new variables
#cust <- cust[, donesOverStarts := as.double(dones/starts)]
cust<- cust[, dropoutRate := 1-as.double(dones/starts) ]


cust[is.na(starts),][,.(sndr_id,starts,dones)]
cust[is.na(dones),][,.(sndr_id,starts,dones)]
#cust<-cust[!is.na(starts)]
#dim(cust)

#Create new variables
cust <- cust[, donesOverStarts := as.double(dones/starts)]
cust<- cust[, dropoutRate := 1-donesOverStarts]

# List all factor variables and its levels
cust_factors<-cust[,sapply(cust,function(x)is.factor(x))]
cust_factors
sapply(cust, levels)

# Select interesting factors and create dummyVars
# Choose one of the two options below
listoffactors <- cols
#listoffactors <- c("sndr_acct_type", "sndr_tof",  "has_bc_y_n","cust_age_grp_code","cust_occup_code",
#                   "cust_house_owned_status_code", "cust_marital_status_code", "cust_gendr_code",
#                   "cust_est_income_range_code", "cust_est_income_range_desc",
#                   "cust_marital_status_desc","cust_house_owned_status_desc", "cust_engagmnt_seg_key",
#                   "cust_engagmnt_sub_lvl_3", "cust_txn_activity_sub_seg") 
transFormula <-as.formula(paste("dropoutRate ~ ",paste(listoffactors,collapse="+")))
dummies <-dummyVars(transFormula, data=cust)
dummyCols <- predict(dummies, newdata = cust)
cust[ , (listoffactors) := NULL]
cust <- cbind(cust, dummyCols)
dim(cust)

# Remove variables with near zero variance
nzv<-nearZeroVar(cust, freqCut = 95/5, uniqueCut = 10, saveMetrics = TRUE, 
                 foreach = FALSE, allowParallel = TRUE)
nzv[nzv$nzv,][1:10,]
nzv<-nearZeroVar(cust)
nzv
cust[ , (nzv) := NULL]
dim(cust)
str(cust)

#Remove irrelevant columns
Cols.chosen <- c("cust_marital_status_desc.UNKNOWN", "cust_house_owned_status_code.#", 
                 "cust_marital_status_code.#", "cust_gendr_code.#", "cust_est_income_range_code.#",
                 "cust_est_income_range_desc.UNKNOWN")
cust<-cust[,(Cols.chosen):=NULL]

# Identify columns and remove rows with NA
apply(cust, 2, sd)
for (i in seq_along(cust)) set(cust, i=which(is.na(cust[[i]])), j=i, value=0)
dim(cust)

# Remove highly correlated variables
tokeep <- which(sapply(cust,is.numeric))
custcor<-cor(cust[ , tokeep, with=FALSE], use="pairwise.complete.obs")
corrplot::corrplot(custcor, order = "hclust", tl.cex = .35)
highCorr <- findCorrelation(custcor, .75)
highCorr
cust[ , (highCorr) := NULL]
dim(cust)

#Cleanup names of variable names
names(cust)
attr(cust, "names") <- gsub("[ ]", "", names(cust)) 
attr(cust, "names") <- gsub("[,]", "", names(cust)) 
attr(cust, "names") <- gsub("[$]", "", names(cust)) 
attr(cust, "names") <- gsub("[+]", ".pl", names(cust)) 
attr(cust, "names") <- gsub("[-]", "_", names(cust)) 
attr(cust, "names") <- gsub("[<]", "lt.", names(cust)) 
names(cust)

# Optional to write to a file
#write.csv(cust, file = "cust.csv")

#Explore using H2O
#cust.hex <- as.h2o(localH2O, cust)
# Now explore the data at http://localhost:54321

qplot(dropoutRate, data=cust)

# Continue analysis with caret
# Split the data into training and test sets
set.seed(42)
trainingRows<-createDataPartition(cust$dropoutRate, p=0.75, list=FALSE)
head(trainingRows) # view the samples of row numbers
cust_train <- cust[c(trainingRows),]
cust_test <- cust[-c(trainingRows),]
dim(cust_train)
dim(cust_test)

#For k-fold cross validation
set.seed(42)
cvSplits<-createFolds(cust_train$dropoutRate, k=7, returnTrain=TRUE)
str(cvSplits)
ctrl <- trainControl(method = "cv", index = cvSplits)
str(ctrl)

Cols.chosen2 <- c("actv_bank_accts_on_file_cnt",
                 "actv_conf_bank_accts_cnt","actv_conf_cc_on_file_cnt",
                 "sndr_acct_type.Consumer","sndr_acct_type.Guest_Upgradeable",
                 "sndr_tof.1Y_3Y","cust_age_grp_code._99",
                 "cust_marital_status_code.M","cust_marital_status_code.S",
                 "cust_gendr_code.F","cust_gendr_code.M",
                 "cust_est_income_range_desc.50000_74999","cust_est_income_range_desc.Greaterthan124999",
                 "cust_marital_status_desc.MARRIED","cust_engagmnt_seg_key.8",
                 "cust_engagmnt_sub_lvl_3.Casual","cust_engagmnt_sub_lvl_3.GUEST",
                 "cust_engagmnt_sub_lvl_3.Habituated","cust_engagmnt_sub_lvl_3.LiteEngaged1",
                 "cust_engagmnt_sub_lvl_3.LiteEngaged2","cust_txn_activity_sub_seg.SingleTransactor",
                 "cust_txn_activity_sub_seg.SuperEngaged1")
trainingData <- cust_train[, Cols.chosen2, with = FALSE]
testData<-cust_test[, Cols.chosen2, with = FALSE]

# Ordinary Linear Regression  
lmFit1<- lm(cust_train$dropoutRate ~ ., data= trainingData)
summary(lmFit1) 
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lmFit1, las=1)
par(opar)
#visreg(lmFit1)
lm1Imp <- varImp(lmFit1, scale = FALSE)
lm1Imp

lmPred1<-predict(lmFit1,cust_test)
head(lmPred1)
lmValues1<- data.frame(obs =cust_test$dropoutRate, pred = lmPred1)
defaultSummary(lmValues1)

# Tune the model
set.seed(100)
lmTune0 <- train(x = trainingData, y =cust_train$dropoutRate,
                 method = "lm",
                 trControl = ctrl)
lmTune0   
testResults <- data.frame(obs =cust_test$dropoutRate,
                          Linear_Regression = predict(lmTune0, testData))
testResults


#Partial Least Squares
set.seed(100)
plsTune <- train(x = trainingData, y =cust_train$dropoutRate,
                 method = "pls",
                 tuneGrid = expand.grid(ncomp = 1:15),
                 trControl = ctrl)
plsTune
testResults$PLS <- predict(plsTune, testData)


set.seed(100)
pcrTune <- train(x = trainingData, y =cust_train$dropoutRate,
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
