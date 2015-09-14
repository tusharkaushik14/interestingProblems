# Import Data
# Transaction Data
system.time(txn <- fread("./data/Txn_Level.csv"))
str(txn)

# Variable transformations
txn$Date <- as.Date(paste(txn$txn_mth, "01", sep = "/"), format = ("%Y/%m/%d"))
head(txn$Date)
setkey(txn, Date)
txn[,txn_mth :=NULL]
cols <- c("mkt_place_attempt_y_n", "browser_code", "chkout_integrtn_type", "mobile_os", 
                   "mobile_devc_type", "PRODUCT", "rcvr_cntry_code", "IB_XB_flag", "Member_Guest",
                   "Web_Mobile", "Product_Exp", "prod_exp_wps", "Fallout_rsn_desc", "Fallout_Desc", 
                   "Review_Desc","last_lndg_pg_dervd_flow_type") 
txn[,(cols):=lapply(.SD, as.factor),.SDcols=cols]
txn$STARTS<-as.double(txn$STARTS)
txn$DONES<-as.double(txn$DONES)
txn[is.na(STARTS),]
txn[is.na(DONES),]
txn<-txn[!is.na(STARTS)]
dim(txn)
summary(txn)


#Create new variables
txn <- txn[, donesOverStarts := as.double(DONES/STARTS)]
txn<- txn[, dropoutRate := 1-donesOverStarts]

# Visualize all variables
# Note slow performance
featurePlot(x = as.data.frame(txn)[, 3:18],
            y = txn$donesOverStarts,
            plot = "pairs")


# List all factor variables and its levels
txn_factors<-txn[,sapply(txn,function(x)is.factor(x))]
txn_factors
sapply(txn, levels)

# Select interesting factors and create dummyVars
# Choose one of the two options below
listoffactors <- cols
#listoffactors <- c("mkt_place_attempt_y_n", "chkout_integrtn_type", "Product_Exp", "IB_XB_flag", "Member_Guest", "Fallout_Desc", "Review_Desc", "last_lndg_pg_dervd_flow_type")
transFormula <-as.formula(paste("dropoutRate ~ ",paste(listoffactors,collapse="+")))
dummies <-dummyVars(transFormula, data=txn)
dummyCols <- predict(dummies, newdata = txn)
txn[ , (listoffactors) := NULL]
txn <- cbind(txn, dummyCols)
dim(txn)

# Remove variables with near zero variance
nzv<-nearZeroVar(txn, freqCut = 95/5, uniqueCut = 10, saveMetrics = TRUE, 
            foreach = FALSE, allowParallel = TRUE)
nzv[nzv$nzv,][1:10,]
nzv<-nearZeroVar(txn)
nzv
txn[ , (nzv) := NULL]
dim(txn)
str(txn)

#Remove un-necessary variables
Cols.chosen <- c("browser_code.-99", "chkout_integrtn_type.#","mobile_os.#","mobile_devc_type.#",
                 "Member_Guest.Unknown/Other")
txn<-txn[,(Cols.chosen):=NULL]
dim(txn)

# Remove highly correlated variables
tokeep <- which(sapply(txn,is.numeric))
txncor<-cor(txn[ , tokeep, with=FALSE], use="pairwise.complete.obs")
corrplot::corrplot(txncor, order = "hclust", tl.cex = .35)
highCorr <- findCorrelation(txncor, .75)
highCorr
txn[ , (highCorr) := NULL]
dim(txn)

#Cleanup variable names
names(txn)
attr(txn, "names") <- gsub("[ ]", "", names(txn)) 
attr(txn, "names") <- gsub("[+]", ".pl", names(txn)) 
attr(txn, "names") <- gsub("[=]", "", names(txn)) 
attr(txn, "names") <- gsub("[-]", "_", names(txn)) 
attr(txn, "names") <- gsub("[<]", "lt.", names(txn)) 
names(txn)


# Optional to write to a file
#write.csv(txn, file = "txn.csv")

#Explore using H2O
#txn.hex <- as.h2o(localH2O, txn)
# Now explore the data at http://localhost:54321

# Continue analysis in caret
# Split the data into training and test sets
set.seed(42)
trainingRows<-createDataPartition(txn$dropoutRate, p=0.75, list=FALSE)
head(trainingRows) # view the samples of row numbers
txn_train <- txn[c(trainingRows),]
txn_test <- txn[-c(trainingRows),]
dim(txn_train)
dim(txn_test)

#For k-fold cross validation
set.seed(42)
cvSplits<-createFolds(txn$dropoutRate, k=7, returnTrain=TRUE)
str(cvSplits)
ctrl <- trainControl(method = "cv", index = cvSplits)
str(ctrl)

# Create a dataset with all potential 'x' values
Cols.chosen2<-c("mkt_place_attempt_y_n.Y","chkout_integrtn_type.M","chkout_integrtn_type.S",
                "mobile_os.Android","mobile_os.iOS","PRODUCT.EmailPayments","PRODUCT.ExpressCheckout",
                "PRODUCT.WebsitePaymentStandard","rcvr_cntry_code.C2","rcvr_cntry_code.DE",
                "rcvr_cntry_code.US","Member_Guest.Guest","Web_Mobile.EmailPayments",
                "Web_Mobile.MobileCheckout","Web_Mobile.WebCheckout","Product_Exp.MEC2",
                "Product_Exp.MobileEC","Product_Exp.mWPS","Product_Exp.UnifiedExperience",
                "prod_exp_wps.3rdparty","prod_exp_wps.Transaction","Fallout_rsn_desc.0.0.0.Success",
                "Fallout_rsn_desc.1.8.5.PageTypeLogin:PageSpeedNotSlow","Review_Desc.AfterReview",
                "last_lndg_pg_dervd_flow_type.Guest_Billing","last_lndg_pg_dervd_flow_type.Member_Login")
trainingData <- txn_train[, Cols.chosen2, with = FALSE]
testData<-txn_test[, Cols.chosen2, with = FALSE]

# Ordinary Linear Regression  
lmFit1<- lm(txn_train$dropoutRate ~ ., data= trainingData)
summary(lmFit1) 
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lmFit1, las=1)
par(opar)
#visreg(lmFit1)
lm1Imp <- varImp(lmFit1, scale = FALSE)
lm1Imp

lmPred1<-predict(lmFit1, txn_test)
head(lmPred1)
lmValues1<- data.frame(obs = txn_test$dropoutRate, pred = lmPred1)
defaultSummary(lmValues1)

# Tune the model
set.seed(100)
lmTune0 <- train(x = trainingData, y = txn_train$dropoutRate,
                 method = "lm",
                 trControl = ctrl)
lmTune0   
testResults <- data.frame(obs = txn_test$dropoutRate,
                          Linear_Regression = predict(lmTune0, testData))
testResults


#Partial Least Squares
set.seed(100)
plsTune <- train(x = trainingData, y = txn_train$dropoutRate,
                 method = "pls",
                 tuneGrid = expand.grid(ncomp = 1:15),
                 trControl = ctrl)
plsTune
testResults$PLS <- predict(plsTune, testData)


set.seed(100)
pcrTune <- train(x = trainingData, y = txn_train$dropoutRate,
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









