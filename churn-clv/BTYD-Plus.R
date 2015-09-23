library(devtools)
library(gsl)
install_github("mplatzer/BTYDplus", dependencies=TRUE)
library(BTYDplus)
library(BTYD)
library(data.table)
library(caret)

# Load  event log
elogFile <- "TxData2.csv"
elog <-fread(elogFile)
setnames(elog, "CUSTOMER ID", "cust")
setnames(elog, "CAL_DT", "date")
setnames(elog, "TPV", "sales")
nullcols <- c("V1","sndr_acct_type", "sndr_acct_cre_dt")
elog[, (nullcols):=NULL]
elog$date <- as.Date(elog$date, "%m/%d/%Y")
elog$sales <- as.numeric(elog$sales)
elog$sales[is.na(elog$sales)] <- 0
elog$cust <-trimws(elog$cust)
str(elog)

# More variable transformation
#elog$sndr_acct_cre_dt <- as.Date(elog$sndr_acct_cre_dt, "%m/%d/%Y");
elog$BC_TPV <- as.numeric(elog$BC_TPV)
elog$BC_TPV[is.na(elog$BC_TPV)] <- 0

cols<-c("sndr_tof","has_bc_y_n","cust_age_grp_code","cust_occup_code",
        "cust_house_owned_status_code","cust_marital_status_code","cust_gendr_code",
        "cust_est_income_range_code")
elog[,(cols):=lapply(.SD, as.factor),.SDcols=cols]
str(elog)

#Dummy Columns
transFormula <-as.formula(paste("sales ~ ",paste(cols,collapse="+")))
dummies <-dummyVars(transFormula, data=elog)
dummyCols <- predict(dummies, newdata = elog)
elog[ , (cols) := NULL]
elog <- cbind(elog, dummyCols)
dim(elog)

#Remove irrelevant columns
Cols.chosen <- c("cust_occup_code.#","cust_house_owned_status_code.#", "cust_gendr_code.#",
                 "cust_est_income_range_code.#")
elog<-elog[,(Cols.chosen):=NULL]

# Transform to CBS 
#calDate<-"2014-07-31"
#cbs <- elog2cbs(elog, per="week", T.cal=calDate)

end.of.cal.period <- min(elog$date) + as.numeric((max(elog$date)-min(elog$date))/2)
simData <- dc.ElogToCbsCbt(elog, per="week", end.of.cal.period, merge.same.date = TRUE, statistic = "freq")
cal.cbs <- simData$cal$cbs
cal.cbt <- simData$cal$cbt

# for Holdout period
hold.cbs <- simData$holdout$cbs
head(hold.cbs)
hold.cbt <- simData$holdout$cbt


# Estimate with no covariates, i.e. model M1 in (Abe 2009)
set.seed(1)
draws_m1 <- abe.mcmc.DrawParameters(as.data.frame(cal.cbs), mcmc=5000, burnin=5000)
plot(draws_m1$level_2, density=FALSE)
params.pnbd_abe.m1 <- round(summary(draws_m1$level_2)$quantiles[, c("2.5%", "50%", "97.5%")], 2)
params.pnbd_abe.m1

# Append dollar amount of first purchase; used as covariate in (Abe 2009)
elog.dt <- elog
setkey(elog.dt, cust, date)
firstsale<-elog.dt[, .(first=sales[1]), by ="cust"]
cal.cbs <- cbind(cust = rownames(cal.cbs), cal.cbs)
cal.cbs <- merge(cal.cbs, firstsale, by= "cust")
head(cal.cbs)

set.seed(1)
draws_m2 <- abe.mcmc.DrawParameters(cal.cbs, covariates=c("first"), mcmc=5000, burnin=5000)
plot(draws_m2$level_2, density=FALSE)
params.pnbd_abe.m2 <- round(summary(draws_m2$level_2)$quantiles[, c("2.5%", "50%", "97.5%")], 4)
params.pnbd_abe.m2
