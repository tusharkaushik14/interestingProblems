# Import the data and create the customer-by-time-matrix
# Get ready to model

##Install packages
toInstallCandidates <- c("ggplot2", "reshape2", "plyr", "lubridate", "devtools", "gsl", "data.table", "vcd", "gplots", "ggdendro")

# check if pkgs are already present

toInstall <- toInstallCandidates[!toInstallCandidates%in%library()$results[,1]] 
if(length(toInstall)!=0)
{install.packages(toInstall, repos = "http://cran.r-project.org")}

# load pkgs
lapply(toInstallCandidates, library, character.only = TRUE)


# Cleanup existing installation of BTYD
if ("package:BTYD" %in% search()) { detach("package:BTYD", unload=TRUE) }
if ("BTYD" %in% rownames(installed.packages())) { remove.packages("BTYD") }

#Change local to 1 if using the local version of BTYD instead of CRAN version
local <-1

if (local){
  devtools::install_github("jamespaul007/BTYD", ref="pnbd-fix")
} else{
  install.packages("BTYD")
}
library(BTYD)

cust_merch <-0

if(cust_merch){
  elogFile <- "TxData.csv"
  elog <- dc.ReadLines(elogFile, cust.idx = 1,
                       date.idx = 2, sales.idx = 3)
  elog$date <- as.Date(elog$date, "%m/%d/%Y");
  elog$cust <- as.character(as.numeric(elog$cust)/10000000000000)
  
}else {
  elogFile <- "MTxData.csv"
  elog <- dc.ReadLines(elogFile, cust.idx = 1,
                       date.idx = 2, sales.idx = 3)
  elog$date <- as.Date(elog$date, "%m/%d/%Y");
}
head(elog)
summary(elog)

# Cohort identification
cohorts<- subset(elog, (date<"2013-07-31"))
cohort_cust<- unique(cohorts$cust)
#cohort_cust <- cohort_cust[1:1000]
str(cohort_cust)
elog<-elog[which(elog$cust %in% cohort_cust),]

if(cust_merch) {
  cust1elog<-elog[which(elog$cust %in% "160428"),]
} else {
  cust1elog<-elog[which(elog$cust %in% "2218002697173354568 "),]
}
qplot(date, sales, data = cust1elog)


# Optional - Sample the data
#class(elog)
#elog <- elog[sample(nrow(elog), 3000), ]
#str(elog)

qplot(date, sales, data = elog)
#dev.copy(png,'SalesByTime.png')
#dev.off()

#p<-ggplot(elog, aes(date, sales))
#p+geom_point(aes(color=factor(sales<=3*sd(sales))))

#Removing outliers
#elog$outlier<-factor(elog$sales<=3*sd(elog$sales))
#summary(elog)
#elog<-subset(elog, outlier=TRUE, select=cust:sales)
#xtabs( sales ~ cust, data =elog)

# Approach #1
# Merge transactions on the same day
#elog <- dc.MergeTransactionsOnSameDate(elog)

# Split into calibration and holdout period
#end.of.cal.period <- min(elog$date) + as.numeric((max(elog$date)-min(elog$date))/2)
#elog.cal <- elog[which(elog$date <= end.of.cal.period), ]

# Information about the customer
#split.data <- dc.SplitUpElogForRepeatTrans(elog.cal)
#clean.elog <- split.data$repeat.trans.elog

## Create customer-by-time matrix
#freq.cbt <- dc.CreateFreqCBT(clean.elog)
#tot.cbt <- dc.CreateFreqCBT(elog)
#cal.cbt <- dc.MergeCustomers(tot.cbt, freq.cbt)
#birth.periods <- split.data$cust.data$birth.per
#last.dates <- split.data$cust.data$last.date
#cal.cbs.dates <- data.frame(birth.periods, last.dates, end.of.cal.period)
#cal.cbs <- dc.BuildCBSFromCBTAndDates(cal.cbt, cal.cbs.dates, per="week")


# Approach #2
# Alternative Method - ElogToCbsCbt

end.of.cal.period <- min(elog$date) + as.numeric((max(elog$date)-min(elog$date))/2)
simData <- dc.ElogToCbsCbt(elog, per="week", end.of.cal.period, merge.same.date = TRUE, statistic = "freq")
cal.cbs <- simData$cal$cbs
cal.cbt <- simData$cal$cbt

# for Holdout period
hold.cbs <- simData$holdout$cbs
head(hold.cbs)
hold.cbt <- simData$holdout$cbt

