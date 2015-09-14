elog.dt <-data.table(elog)
last.date <- max(elog.dt$date)


## Now build the data
rfm.data <-elog.dt[, .(first.purchase = date[1],last.purchase = date[length(date)],
                txns = .N,amount = sum(sales)),keyby = cust]
head(rfm.data)

## These dates are relative to "now", i.e. last.date
rfm.data[, birth := last.date - first.purchase]
rfm.data[, death := last.date - last.purchase]

### Investigate the RFM data

### Recency, which we called "death" in the data

## The earliest death is the one furthest away from "now"
first.death <- with(rfm.data, max(death))

## Create a table of deaths; i.e. recency
deaths <- rfm.data[, .N, keyby = death]

# Clustering
Cols.chosen = c("first.purchase","last.purchase")
rfm.data[ ,(Cols.chosen) := NULL]
rfm.data$birth <- as.numeric(rfm.data$birth)
rfm.data$death <- as.numeric(rfm.data$death)
rfm.data$txns <- as.numeric(rfm.data$txns)

rfm.data <- na.omit(rfm.data) # listwise deletion of missing
rfm.data <- scale(rfm.data) # standardize variables

# Determine number of clusters
wss <- (nrow(rfm.data)-1)*sum(apply(rfm.data,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(rfm.data, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(rfm.data, 5) # 5 cluster solution
# get cluster means 
aggregate(rfm.data,by=list(fit$cluster),FUN=mean)
# append cluster assignment
rfm.data <- data.frame(rfm.data, fit$cluster)

# Ward Hierarchical Clustering
d <- dist(rfm.data, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")

rfm.data <- cbind(rfm.data, groups)
rfm.data[,.(.N,amt_var = sd(amount), txns_var = sd(txns)),by=groups]

clustinfo <- rfm.data[,.(cust,groups)]

elog<-join(elog, clustinfo, by = "cust")

elog<- subset(elog, groups=3, select=cust:sales)

# create the cbs-cbt matrices
T.cal <- as.Date("2013-12-31")
simData <- dc.ElogToCbsCbt(elog, per="week", T.cal)
cal.cbs <- simData$cal$cbs
cal.cbt <- simData$cal$cbt

# for Holdout period
hold.cbs <- simData$holdout$cbs
head(hold.cbs)
hold.cbt <- simData$holdout$cbt

