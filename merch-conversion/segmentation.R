#Set up the parameters:
fc_cont <- new("flexclustControl") ## holds "hyperparameters"
fc_cont@tolerance <- 0.1
fc_cont@iter.max <- 30
fc_cont@verbose <- 1 ## verbose > 0 will show iterations
fc_family <- "ejaccard"

#Prepare data
Cols.chosen<-c("txn_mth", "STARTS", "DONES","donesOverStarts" )
txn.mat <-as.matrix(txn[, (Cols.chosen) := NULL])
dim(txn.mat)
str(txn.mat)

#Invoke kcca(): "k-centroid cluster analysis"
fc_seed <- 577 
num_clusters <- 3 ## Simple example - only three clusters
set.seed(fc_seed)
txn.cl <- kcca(txn.mat, k = num_clusters, save.data = TRUE,
               control = fc_cont, family = kccaFamily(fc_family))

#Extract principal components
txn.pca <- prcomp(txn.mat)
plot(txn.cl, data = txn.mat, project = txn.pca, main = ...)
#Segment profile plot
barchart(txn.cl, strip.prefix = "#", shade = TRUE, layout = c(txn.cl@k, 1), main = ...)


