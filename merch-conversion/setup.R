setwd("~/Merch_Conversion")
getwd()
version

##Install packages
toInstallCandidates <- c("ggplot2", "reshape2", "caret", "plyr", "corrplot", "visreg", "MASS",
                         "lubridate", "devtools", "gsl", "data.table", "vcd", "gplots", "ggdendro")

# check if pkgs are already present
toInstall <- toInstallCandidates[!toInstallCandidates%in%library()$results[,1]] 
if(length(toInstall)!=0)
{install.packages(toInstall, repos = "http://cran.r-project.org")}

# load pkgs
lapply(toInstallCandidates, library, character.only = TRUE)

# The following two commands remove any previously installed H2O packages for R.
h2o.shutdown(conn = h2o.getConnection(), prompt = TRUE)
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
if (! ("methods" %in% rownames(installed.packages()))) { install.packages("methods") }
if (! ("statmod" %in% rownames(installed.packages()))) { install.packages("statmod") }
if (! ("stats" %in% rownames(installed.packages()))) { install.packages("stats") }
if (! ("graphics" %in% rownames(installed.packages()))) { install.packages("graphics") }
if (! ("RCurl" %in% rownames(installed.packages()))) { install.packages("RCurl") }
if (! ("rjson" %in% rownames(installed.packages()))) { install.packages("rjson") }
if (! ("tools" %in% rownames(installed.packages()))) { install.packages("tools") }
if (! ("utils" %in% rownames(installed.packages()))) { install.packages("utils") }

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-simons/7/R")))
library(h2o)
localH2O = h2o.init( nthreads = -1)

