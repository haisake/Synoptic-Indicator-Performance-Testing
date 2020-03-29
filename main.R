# Purpose: To run hypothesis tests on indicators on the synoptic indicator dashboards
# Author: Hans Aisake
# Date Created: Forked from TME analysis on 2020-02-29
# Date Modified: see github
# Comments
# You have to open this script/project from whereever it is saved localy for this to work properly.

#setwd
setwd("M:/Laboratory/LIS/Pathology/Synoptic Reporting/Bisrey Analytics Report/Synoptic-Indicator-Performance-Testing")

#source files
source("./src/src.R", local = TRUE)

#parameters; change to 0.1 for positive_deviance true
alpha <- 0.05
positive_deviance <- FALSE
mDrive <- TRUE

#set local library and load packages; will be moved out of this report file into a main execution file as required
setLibrary(mDrive)
loadPackages()

#set parameters
db_config <- config::get("cdrprod",file ="./DataSources/CDRPROD.yml") #get dsn details from a config

# load the data, flag outliers using the positive a positive deviance framing
# flag the number of peers and compute the percentage of flagged peers
positive_deviance <- TRUE
df1.data <- loadData(db_config) #load data
df2.data <- getPvalues(df1.data, alpha, positive_deviance) #add pvalues form testing
df3.data <- flagPeers(df2.data)         #add peers flagged context

# Repeat above with negative deviance framing
positive_deviance <- FALSE
df4.data <- getPvalues(df1.data, alpha, positive_deviance) #add pvalues form testing
df5.data <- flagPeers(df4.data)         #add peers flagged context

# combin positive and negative deviance results
df6.data <- union(df3.data, df5.data) %>% distinct() 

# store the data back into a SQL table CDRProd.dbo.tblPvalues
saveData(db_config, df6.data)

#end script




 