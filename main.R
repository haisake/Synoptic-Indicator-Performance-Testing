# Purpose: To run hypothesis tests on indicators on the synoptic indicator dashboards
# Author: Hans Aisake
# Date Created: Forked from TME analysis on 2020-02-29
# Date Modified: see github
# Comments
# You have to open this script/project from whereever it is saved localy for this to work properly.

#source files
source("./src/src.R", local = TRUE)

#set local library and load packages; will be moved out of this report file into a main execution file as required
setLibrary()
loadPackages()

#set parameters
alpha <- 0.05
positive_deviance <- FALSE
db_config <- config::get("cdrprod",file ="./DataSources/CDRPROD.yml") #get dsn details from a config

# load the data, flag outliers using the positive or negative deviance frame
# flag the number of peers and number of flagged peers
df1.data <- loadData(db_config) #load data
df2.data <- getPvalues(df1.data, alpha, positive_deviance) #add pvalues form testing
df3.data <- flagPeers(df2.data)         #add peers flagged context

# store the data back into a SQL table CDRProd.dbo.tblPvalues
saveData(db_config, df3.data)

#end script


 