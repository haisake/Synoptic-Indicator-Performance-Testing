# Purpose: To run hypothesis tests on indicators on the synoptic indicator dashboards to test if the indicator is
# performing above or below target(expected) performance levels
# Negative deviance looks for poor performers
# Positive deviance looks for exceptional performers
# The pvalues are being used as a scale of confidence in evidence, which is a bit looser than their standard textbook usage 

# Author: Hans Aisake
# Date Created: Forked from TME analysis on 2020-02-29
# Date Modified: see github
# Comments
# We set this up to work out of the Bisrey Anaytics folder on the FHA M drive
# If the CDRprod moves, the user credentials change, or this report is moved this script will break
# Adjusting credentials and server information are done in the config file in the local DataSource folder
# R libraries and source codes are in R library and src folders.

############################# 
# Script intialization, parameter setting, and data loading
#############################

# you will need to change this if you move the R code or R libraries
# setwd("M:/Laboratory/LIS/Pathology/Synoptic Reporting/Bisrey Analytics Report/Synoptic-Indicator-Performance-Testing")

#source files; pulls in the user defined functions.
source("./src/src.R", local = TRUE)

# Set the type 1 error limits for the statistical testing for positive and negative deviance
# you can tweak the 0.10 or 0.05 values
parameters <-
  suppressWarnings(
    frame_data(
      ~positive_deviance, ~alpha,
      TRUE, 0.10,
      FALSE, 0.05 
    )
  )

mDrive <- FALSE



#set local library and load packages; will be moved out of this report file into a main execution file as required
setLibrary(mDrive)
loadPackages()

#load in the CDR prod connection details
db_config <- config::get("cdrprod",file ="./DataSources/CDRPROD.yml") #get dsn details from a config


############################# 
# Main computations
#############################

# load the data, flag outliers using the positive a positive deviance framing
# flag the number of peers and compute the percentage of flagged peers
positive_deviance <- TRUE
df1.data <- loadData(db_config) #load data
df2.data <- getPvalues(df1.data, positive_deviance, parameters) #add pvalues form testing
df3.data <- flagPeers(df2.data)         #add peers flagged context

# Repeat above with negative deviance framing
positive_deviance <- FALSE
df4.data <- getPvalues(df1.data, positive_deviance, parameters) #add pvalues form testing
df5.data <- flagPeers(df4.data)         #add peers flagged context

# combine positive and negative deviance results
df6.data <- union(df3.data, df5.data) %>% distinct() 

# store the data into a SQL table CDRProd.dbo.tblPvalues
saveData(db_config, df6.data)

#############################
#end script
#############################



 