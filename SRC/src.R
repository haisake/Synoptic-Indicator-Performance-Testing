#USER DEFINED FUNCTIONS

#create a function to run the tests
test <- function(x, n, target, direction, alpha, positive_deviance){
  
  if (direction =="OVER" & positive_deviance ==TRUE){
    direction <- "greater"
  }else if (direction =="OVER" & positive_deviance ==FALSE){
    direction <- "less"
  }else if (direction =="UNDER" & positive_deviance ==FALSE){
    direction <- "greater"
  }else if (direction =="UNDER" & positive_deviance ==TRUE){
    direction <- "less"
  }else{
    direction <- "two.sided"
  }

  temp <- binom.test(x, n, p=target, alternative=direction, conf.level=(1-alpha))
  return(temp$p.value)
}

#set local library
setLibrary <- function(){
  libPath <-"H:/Hans/R Libraries"   #change this as needed for instal.
  .libPaths(libPath)                #set the library path 
}

#load required packages
loadPackages <- function(){
  require("DescTools")
  require("DBI")
  require("tidyverse")
  require("DT")
  require("rmarkdown")
}


#load data
loadData <- function(db_config){
  query <- "SELECT * FROM dbo.vwFeedDashIDPvalueTesting"
  
  # load data from the database; you will need a yml file with the data source details
  con <- DBI::dbConnect(odbc::odbc()
                        , Driver = db_config$driver
                        , Server = db_config$server
                        , Database =db_config$database
                        , UID = db_config$uid
                        , PWD = db_config$pwd) #establish connection
  data <- dbGetQuery(con, query)   #run the query and get result
  dbDisconnect(con) #close data base connection
  return(as.data.frame(data))
}

#save data
saveData <- function(db_config, df){
  
  # load data from the database; you will need a yml file with the data source details
  con <- DBI::dbConnect(odbc::odbc()
                        , Driver = db_config$driver
                        , Server = db_config$server
                        , Database =db_config$database
                        , UID = db_config$uid
                        , PWD = db_config$pwd) #establish connection
  
  dbGetQuery(con, "TRUNCATE TABLE CDRProd.dbo.tblPValues") 
  dbWriteTable(con, "tblPValues", df, append=TRUE)
  dbDisconnect(con) #close data base connection
  return(1)

}

#get pvalues for tests of all entities if positive deviance looks for strongest sucesses
getPvalues <- function(df, alpha, positive_deviance){
  
  df$p_val  <- mapply(test, df$Numerator, df$Denominator, df$Target, df$desiredDirection, alpha, positive_deviance) 
  df$reject <- (df$p_val < alpha)
  df$alpha  <- alpha
  
  if (positive_deviance){
    df$test_method <- "positive deviance"  
  } else {
    df$test_method <- "negative deviance"
  }

  return(df)  
} 

#identify flagged peers
flagPeers <- function(df){

  #count flagged peers
  df1.peers <- df %>% 
    group_by(Entity, EntityType, Tissue, IndicatorID, TimeFrame, Master_Type) %>% 
    summarize( flagged_peers = sum(reject), num_peers = n())  %>% 
    ungroup()

  #join data together and compute % of peers flagged
  # distinct() is not needed if the infrastructure works properly you can think of it as a failsafe
  df2.final.data <- df %>% 
    left_join(df1.peers, by =c("Entity"="Entity"
                              , "EntityType" ="EntityType"
                              , "Tissue" ="Tissue"
                              , "IndicatorID"="IndicatorID"
                              , "TimeFrame" = "TimeFrame"
                              , "Master_Type"="Master_Type"
                              )) %>%
    mutate( percent_peers_flagged = flagged_peers / num_peers
            , value = Numerator/Denominator) %>%
    select( Entity, EntityType
            ,Tissue ,TimeFrame
            ,TimeFrameType ,IndicatorID
            ,NameIndicator ,ConcatMaster
            ,Master_Type ,test_method
            ,Numerator, Denominator, value
            ,Target, desiredDirection
            ,p_val ,alpha
            ,reject ,flagged_peers
            ,num_peers ,percent_peers_flagged) %>%
    distinct()
  

  return(df2.final.data)  
}


