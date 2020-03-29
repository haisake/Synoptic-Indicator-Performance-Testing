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
setLibrary <- function(mDrive){
  
  if (mDrive==TRUE){
    .libPaths("M:/Laboratory/LIS/Pathology/Synoptic Reporting/Bisrey Analytics Report/R Library")
  }else {
    libPath <-"H:/Hans/R Libraries"   #change this as needed for instal.
    .libPaths(libPath)                #set the library path 
  }
}

#load required packages
loadPackages <- function(){
  require("DescTools")
  require("DBI")
  require("dplyr")
  require("DT")
  require("tidyr")
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
  return("Save successful")

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

  #troubleshooting
  # df <- df2.data

  #count flagged peers for simple cases
  df1.peers.simple <- df %>% 
    filter(grepl("\\.",ConcatMaster)==FALSE) %>%
    group_by(Tissue, TimeFrame, Master_Vertex, Master_Type) %>% 
    summarize( flagged_peers = sum(reject), num_peers = n_distinct(Entity))  %>% 
    ungroup()
  
  #count flagged peers for complex cases with multi masters
  df1.peers.complex <- df %>% 
    filter(grepl("\\.",ConcatMaster)==TRUE) %>%
    select(Tissue, TimeFrame, ConcatMaster, Master_Type, Master_Vertex ) %>%
    distinct()
  
  df2.peers.complex <- df1.peers.complex %>%
    inner_join( df1.peers.simple, by=c("Master_Vertex"="Master_Vertex"
                                       , "Master_Type"="Master_Type"
                                       , "Tissue"="Tissue"
                                       , "TimeFrame"="TimeFrame"
                                       )) %>%
    group_by(Tissue, TimeFrame, ConcatMaster, Master_Type ) %>%
    summarize( flagged_peers2 = sum(flagged_peers), num_peers2 = sum(num_peers)) %>%
    ungroup() %>%
    select(Tissue, TimeFrame, ConcatMaster, Master_Type, flagged_peers2, num_peers2) %>%
    rename(flagged_peers = flagged_peers2, num_peers=num_peers2 )
  
  #join simple and complex cases together
  df3.peers.final <- union(df1.peers.simple %>% rename(ConcatMaster = Master_Vertex) , df2.peers.complex )


  #join data together and compute % of peers flagged
  #we drop the master vertex distinctions here hence why we need distinct
  df4.final.data <- df %>% 
    left_join(df3.peers.final, by =c("Tissue" ="Tissue"
                                     , "TimeFrame" = "TimeFrame"
                                     , "ConcatMaster"="ConcatMaster"
                                     , "Master_Type" ="Master_Type"
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
            ,num_peers ,percent_peers_flagged)  %>%
    distinct()

  return(df4.final.data )  
}


