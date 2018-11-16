library(devtools)
library(parallel)
library(fasttime)
require(data.table)
require(plyr)
require(psych)
require(zoo)

assignInNamespace("version_info", c(devtools:::version_info, list("3.5" = list(version_min = "3.3.0", version_max = "99.99.99", path = "bin"))), "devtools")
#find_rtools()
#devtools::install_github('r-dbi/RSQLite')
library(RSQLite)

filename <- "wearables_clinical_deID.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = filename)

## default to_remove function
rem <- c("5d1a706641a0b458da136a16b6c65d6b","c7c2d5fe4b4b981165a14f0684b31dae",
         "8ce925a402a0b3e31883d3c600ede9cd","807a78004456272761454351a6a759ff")

#############

#Table Data Loader
readDB <- function(tableName, columnString='*', where = '1 = 1'){
  table<- dbGetQuery(db, paste('select ',columnString, 'from ', tableName, ' where ', where))
  return(as.data.table(table))
}

as.data.dataframe(table)

readCSV <- function(fileName){
  table<- read.csv(fileName)
  return (as.data.table(table))
}

test <- readCSV('ACC.csv')
test

#########
#pulled up here for function construction. To be deleted
TB = readDB("wearable_data")
demographics = readDB("demographics")
lab_results = readDB("lab_results")
vitals = readDB("vitals")
TB = TB[1:100000,]
TB

##########
#Table Cleaner ## optional
cleanTable<-function(table, remm, colName){
  table2<<-table[! (eval(colName)) %in% remm]
  return(table2)
}
cleanTable(TB, '03de0e762b942438d6d7d8ba0ca0f929', quote(Wearable_Account_MD5))


######################


######################
#a function that automates parallel computing
parallelize<- function(data, func){
  no_cores <-  detectCores()-1
  cl <- makeCluster(no_cores)
  newData <- parLapply(cl, data, func)
  stopCluster(cl)
  return(newData)
}
######################


######################
#change time format
formatTime<-function(table, format_DIY, colName){
  table$Date <- fastPOSIXct(table[,eval(colName)])
  return (table)
}
TB <- formatTime(TB,"%Y-%m-%d %H:%M:%S", quote(Timestamp_Local))
TB
#user  system elapsed 
#4.93    2.72    7.75
############


TB[, GSR:=as.numeric(GSR)]
TB[, Steps := as.numeric(Steps)]
sapply(TB, class)

########
#columnInfoGetter
getColumnInfo<- function(table, colName){
  return(data.table(describe(table[,eval(colName)])))
  
}
getColumnInfo(TB, quote(GSR))
#user  system elapsed 
#15.47    3.03   18.68
##########


queryDayData <- function(table,columns, dateCol){
  getTable(table, columns, paste('time(',dateCol,') >= \'08:00:00\' AND ', 'time(',dateCol,') < \'20:00:00\''))
}

dayDemo <- queryDayData('wearable_data', 'Timestamp_Local','Timestamp_Local')

getDayData <- function(table, dateCol){
  dayTable <- table[hour(eval(dateCol)) >= 8 & hour(eval(dateCol)) < 20]
  return (dayTable)
}

getNightData<- function(table, dateCol){
  return(table[hour(eval(dateCol)) < 8 | hour(eval(dateCol)) >= 20])
}

getNightData(TB,quote(Date))

#user  system elapsed 
#9.84    1.33   11.33 
##############

############
# before using the below functions, make sure that the columns you use are of numeric type. You can do so by doing:



############


############
# a function that categorizes a feature to the corresponding quantile
# assumption here is that if there's at most one value that crosses multiple quantiles
getQuantile<- function(table, column,newColName, rangeStart, rangeEnd, rangeStep){
  vec <- table[,eval(column)]
  #print(vec)
  quantiles<- quantile(vec, na.rm = TRUE, probs = seq(rangeStart, rangeEnd, by = rangeStep))
  
  #quantiles <-quantile(vec, na.rm = TRUE, probs = seq(rangeStart, rangeEnd, by = rangeStep))
  
  checkPt <- which(table(quantiles) > 1)
  for (i in (2:len)){
    if(quantiles[i]== quantiles[checkPt])
    {
      quantiles[i] = quantiles[checkPt]+runif(1, quantiles[i-1], quantiles[len]/(len*len))
    }
  }
  
  
  #cbind(table, findInterval(vec, quantiles))
  return (findInterval(vec, quantiles))

}

TB[, StDecID:=getQuantile(TB, quote(Steps),'StDecID',.1,1.0,.1)]
#very quick
TB

#dummy_wearable = makeNum(dummy_wearable, c('Skin_Temperature_F'))
#getQuantile(dummy_wearable, 'Skin_Temperature_F', 'SkinTempID',.1,1.0,.1)


######################

######################
#create a new list which reflect the sum of the items in a specific window
window <- function(table, column, groupKey, winSize, func){
  vec <- table[,eval(column)]
  keyVec<- table[,eval(groupKey)]
  newVec = ave(vec, keyVec, FUN = function(x) rollapply(x, width =winSize, FUN= func, align ="right", partial = TRUE))
  return (newVec)
}
TB[,stepWindow:=window(TB, quote(Steps), quote(Wearable_Account_MD5), 10, sum)]
#user  system elapsed 
#759.59    5.35  786.88
TB
######################

######################
#get resting information based on $keyColumn
restInfo<- function(table, column, keyCol, threshold =10)
{
  subTable = table[(eval(keyCol))< threshold & !is.na(eval(keyCol))]
  return(subTable[,eval(column)])
}
restInfo(TB, quote(Heart_Rate), quote(stepWindow))
#user  system elapsed 
#5.08    3.62   10.19
######################

######################
#based on the previous function, we now develop a get resting data function, which will append the resting data to the original table
TB[,keyWindowValue:=window(TB, quote(Steps), quote(Wearable_Account_MD5), 10, sum)]
TB[,restInfo:=restInfo(TB, quote(Heart_Rate), quote(keyWindowValue),threshold=10)]
TB
######################

######################
#get a subtable of high activity level according to self-defined standard

highActivityTable<- function(table, StandardColumn, decile){
    subTable <- table[(eval(StandardColumn)) >= decile & !is.na(eval(StandardColumn))]
    return (subTable)
  }
highActivityTable(TB, quote(StDecID),5)
#user  system elapsed 
#2.31    1.33    3.93
######################

######################
#get a subtable of high activity level according to self-defined standard
#remember to numerize required columns
lowActivityTable<- function(table, StandardColumn,decile){
  subTable <- table[(eval(StandardColumn)) <= decile & !is.na(eval(StandardColumn))]
  return (subTable)
}
lowActivityTable(TB, quote(StDecID),3)


getNightData(lowActivityTable(TB,quote(StDecID),3),quote(Date))
