library(devtools)

require(data.table)
require(plyr)
require(psych)
require(zoo)

assignInNamespace("version_info", c(devtools:::version_info, list("3.5" = list(version_min = "3.3.0", version_max = "99.99.99", path = "bin"))), "devtools")
find_rtools()
devtools::install_github('r-dbi/RSQLite')
library(RSQLite)

filename <- "wearables_clinical_deID.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = filename)

## default to_remove function
rem <- c("5d1a706641a0b458da136a16b6c65d6b","c7c2d5fe4b4b981165a14f0684b31dae",
         "8ce925a402a0b3e31883d3c600ede9cd","807a78004456272761454351a6a759ff")

#Table Data Loader
getTable <- function(string){
  table<- dbGetQuery(db, paste('select * from ', string))
  return(table)
}

#########
#pulled up here for function construction. To be deleted
weareable_data = getTable("wearable_data")
demographics = getTable("demographics")
lab_results = getTable("lab_results")
vitals = getTable("vitals")
##########
#Table Cleaner ## optional
cleanTable<-function(table, remm, colName){
  table2<<-table[!(table[,colName] %in% remm),]
  return(table2)
}
######################

######################
#change time format
formatTime<-function(table, format_DIY, colName){
  table$Date <- as.POSIXlt(table[,colName],format= format_DIY)
  return (table)
}
############
#created a dummy_wearable for testing. to be deleted
dummy_wearable <- weareable_data[c(1:3000),]
dummy_wearable

dummy_wearable<-formatTime(dummy_wearable,"%Y-%m-%d %H:%M:%S", "Timestamp_Local")
########

#columnInfoGetter
getColumnInfo<- function(table, colName){
  return(data.frame(describe(table[colName])))
  
}
getColumnInfo(dummy_wearable, 'GSR')

##########
# get day data or get night data
getDayData <- function(table, dateCol){
  dayTable = table[table[,dateCol]$hour >= 8 & table[,dateCol]$hour <20,]
  return (dayTable)
}

getNightData <- function(table, dateCol){
  nightTable = table[!(table[,dateCol]$hour >= 8 & table[,dateCol]$hour <20),]
  return (nightTable)
}

getNightData(dummy_wearable,'Date')
##############

############
# a function that takes in a table and a list of column names and change them into numerical types

makeNum<- function(table, columns){
  table[,columns] <- suppressWarnings(sapply(table[,columns],as.numeric))
  return (table)
}

dummy_wearable = makeNum(dummy_wearable, c('Steps'))
sapply(dummy_wearable, class)
############


############
# a function that categorizes a feature to the corresponding quantile
# assumption here is that if there's at most one value that crosses multiple quantiles
getQuantile<- function(table, column, newColName, rangeStart, rangeEnd, rangeStep){

  quantiles <-quantile(dummy_wearable[,column], na.rm = TRUE, probs = seq(rangeStart, rangeEnd, by = rangeStep))
  
  checkPt <- which(table(quantiles) > 1)
  len = length(quantiles)
  for (i in (2:len)){
    if(quantiles[i]== quantiles[check])
    {
      quantiles[i] = quantiles[check]+runif(1, quantiles[i-1], quantiles[len]/(len*len))
    }
  }
  table[, newColName] <- findInterval(table[,column], quantiles)
  return (table)

}

getColumnInfo(getQuantile(dummy_wearable, 'Steps', 'StDecID',.1,1.0,.1),'StDecID')
dummy_wearable = makeNum(dummy_wearable, c('Skin_Temperature_F'))
getQuantile(dummy_wearable, 'Skin_Temperature_F', 'SkinTempID',.1,1.0,.1)


######################

######################
#create a new list which reflect the sum of the items in a specific window
window <- function(table, column, groupKey, winSize, func, newColName){
  table[,newColName] = ave(table[,column], table[,groupKey], FUN = function(x) rollapply(x, width =winSize, FUN= func, align ="right", partial = TRUE))
  return (table)
}
dummy_wearable <- window(dummy_wearable, 'Steps', 1, 10, sum, '10minSum')
######################

######################
#get resting information based on $keyColumn
restInfo<- function(table, column, keyCol, threshold =10, newName= 'restInfo')
{
  table[,keyCol][is.na(table[,keyCol])] <- 0
  table[,newName] = table[,column][which(table[keyCol] < threshold & !is.na(table[keyCol]))]
  return(table)
}
######################

######################
#based on the previous function, we now develop a get resting data function, which will append the resting data to the original table
getRestData <- function(table, column, newColumnName, winSize = 10, threshold, keyColumn){
  table<- window(table, keyColumn, 1, winSize, sum, "keyWindowValue")
  table<-restInfo(table, column, "keyWindowValue", threshold, newName = newColumnName)
  return (table)
}
getRestData(dummy_wearable, "Heart_Rate", "RestingHR", 10, 10, "Steps")
######################

######################
#get a subtable of high activity level according to self-defined standard
highActivityTable<- function(table, standardColumn){
  table = makeNum(table, c(standardColumn))
  table = getQuantile(table, standardColumn, "StandardColumnID",.1,1.0,.1)
  table <- table[table$StandardColumnID >= 9 & !is.na(table$StandardColumnID),]
  return (table)
}

highActivityTable(dummy_wearable, "Steps")
######################

######################
#get a subtable of high activity level according to self-defined standard
lowActivityTable<- function(table, standardColumn){
  table = makeNum(table, c(standardColumn))
  table = getQuantile(table, standardColumn, "StandardColumnID",.1,1.0,.1)
  table <- table[table$StandardColumnID <= 1 & !is.na(table$StandardColumnID),]
  return (table)
}

getNightData(lowActivityTable(dummy_wearable, "Steps"),'Date')
