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
######

#change time format
formatTime<-function(table, format_DIY, colName){
  table$Date <- as.POSIXlt(table[,colName],format= format_DIY)
  return (table)
}
############

#created a dummy_wearable for testing. to be deleted
dummy_wearable <- weareable_data[c(1:100),]
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


dummy_wearable <- weareable_data[c(1:3000),]
dummy_wearable

makeInt(table, )
stepQuantiles <-quantile(dummy_wearable$Steps, na.rm = TRUE, probs = seq(0.1, 0.9, by = 0.1))





