##########EC2##########
########1########
install.packages("RSQLite")
library(RSQLite)
setwd("/home/ubuntu/mnt/airline")

drv <- dbDriver( "SQLite" )
#connect to database
db <- dbConnect( drv, dbname = "airline" )

#set up database
#read in data from first year
dat <- read.csv("1987.csv.bz2")
#substitute all NA's for 9999
dat[is.na(dat)] <- 9999
#write to database
dbWriteTable(conn=db, name="airline", value=dat, row.names=FALSE)
rm(dat)

#do the same for the other years, append to the same table
years <- seq( 1988, 2008 )
for (i in years){
  val <- paste0( i, ".csv.bz2" )
  dat <- read.csv( file=val )
  dat[is.na(dat)] <- 9999
  dbWriteTable(conn = db, name = "airline", value = dat, row.names = FALSE, append= TRUE)
  rm(dat)
}

#check database size
size <- file.size("airline")
utils:::format.object_size(size, "Gb")
#[1] "10.3 Gb"


########2(a)########
cleandb <- dbConnect( drv, dbname = "cleanairline" )
#only include columns we will need later in the problem
#create new column for hour of day of the scheduled departure time
data <- dbGetQuery(db, "Select Year, Month, DayOfMonth, DayOfWeek, DepTime, CRSDepTime, 
                   ArrTime, CRSArrTime, UniqueCarrier, FlightNum, ArrDelay, DepDelay, 
                   Origin, Dest, Distance, (CRSDepTime-(CRSDepTime%100))/100 as CRSDepHour 
                   FROM airline 
                   WHERE DepDelay < 9999 and ActualElapsedTime > 0 and CRSElapsedTime > 0")
dbWriteTable(conn = cleandb, name = "cleanairline", value = data, row.names = FALSE)
rm(data)


########2(b)########
queryFun <- function( conn, query ) {
  df <- dbGetQuery( conn, query )
  return(df)
}

#query creates new columns DepDelay30, DepDelay60, DepDelay180 for flights more than 30, 
#60, 180 minutes late and new column total for total number of flights; compute proportions 
#of flights more than 30, 60, 180 minutes late; group data by the specified categories
query <- "SELECT COUNT(*) as total, 
SUM(CASE WHEN DepDelay>30 THEN 1 ELSE 0 END) as DepDelay30, 
SUM(CASE WHEN DepDelay>60 THEN 1 ELSE 0 END) as DepDelay60, 
SUM(CASE WHEN DepDelay>180 THEN 1 ELSE 0 END) as DepDelay180,
CAST(SUM(CASE WHEN DepDelay>30 THEN 1 ELSE 0 END) AS FLOAT)/CAST(COUNT(*) AS FLOAT) AS prop30,
CAST(SUM(CASE WHEN DepDelay>60 THEN 1 ELSE 0 END) AS FLOAT)/CAST(COUNT(*) AS FLOAT) AS prop60,
CAST(SUM(CASE WHEN DepDelay>180 THEN 1 ELSE 0 END) AS FLOAT)/CAST(COUNT(*) AS FLOAT) AS prop180,
UniqueCarrier,Origin,Dest,Month,DayOfWeek,CRSDepHour 
FROM cleanairline
GROUP BY UniqueCarrier,Origin,Dest,Month,DayOfWeek,CRSDepHour"

system.time( queryFun( cleandb, query ) )
# user  system elapsed 
# 614.428  33.716 659.848


########2(d)########
#create index
dbGetQuery(cleandb, "CREATE INDEX ind ON cleanairline(UniqueCarrier,Origin,Dest,Month,DayOfWeek,
           CRSDepHour)")

system.time( queryFun(cleandb,query) )
# user  system elapsed 
# 225.620  27.560 253.183


########2(e)########
df <- queryFun(cleandb,query)
#subset keys with at least 150 flights
data <- subset(df,total>=150)
head( data[order(-data$prop30),],10 )
#         total DepDelay30 DepDelay60 DepDelay180    prop30    prop60     prop180
# 6517747   160         66         28           0 0.4125000 0.1750000 0.000000000
# 6582556   151         61         18           1 0.4039735 0.1192053 0.006622517
# 6517520   150         57         31           3 0.3800000 0.2066667 0.020000000
# 6517748   152         57         22           0 0.3750000 0.1447368 0.000000000
# 6583038   163         60         25           0 0.3680982 0.1533742 0.000000000
# 6517407   158         58         19           1 0.3670886 0.1202532 0.006329114
# 5192821   162         59         36           1 0.3641975 0.2222222 0.006172840
# 6583522   172         62         25           6 0.3604651 0.1453488 0.034883721
# 6518208   165         58         20           3 0.3515152 0.1212121 0.018181818
# 6517745   177         62         28           1 0.3502825 0.1581921 0.005649718
#         UniqueCarrier Origin Dest Month DayOfWeek CRSDepHour
# 6517747            WN    DAL  HOU     6         5         20
# 6582556            WN    HOU  DAL     2         5         19
# 6517520            WN    DAL  HOU     4         5         20
# 6517748            WN    DAL  HOU     6         5         21
# 6583038            WN    HOU  DAL     6         5         19
# 6517407            WN    DAL  HOU     3         5         20
# 5192821            UA    LAX  SFO    12         5         11
# 6583522            WN    HOU  DAL    10         5         19
# 6518208            WN    DAL  HOU    10         5         20
# 6517745            WN    DAL  HOU     6         5         18


########3########
require(parallel)
require(doParallel)

#set number of cores
registerDoParallel(4)

taskFun <- function(i) {
  #open separate database connections for the separate tasks for them to operate in parallel
  newdb <- dbConnect( SQLite(), dbname = "cleanairline" )
  query <- paste0("SELECT COUNT(*) as total, 
                  SUM(CASE WHEN DepDelay>30 THEN 1 ELSE 0 END) as DepDelay30, 
                  SUM(CASE WHEN DepDelay>60 THEN 1 ELSE 0 END) as DepDelay60, 
                  SUM(CASE WHEN DepDelay>180 THEN 1 ELSE 0 END) as DepDelay180, 
                  CAST(SUM(CASE WHEN DepDelay>30 THEN 1 ELSE 0 END) AS FLOAT)/CAST(COUNT(*) AS FLOAT) AS prop30, 
                  CAST(SUM(CASE WHEN DepDelay>60 THEN 1 ELSE 0 END) AS FLOAT)/CAST(COUNT(*) AS FLOAT) AS prop60, 
                  CAST(SUM(CASE WHEN DepDelay>180 THEN 1 ELSE 0 END) AS FLOAT)/CAST(COUNT(*) AS FLOAT) AS prop180, 
                  UniqueCarrier,Origin,Dest,Month,DayOfWeek,CRSDepHour 
                  FROM cleanairline WHERE Month=",i, 
                  " GROUP BY UniqueCarrier,Origin,Dest,Month,DayOfWeek,CRSDepHour", sep="")
  df <- dbGetQuery(newdb,query) 
}  

system.time(dfPar <- mclapply(1:12, taskFun, mc.cores=4))
# user   system  elapsed 
# 1070.076   84.436  326.106

head(dfPar[[2]])
#   total DepDelay30 DepDelay60 DepDelay180    prop30    prop60 prop180
# 1     7          0          0           0 0.0000000 0.0000000   0.000
# 2     7          1          0           0 0.1428571 0.0000000   0.000
# 3     7          0          0           0 0.0000000 0.0000000   0.000
# 4     7          2          2           0 0.2857143 0.2857143   0.000
# 5     8          2          1           1 0.2500000 0.1250000   0.125
# 6     8          2          1           0 0.2500000 0.1250000   0.000
#   UniqueCarrier Origin Dest Month DayOfWeek CRSDepHour
# 1            9E    ABE  DTW     2         1          6
# 2            9E    ABE  DTW     2         1         12
# 3            9E    ABE  DTW     2         1         16
# 4            9E    ABE  DTW     2         2          6
# 5            9E    ABE  DTW     2         2         12
# 6            9E    ABE  DTW     2         2         16