  #read in the number of rows, columns, and the 13 column numbers from txt files as numeric values
  rows <- as.numeric(readLines("nRows.txt"))
  cols <- as.numeric(readLines("nCols.txt"))
  sampleCols <- as.numeric(readLines("sampleColNum.txt"))
  
  #create character array with length equals to the total number of columns with "NULL" on each position
  colChar <- rep("NULL", cols)
  #replace "NULL" with "NA" for the columns we want to include
  for (i in 1:cols){
  if( i %in% sampleCols ){
  colChar[i] <- NA
  }
  }
  
  #take a random sample of the row numbers to get the 10000 rows we want to include in sample
  sampleRows <- sample(1:rows-1, 10000, replace=FALSE)
  #sort row numbers in increasing number
  sampleRows <- sort(sampleRows, decreasing = FALSE)
  
  #preallocate memory; construct data frame with size of the final sample
  data <- data.frame(matrix(0, nrow=10000, ncol=13))
  
  #size of each chunk
  blockSize <- 100000
  
  #open connection
  con <- bzfile("ss13hus.csv.bz2", open="r")
  
  #read in the first row to get rid of header
  read.csv( file=con, nrows=1, header=FALSE)
  
  #loop through whole data set in blocks of 100000
  for( i in 1:ceiling( (rows-1)/blockSize ) ){
    #read in the ith block with only the columns we want
    block <- read.csv(con, header=FALSE, sep=",", colClasses=colChar, nrows=100000, 
                      stringsAsFactors = FALSE )
    
    #loop through the sampleRows 
    for( j in 1:10000){
      #get the sample row numbers within the ith block
      if( sampleRows[j] > (i-1)*blockSize && sampleRows <= i*blockSize){
        #save to the data frame we created before
        data[j,] <- block[sampleRows[j]-(i-1)*blockSize,]
      }
    }
  }
  #close connection
  close(con)

  #set column names of data frame
  colnames(data) <- c("ST", "NP", "BDSP", "BLD", "RMSP", "TEN", "FINCP","FPARC", "HHL", "NOC", "MV", "VEH", "YBL")
  
  #write data frame to csv file
  write.csv(data, file = "data.csv")
  
  #open connection
  con <- bzfile("ss13hus.csv.bz2", open="r")
  
  #read in the first row to get rid of header
  read.csv( file=con, nrows=1, header=FALSE)
  
  #test time for read.csv
  system.time(block1 <- read.csv(con, header=FALSE, sep=",", nrows=100000, stringsAsFactors = FALSE ))
  
  ## user  system elapsed 
  ## 19.032   0.127  19.157
  
  #close connection
  close(con)
  
  #open connection
  con <- bzfile("ss13hus.csv.bz2", open="r")
  
  #read in the first row to get rid of header
  read.csv( file=con, nrows=1, header=FALSE)
  
  #test time for readLine
  system.time(block2 <- readLines(con, n=100000, skipNul = FALSE))
  
  ## user  system elapsed 
  ## 18.116   0.068  18.187
  
  #close connection
  close(con)
  
  #cross-tabulation
  prop.table( table(data$VEH, data$NOC), margin = 2 )
  
  ##            1          2          3          4
  ##  1 0.25902335 0.08714734 0.16016427 0.06754530
  ##  2 0.14012739 0.04764890 0.08829569 0.04448105
  ##  3 0.31210191 0.19874608 0.28131417 0.12850082
  ##  4 0.18046709 0.24451411 0.27926078 0.15716639
  ##  5 0.06157113 0.32225705 0.15400411 0.23657331
  ##  6 0.01910828 0.07272727 0.02258727 0.15914333
  ##  7 0.02760085 0.02695925 0.01437372 0.20658979
  
 