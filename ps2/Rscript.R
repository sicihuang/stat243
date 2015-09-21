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
  block <- read.csv(con, header=FALSE, sep=",", colClasses=colChar, nrows=100000, stringsAsFactors = FALSE )
  
  #loop through the sampleRows 
    for( j in 1:10000){
  #get the sample row numbers within the ith block
      if( sampleRows[j] > (i-1)*blockSize && sampleRows <= i*blockSize){
  #save to the data frame we created before
        data[j,] <- block[sampleRows[j],]
      }
    }
  }
  #close connection
  close(con)

  #open connection
  con <- bzfile("ss13hus.csv.bz2", open="r")
  
  #read in the first row to get rid of header
  read.csv( file=con, nrows=1, header=FALSE)
  
  #test time for read.csv
  system.time(block1 <- read.csv(con, header=FALSE, sep=",", nrows=100000, stringsAsFactors = FALSE ))
  
  ## user  system elapsed 
  ## 19.554   0.181  19.739
  
  #test time for readLine
  system.time(block2 <- readLines(con, n=100000, skipNul = FALSE))
  
  ## user  system elapsed 
  ## 18.116   0.068  18.187
  
 