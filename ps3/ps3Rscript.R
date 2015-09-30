library(XML)
library(stringr)

theUrl <- "http://www.debates.org/index.php?page=debate-transcripts"

getFirstDebateUrl <- function(url, year){
  #parse url
  doc <- htmlParse(url)
  #get all a nodes
  listOfNodes <- getNodeSet(doc, "//a[@href]")
  #use xmlValue to get the text of the nodes
  #create boolean vector; TRUE if text contains "The First"
  bools <- str_detect( sapply(listOfNodes, xmlValue), "The First" ) 
  #overlay listOfNodes with boolean vector; only include the nodes for the first debates
  #get herf attributes i.e. urls
  firstDebateUrl <- sapply( listOfNodes[bools], xmlGetAttr, "href" )
  #grep url indices in the year we passed in 
  index <- grep(year, firstDebateUrl)
  #return only first debate urls of the relevant year
  firstDebateUrl[index]
}


extractByP <- function(url){
  doc <- htmlParse(url)
  #find matching nodes with pattern "//p/text()" i.e. body paragraph
  bodyByP <- xpathSApply(doc, '//p/text()', xmlValue)
}

listOfUrl <- list()
listOfTrans <- list()
year <- c("1996", "2000", "2004", "2008", "2012")

for( i in 1:5 ){
  listOfUrl[[i]] <- getFirstDebateUrl(theUrl,year[i])
  listOfTrans[[i]] <- extractByP(listOfUrl[[i]])
}

cat(head(listOfTrans[[2]], sep = "\n"))


#initiate a list
dat <- list()

transSplit <- function(transcript){
  #original transcripts were divided up by lines; paste them into one element
  transcript <- paste(transcript, collapse ="\n")
  #split transcript by speaker, which is marked with capital letters followed by colon
  splitBySpeaker <- strsplit(transcript, "[A-Z]+: ")
  #extract speaker names (character vector)
  speakerNames <- str_extract_all(transcript, "[A-Z]+: ")
  data <- data.frame( matrix(0, nrow=length(speakerNames[[1]]), ncol=2) )
  #put speaker names into first column of data frame
  data[,1] <- speakerNames
  #put the rest of the transcript (after stripping out speakers) into second column of 
  #data frame; observe that the first element (header) does not correspond to a speaker, 
  #exclude it
  data[,2] <- splitBySpeaker[[1]][-1]
  #special case: part of header says "speakers: ..."; exclude that element if applicable
  if(data[1,1]=="SPEAKERS: "){
    data <- data[-1,]
  }
  colnames(data) <- c("speaker", "content")
  return(data)
}

mergeSpeaker <- function(data){
  #initiate
  bools <- rep(TRUE, nrow(data))
  for( i in 1:nrow(data) ){
    #adjacent rows by same speaker
    if( identical(data$speaker[i], data$speaker[i+1]) ){
      #combine text spoken
      data[i,2] <- paste(data[i,2], data[i+1,2])
      #set element in logical vector to FALSE so the repetitive row will be left out when 
      #subsetting
      bools[i+1] <- FALSE
    }
  }
  data <- subset(data, bools)
}

chunkBySpeaker <- function(data){
  #get unique speaker names
  uniqueSpeakers <- unique(data[,1])
  for(i in 1:length(uniqueSpeakers) ){
    #assign text spoken by each speaker to the corresponding attribute
    attr(data, uniqueSpeakers[i]) <- data[data$speaker==uniqueSpeakers[i],2]
  }
  return(data)
}

countStr <- function(data, string){
  uniqueSpeakers <- unique(data[,1])
  #set name of the attribute depending on the string passed in 
  attrNames <- sapply(uniqueSpeakers, function(x){paste(x, string)})
  for( i in 1:length(uniqueSpeakers) ){
    #get a vector of the locations of the string; save its length i.e. number of appearance 
    #to the corresponding attribute
    attr(data, attrNames[i]) <- length(grep(string, attr(data, uniqueSpeakers[i])))
  }
  return(data)
}

#loop through transcripts from 5 years (list of 5 transcripts)
for(i in 1:5){
  dat[[i]] <- transSplit(listOfTrans[[i]])
  dat[[i]] <- mergeSpeaker(dat[[i]])
  #strip out new line characters
  dat[[i]]$content <- sapply(dat[[i]]$content, function(x){str_replace_all(x, "\n", " ")})
  dat[[i]] <- chunkBySpeaker(dat[[i]])
  dat[[i]] <- countStr(dat[[i]], "APPLAUSE")
  dat[[i]] <- countStr(dat[[i]], "LAUGHTER")
  #strip out text not spoken
  dat[[i]]$content <- sapply(dat[[i]]$content, function(x){str_replace_all(x, "\\([a-zA-Z]+\\)", "")})
  #update attributes for text spoken by each speaker
  dat[[i]] <- chunkBySpeaker(dat[[i]])
}

head(dat[[5]])
head(attr(dat[[5]], "OBAMA: "))
attr(dat[[5]], "OBAMA:  LAUGHTER")


bySentence <- list()
byWord <- list()

extractSentence <- function( data ){
  uniqueSpeakers <- unique(data[,1])
  sentenceSplit <- list()
  for( i in 1:length(uniqueSpeakers) ){
    #get text spoken by ith speaker
    attrSpeaker <- attr(data, uniqueSpeakers[i])
    #checked website: no exclamation points, no Mrs Ms Prof
    #swap Mr./Dr. for Mr/Dr so the period doesn't interfere with other periods when 
    #spliting into sentences
    sentenceSplit[[i]] <- str_replace_all(attrSpeaker, "Mr. ", "Mr ")
    sentenceSplit[[i]] <- str_replace_all(sentenceSplit[[i]], "Dr. ", "Dr ")
    #swap ? for . so it can be used to delimit sentences
    sentenceSplit[[i]] <- str_replace_all(sentenceSplit[[i]],  "\\? ", "\\. ")
    #delet ... in the middle of sentences
    sentenceSplit[[i]] <- str_replace_all(sentenceSplit[[i]], "(\\.\\.\\.)+ ", "")
    #split on ". "; unlist to get a character vector with each element being a sentence 
    sentenceSplit[[i]] <- unlist(strsplit(sentenceSplit[[i]], "\\. "))
  }
  #write text spoken by each speaker (splited by sentences) into a list
  list <- list(sentenceSplit[1:length(uniqueSpeakers)])
}

extractWord <- function( listSentence ){
  wordSplit <- list()
  for(i in 1:3){
    #delet punctuations
    wordSplit[[i]] <- str_replace_all(listSentence[[i]], ",", "")
    wordSplit[[i]] <- str_replace_all(wordSplit[[i]], "-- ", "")
    wordSplit[[i]] <- str_replace_all(wordSplit[[i]], '\"', "")
    wordSplit[[i]] <- str_replace_all(wordSplit[[i]], ";", "")
    wordSplit[[i]] <- str_replace_all(wordSplit[[i]], ":", "")
    #split on " "; unlist to get a character vector with each element being a word
    wordSplit[[i]] <- unlist(strsplit(wordSplit[[i]], " "))
  }
  #write text spoken by each speaker (splited by words) into a list
  list <- list(wordSplit[1:length(listSentence)])
}

#loop through transcripts from 5 years
for(i in 1:5){
  bySentence[[i]] <- extractSentence( dat[[i]] )
  byWord[[i]] <- extractWord ( bySentence[[i]][[1]] )
}

head(bySentence[[2]][[1]][[2]])
head(byWord[[2]][[1]][[2]])


sumWords <- list()

for( i in 1:5 ){
  wordCount <- matrix(0, nrow = 3, ncol=4)
  uniqueSpeakers <- unique(dat[[i]][,1])
  #get total number of words from a certain speaker
  numWords <- sapply(byWord[[i]][[1]], length)
  #get number of characters by summing length of individual words
  numChar <- sapply(sapply(byWord[[i]][[1]], str_count), sum)
  #average length of words spoken
  avgLength <- numChar/numWords
  #write into a matrix
  wordCount[,1] <- uniqueSpeakers
  wordCount[,2] <- numWords
  wordCount[,3] <- numChar
  wordCount[,4] <- avgLength
  #assign column names
  colnames(wordCount) <- c("Speaker", "numWords", "numChar", "avgLength")
  #store matrix as ith element in list
  sumWords[[i]] <- wordCount
}

sumWords

  
freqWords <- list()

#vectore of words we are intersted in in the form of proper regular expressions
#use \\b to match the letter following it between a word and a non-word character
vecWords <- c("\\bI\\b", "\\bwe\\b", "\\bAmerican?\\b", "\\bdemocra(cy|tic)\\b", "\\brepublic\\b", 
              "\\bDemocrat(ic)?\\b", "\\bRepublican\\b", "\\bfree(dom)?\\b", "\\bwar\\b", 
              "\\bGod [^b][^l][^e][^s][^s]", "\\bGod Bless\\b", "\\bJesus|Christ|Christian\\b")

for( i in 1:5 ){
  numCount <- matrix(0, nrow = 3, ncol = 13)
  numCount[,1] <- unique(dat[[i]][,1])
  #loop through all words we are interested in 
  for(j in 1:length(vecWords)){
    #for each debate and speaker, grep for the locations where a certain word appeared 
    #(returns vector); and compute the length of the vector, which gives the number of appearance
    vecCount <- sapply(sapply(bySentence[[i]][[1]], function(x){grep(vecWords[j], x)}), length)
    #save counts (vector, each entry is from a different speaker) to matrix
    numCount[,j+1] <- vecCount
  }
  colnames(numCount) <- c("Speaker", "I", "we", "America{,n}", "democra{cy,tic}", "republic", 
                          "Democrat{,ic}", "Republican", "free{,dom}", "war", "God", 
                          "God Bless", "{Jesus, Christ, Christian}")
  #store matrix as ith element in list
  freqWords[[i]] <- numCount
}

freqWords


ramWalk <- function( n, fullpath = TRUE ){
  #assertions to make sure the length of walk passed in is a positive integer
  if(n<0){
    stop("Number of steps needs to be positive.")
  }
  if(n==0){
    stop("No steps taken.")
  }
  #check if n is an integer
  #is.integer doesn't always return logical values
  #can also use mod
  if( !isTRUE(n == floor(n)) ){
    stop("Number of steps needs to be an integer")
  }
  
  #random walk of length n has n-1 steps
  #sample n-1 random values with 1=going forward, -1=going backward
  stepDir <- sample( c(-1,1), n-1, replace=TRUE)
  #sample n-1 random values with TRUE=move horizontally, FALSE=move vertically
  horizontal <- sample( c(TRUE, FALSE), n-1, replace=TRUE)
  #get x and y locations as lists of coordinates
  #ifelse takes value of second argument if condition/test(first argument) is TRUE; else 
  #takes value of third argument; if ith step is horizontal, add 1/-1 to xlocation, 
  #otherwise add 1/-1 to y location
  #cumsum returns cumulative sum of 1's and -1's
  xlocation <- c(0, cumsum( ifelse(horizontal, stepDir, 0) ))
  ylocation <- c(0, cumsum( ifelse(horizontal, 0, stepDir) ))
  
  #check if second(optional) argument passed in is false
  if( fullpath == FALSE ){
    #if false, return matrix of last elements of x and y locations in long format
    mat <- t(matrix(c(xlocation[n], ylocation[n]), ncol=2))
    rownames(mat) <- c("x", "y")
    return(mat)
  }
  else{
    #otherwise, return matrix of all elements of x and y locations
    mat <- t(matrix(c(xlocation, ylocation), ncol=2))
    rownames(mat) <- c("x", "y")
    return(mat)
  }
}

ramWalk(6)
ramWalk(6, fullpath = FALSE)


#constructor; random walk of the length passed in and default starting point (0,0)
#can access the length, path, and starting point of the walk for objects of rw class 
rw <- function( lengthWalk = NA, start = c(0,0) ){
  obj <- list( lengthWalk = lengthWalk, path = ramWalk(lengthWalk), start = start )
  class(obj) <- "rw"
  return(obj)
}

#define new generic print method
print <- function( obj, ... ){
  UseMethod("print")
}

#rw class specific print method; returns final position, length of walk, and starting 
#point of walk
print.rw <- function( obj ){
  print.default(c("The final position is: ", obj$path[,obj$lengthWalk] ))
  print.default(c("The length of the walk is ", obj$lengthWalk))
  print.default(c("The starting point is ", obj$start))
}

x <- rw(6)
x$path
print(x)

plot <- function( obj, ... ){
  UseMethod("plot")
}

#rw class specific plot method; plot the path of the walk; set ranges of the x and y axes 
#to be the ranges of the x and y coordinates of the path respectively
plot.rw <- function( obj ){
  plot.default(obj$path[1,], obj$path[2,], type="l", xlab="x", ylab="y", main="Random Walk
               in Two Dimensions", xlim=range(obj$path[1,]),ylim=range(obj$path[2,]))
}

x <- rw(1000)
plot(x)

#rw class specified operator; returns the (number passed in)th row in the path matrix; 
#gives the ith position of the walk  
`[.rw` <- function( obj, index ){ 
  position <- obj$path[,index]
}

x[98]

'start<-' <- function( obj, ...){
  UseMethod("start<-")
}

#replacement method; update starting point of walk to value (numeric vector) passed in; 
#shift path to that of the walk starts at new starting point
#by default, replacement function passes in the new value as a parameter named value
'start<-.rw' <- function( obj, value ){
  obj$start <- value
  obj$path <- obj$path+value
  return(obj)
}

x <- rw(6)
x$start
x$path

start(x) <- c(2,4)
x$start
x$path