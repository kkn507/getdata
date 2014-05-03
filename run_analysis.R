library("plyr")
dataDir <- "UCI HAR Dataset"
bufferSize <- 100

features <- read.table(paste(dataDir,"features.txt",sep="/"),header=FALSE, sep=" ",strip.white=TRUE)

fNum <- dim(features)[1]      # Number of features
fWidth <- 16                  # Feature field width

features <- features[         # mean & std features
  grep(
    "mean()|meanFreq()|std()",
    features$V2
  )
,]

widthVec <- function(x, w) {
  acc <- NULL
  prev <- 0
  for (i in x) {
    if (i - prev == 1) {
      acc <- c(acc,w)
    }
    else {
      acc <- c(acc,w*(prev - i + 1),16)
    }
    prev <- i
  }
  acc
}

widths <- widthVec(features$V1,fWidth)

X_test <- read.fwf(
  paste(dataDir,"test","X_test.txt",sep="/"),
  widths,
  buffersize=bufferSize,
  header=FALSE, 
  strip.white=TRUE
)

X_train <- read.fwf(
  paste(dataDir,"train","X_train.txt",sep="/"),
  widths,
  buffersize=bufferSize,
  header=FALSE, 
  strip.white=TRUE
)

X <- rbind(X_train,X_test)
colnames(X) <- features$V2

subject_test <- read.table(
  paste(dataDir,"test","subject_test.txt",sep="/"),
  strip.white=TRUE,
  header=FALSE
)

subject_train <- read.table(
  paste(dataDir,"train","subject_train.txt",sep="/"),
  strip.white=TRUE,
  header=FALSE
)
subject <- rbind(subject_train,subject_test)
colnames(subject) <- "Subject_ID"
df <- cbind(subject,X)

avg <- ddply(df,.(Subject_ID),numcolwise(mean))
header <- paste(c("",rep("avg-",nrow(avg) - 1)),colnames(avg),sep="")
colnames(avg) <- header

write.table(avg,file="result.txt",sep=",",row.names=FALSE)
