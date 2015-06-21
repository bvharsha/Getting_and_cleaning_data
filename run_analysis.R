merge.data <- function(path) {
  
  #reading test data
  dir <- paste("./", path,"/test/X_test.txt", sep="")
  X_test <- read.table(dir)
  dir <- paste ("./", path,"/test/y_test.txt", sep="")
  Y_test <- read.table(dir)
  dir <- paste ("./", path,"/test/subject_test.txt", sep="")
  subject_test <- read.table(dir)
  
  #reading train data
  dir <- paste("./", path,"/train/X_train.txt", sep="")
  X_train <- read.table(dir)
  dir <- paste ("./", path,"/train/y_train.txt", sep="")
  Y_train <- read.table(dir)
  dir <- paste ("./", path,"/train/subject_train.txt", sep="")
  subject_train <- read.table(dir)
  
  dir <- paste("./", path, "/activity_labels.txt", sep="")
  act.lbl <- read.table(dir)
  
  Y_test <- merge(Y_test,act.lbl,by="V1")
  Y_train <- merge(Y_train,act.lbl,by="V1")
  
  test_data <- cbind(subject_test, Y_test, X_test)
  train_data <- cbind(subject_train, Y_train, X_train)
    
  #merging test and train data
  all.data <- rbind(test_data, train_data)
  
  return(all.data)
  
}

# check1 <- merge.data("UCI HAR Dataset") - to check progress till 1st function

mean.std <- function(compdata,path){
  
  dir <- paste("./", path, "/features.txt", sep="")
  features.data <- read.table(dir)
  
  mean.std.only <- subset(features.data,  grepl("mean|std", features.data$V2) )
  

  colnames(compdata) <- c("Subject","Activity_Id","Activity",as.vector(features.data[,2]))
  
  
  mean_columns <- grep("mean", colnames(compdata), fixed=TRUE)
  std_columns <- grep("std", colnames(compdata), fixed=TRUE)
  

  mean.std <- c(mean_columns, std_columns)
  

  
  extr.mean.std <- compdata[,c(1,2,3,mean.std)]
  return (extr.mean.std)
 
    
  
}


# check2 <- mean.std(check1,"UCI HAR Dataset") - to check progress till 2nd function

tidy <- function(mean_std_ext){
  
   
  melt.data <- melt(mean_std_ext, id=c("Subject","Activity_Id","Activity"))
  
 
  tidy.data <- cast(melt.data, formula = Subject + Activity_Id + Activity ~ variable, mean)
  
  write.table(tidy.data, file="./final.txt", sep="\t", row.names=FALSE)
}

# check3 <- tidy(check2) 
