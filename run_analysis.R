run_analysis <- function(x){
  #Variables used in this function:
    #features and features2 - list of feature labels for the tidy data set, and a vectorized version
    #activities and activies2 - list of activity labesl for the tidy data, and a vectorized version
    #subject_test, x_text, y_test - data tables from the testing data for the experiment
    #subject_train, x_train, y_train - data tables from the training data for the experiment
    #testing1 and training1 - combined data for the testing set and the training set respectably
    #complete - merged training and testing data
    #complete2 - filtered data set that only includes columns that contain "mean" or "std" in the colname
    #tidy_data - data set with the Subject, Activity, and filtered observations from complete2
    #tidy_data2 - tidy_data data set grouped by Subject and Activity
    #mean_tidy_data - summarized tidy_data2 shows displays mean for each Activity by Subject
  
  #sets the working directory to the correct forlder 
  setwd("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset")
  
  #reads in the features file
  features<- read.table("features.txt")
  #creates a vector of the feature labels
  features2<- c(as.character(features$V2))
  
  #reads in acitvity names
  activities <- read.table("activity_labels.txt")
  #creates a vector of the activity labels
  activities2 <- c(as.character(activities$V2))
  
  #changed wd to "test"
  setwd("./test")
  
  #creates a complete data set from the testing data
  create_testset <-function(){
    
    #next three lines read in test data 
    subject_test<-read.table("subject_test.txt")
    x_test<- read.table("X_test.txt")
    y_test<- read.table("y_test.txt")
    #binds subject_test, y_test, and x_test together
    testing1<<-cbind(subject_test,y_test,x_test)
    #counts columns in "testing1" (starting from 3), and relabels using features
    
  }
  
  create_testset()
  
  #renames testing1 columns
  for (i in 3:ncol(testing1)){
    names(testing1)[i]<-features2[i-2]
  }
  
  #changes wd to "train"
  setwd("../train")
  
  #creates a complete data set from the training data
  create_trainset<-function(){
    
    
    #next three lines read in the training data
    subject_train<- read.table("subject_train.txt")
    x_train<-read.table("X_train.txt")
    y_train<-read.table("y_train.txt")
    #binds the subject label to the observations
    training1<<-cbind(subject_train,y_train,x_train)
    #counts columns in "training1" (starting from 3), and relables using features
    
  }
  
  create_trainset()
  
  #renames training1 columns
  for (i in 3:ncol(training1)){
    names(training1)[i]<-features2[i-2]
  }
  
  #binds training and testing data
  complete<- rbind(training1,testing1)
  
  #uses grep to find only columns with "mean" or "std" in the colnames
  complete2 <- complete[,c(colnames(complete)[grep("mean|std",colnames(complete))])]
  
  #binds complete2 to the subject and activity columns
  tidy_data <- cbind(complete[,1:2],complete2) 
  
  #renames columns 1 and 2 to "subject" and "activity"
  names(tidy_data)[1:2]<- c("Subject","Activity")
  
  #loops through tidy_data$Activity and renames to values in activites2
  for (i in 1:nrow(tidy_data)){
    tidy_data$Activity[i] <- activities2[as.numeric(tidy_data$Activity[i])]
  }
  
  #loads dplyr library
  library(dplyr)
  
  #groups the tidy_data by Subject and Activity
  tidy_data2<- group_by(tidy_data, Subject, Activity)
  
  #summarizes the tidy_data2 data frame using summarize_each. New data frame shows the mean for each subject and activity
  mean_tidy_data<<- summarize_each(tidy_data2, funs(mean))
  
}

