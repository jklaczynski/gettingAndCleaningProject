## script for homework assignment: Getting and cleaning data - week 4
##
## 25 March 2016 Klaczynski
##
## This script assumes we're running in the directory that has the already unzipped files
## from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
## Within this directory, two sub-directories exist - train and test 
## These directories contain identical filenames (but not data) - these are all merged into one
## easy to use, well labeled table.
## 
## 
## 

library(dplyr);
library(reshape2);

# Initialize the result tables to be empty
summaryTable <- NULL;
tidyTable <-NULL;

runAnalysis <- function() 
{
  ## Note - we don't use these in this HW, When I merged them into the dataset (not reading ahead)
  ## performance went to hell... Keeping these declarations for future use
  signalData_FileNames <- c("body_acc_x_", "body_acc_y_", "body_acc_z_", 
			    "body_gyro_x_", "body_gyro_y_", "body_gyro_z_",
			    "total_acc_x_", "total_acc_y_", "total_acc_z_");
  # Collect  the data - NOTE ALWAYS keep the data in the same order (test, then train)
  # First the data dictionary
  activityLabels <- read.table("./activity_labels.txt");
  features <- read.table ("./features.txt");

  # featuresInfo <- read.table("features_info.txt");  # This is not needed

  # Next the data sets
  xData1 <- read.table("./test/X_test.txt");
  xData2 <- read.table("./train/X_train.txt");
  yData1 <- read.table("./test/y_test.txt");
  yData2 <- read.table("./train/y_train.txt");
  subjects1 <- read.table("test/subject_test.txt");
  subjects2 <- read.table("train/subject_train.txt");

  # read the column description file
  
  # note - we are NOT reading in the sensor signal data since the analysis for this assignment does
  # not require it, if we were to do so, however, the steps shown below would be identical to
  # the processing of the xData
  #

  # Assignment - Part 1 - combine test & training data sets

  yData <- rbind(yData1, yData2)  
  subjects <- rbind (subjects1, subjects2)
  xData <- rbind(xData1, xData2) 

  ## clean up/free some memory along the way
  rm(xData1, xData2, yData1, yData2, subjects1, subjects2)

  mergedDataSet <- data.frame(Subject=subjects$V1, Activity=yDataNamed$Label, DataPoints=xData)

  ## Assignment - Part 2 - Extract only the MEAN and STANDARD DEVIATION



  validColumns1 <- grep ("mean", features[,2], ignore.case=TRUE)
  validColumns2 <- grep ("std",  features[,2], ignore.case=TRUE)
  validColumns <- c(validColumns1, validColumns2);

  validNames <- features[validColumns, 2]
 
  ## Assignment Part 3 - Change activity numbers into names
  yDataNamed <- mutate(yData, Label=activityLabels$V2[V1])

  ## Assignment - Part 4 - create the simplified data set
  xDataMatrix = as.matrix(xData)   # Change to a matrix easily pull a 'horizontal' vector
  summaryTable <<- data.frame(Subject=subjects$V1, Activity=yDataNamed$Label, 
                            xDataMatrix[,validColumns]);

  for (i in 1:length(validNames))
  {
    colnames(summaryTable)[i+2] = as.character(validNames[i])
  } 

  ## Assignment - Part 5 - create tidy dataset? 
  tidyTable <<- summarize(group_by(summaryTable, Subject, Activity) ) 
                          

  write.table(tidyTable, "tidyTable.csv.txt", sep=",", row.names=FALSE);

  print ("Analysis complete - see tables:  summaryTable");
  print ("                            and  tidyTable");
  print ("");
  print ("Tidy Data also written to file as : tidyTable.csv");
}
