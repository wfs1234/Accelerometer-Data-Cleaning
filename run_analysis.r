##### READ ME ####
#the below code assumes you've alread downloaded 
#the file from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#and unzipped the contents to a folder of your choosing


#load necessary libraries

library(dplyr)


#set working directory: change the variable below to the folder where your data's located

working_directory <- "C:/Users/Willy/Desktop/rstuff"  #set to directory where .zip file was unzipped
setwd(working_directory)

### Read data from text files to create complete dataset ###


#Test data first

testx = read.table("getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
testx_col = read.table("getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/features.txt")
xcolumns = testx_col$V2
colnames(testx) = xcolumns #relabel colums this is step two in the homework

#subset "X" data to only the Means and Standard Deviation variables

testx_means = testx[,grepl("*mean",names(testx))]
testx_SDs = testx[,grepl("*std",names(testx))]

testy = read.table("getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")
colnames(testy) = "activity"


#Create column that gives human-readable labels to the activities

testy$activity_labels = ifelse(testy$activity == 1, "walking",ifelse(testy$activity == 2,"walking upstairs",ifelse(testy$activity ==3,"walking downstairs",ifelse(testy$activity == 4, "sitting",ifelse(testy$activity == 5,"standing",ifelse(testy$activity==6, "laying","error"))))))

test_sub = read.table("getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
colnames(test_sub) = "Test_Subject"

#combine the test data columns
test_data = cbind(test_sub,testy,testx)
test_data_filtered = cbind(test_sub,testy,testx_means,testx_SDs)


#Prepare the Training data###

trainx = read.table("getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
colnames(trainx) = xcolumns

#subset "X" data to only the Means and Standard Deviation variables

trainx_means = trainx[,grepl("*mean()",names(trainx))]
trainx_SDs = trainx[,grepl("*std()",names(trainx))]

trainy = read.table("getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
colnames(trainy) = "activity"

train_sub = read.table("getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
colnames(train_sub) = "Test_Subject"


#Create column that gives human-readable labels to the activities

trainy$activity_labels = ifelse(trainy$activity == 1, "walking",ifelse(trainy$activity == 2,"walking upstairs",ifelse(trainy$activity ==3,"walking downstairs",ifelse(trainy$activity == 4, "sitting",ifelse(trainy$activity == 5,"standing",ifelse(trainy$activity==6, "laying","error"))))))

train_data = cbind(train_sub,trainy,trainx)

train_data_filtered = cbind(train_sub,trainy,trainx_means,trainx_SDs)


#This completes steps 1-4: a complete merged dataset with human-readable labels, and a second dataset with only the Mean and SD variables

CompleteDataset= rbind(test_data,train_data) 
CompleteDataset_filtered= rbind(test_data_filtered,train_data_filtered)


##### Step 5 #####

grouping = group_by(CompleteDataset_filtered, .dots= c("Test_Subject","activity_labels"))

summarized_data = summarize_all(grouping,mean)

write.csv(summarized_data, file = "Accelerometer Data.csv")
write.table(summarized_data, file = "Accelerometer Data.txt", row.name = FALSE)
