#read packages 
library(dplyr)
library(data.table)

#Load the data
urll<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destfile<-paste0(getwd(),"/","assessdata3.zip")
download.file(urll,destfile)
unzip("assessdata3.zip",list = TRUE)

#Read the Necassary Files
features<-read.table("./UCI HAR Dataset/features.txt",header = FALSE)
activitys_names<-read.table("./UCI HAR Dataset/activity_labels.txt",header = FALSE)
features_train<-read.table("./UCI HAR Dataset/train/X_train.txt",header = FALSE)
activitys_train<-read.table("./UCI HAR Dataset/train/Y_train.txt",header = FALSE)
subjects_train<-read.table("./UCI HAR Dataset/train/subject_train.txt",header = FALSE)
bodyAccX_train<-read.table("./UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt",header=FALSE)
bodyAccY_train<-read.table("./UCI HAR Dataset/train/Inertial Signals/body_acc_y_train.txt",header=FALSE)
bodyAccZ_train<-read.table("./UCI HAR Dataset/train/Inertial Signals/body_acc_z_train.txt",header=FALSE)
bodyGyroX_train<-read.table("./UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt",header=FALSE)
bodyGyroY_train<-read.table("./UCI HAR Dataset/train/Inertial Signals/body_gyro_y_train.txt",header=FALSE)
bodyGyroZ_train<-read.table("./UCI HAR Dataset/train/Inertial Signals/body_gyro_z_train.txt",header=FALSE)
totalAccX_train<-read.table("./UCI HAR Dataset/train/Inertial Signals/total_acc_x_train.txt",header=FALSE)
totalAccY_train<-read.table("./UCI HAR Dataset/train/Inertial Signals/total_acc_y_train.txt",header=FALSE)
totalAccZ_train<-read.table("./UCI HAR Dataset/train/Inertial Signals/total_acc_z_train.txt",header=FALSE)
features_test<-read.table("./UCI HAR Dataset/test/X_test.txt",header = FALSE)
activitys_test<-read.table("./UCI HAR Dataset/test/Y_test.txt",header=FALSE)
subjects_test<-read.table("./UCI HAR Dataset/test/subject_test.txt",header=FALSE)
bodyAccX_test<-read.table("./UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt",header=FALSE)
bodyAccY_test<-read.table("./UCI HAR Dataset/test/Inertial Signals/body_acc_y_test.txt",header=FALSE)
bodyAccZ_test<-read.table("./UCI HAR Dataset/test/Inertial Signals/body_acc_z_test.txt",header=FALSE)
bodyGyroX_test<-read.table("./UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt",header=FALSE)
bodyGyroY_test<-read.table("./UCI HAR Dataset/test/Inertial Signals/body_gyro_y_test.txt",header=FALSE)
bodyGyroZ_test<-read.table("./UCI HAR Dataset/test/Inertial Signals/body_gyro_z_test.txt",header=FALSE)
totalAccX_test<-read.table("./UCI HAR Dataset/test/Inertial Signals/total_acc_x_test.txt",header=FALSE)
totalAccY_test<-read.table("./UCI HAR Dataset/test/Inertial Signals/total_acc_y_test.txt",header=FALSE)
totalAccZ_test<-read.table("./UCI HAR Dataset/test/Inertial Signals/total_acc_z_test.txt",header=FALSE)

#Rename the columns 


names(features_train)<-features$V2
names(features_test)<-features$V2
names(activitys_train)<-"activitys"
names(activitys_test)<-"activitys"
names(subjects_train)<-"subject"
names(subjects_test)<-"subject"

#Add subject and activities to the training and test datasets 

sub_act_feat_train<-cbind(subjects_train,activitys_train,features_train)
sub_act_feat_test<-cbind(subjects_test,activitys_test,features_test)

#Combine the Test and traning data 
subActFeatures_both<-rbind(sub_act_feat_train,sub_act_feat_test)

#remove all bu the mean and STD results 

mean_std_both<-subActFeatures_both[, c(1,2,grep("[Mm]ean|[Ss]td", colnames(subActFeatures_both)))]

#Add the descriptive activity names 

new1<-arrange(mean_std_both,activitys)

detailed_data<-mutate(new1,activitys=as.character(factor(activitys,levels=1:6,labels=activitys_names$V2)))
# Give Column names a descriptive Title 


names(detailed_data)<-gsub("tBodyAcc-","Body acceleration signal in time domain (from the accelerometer)",names(detailed_data))
names(detailed_data)<-gsub("tBodyAccMag-","Body acceleration signal in time domain applied to Fast Fourier Transform(from the accelerometer)",names(detailed_data))
names(detailed_data)<-gsub("tBodyAccJerk-","Body acceleration jerk signal in time domain (from the accelerometer)",names(detailed_data))
names(detailed_data)<-gsub("tBodyAccJerkMag-","Body acceleration jerk signal in time domain applied to Fast Fourrier Transform (from the accelerometer)",names(detailed_data))
names(detailed_data)<-gsub("tGravityAcc-","Gravity acceleration signal in time domain (from the accelerometer)",names(detailed_data))
names(detailed_data)<-gsub("tGravityAccMag-","Gravity acceleration signal in time domain applied to Fast Fourier Transform(from the accelerometer)",names(detailed_data))
names(detailed_data)<-gsub("tBodyGyro-","Body acceleration signal in time domain (from the gyroscope)",names(detailed_data))
names(detailed_data)<-gsub("tBodyGyroMag-","Body acceleration signal in time domain applied to Fast Fourrier Transform(from the gyroscope)",names(detailed_data))
names(detailed_data)<-gsub("tBodyGyroJerk-","Body acceleration jerk signal in time domain (from the gyroscope)",names(detailed_data))
names(detailed_data)<-gsub("tBodyGyroJerkMag-","Body acceleration jerk signal in time domain applied to Fast Fourrier Transform(from the gyroscope)",names(detailed_data))
names(detailed_data)<-gsub("fBodyAcc-","Body acceleration signal in frequence domain (from the accelerometer)",names(detailed_data))
names(detailed_data)<-gsub("fBodyAccMag-","Body acceleration signal in frequence domain applied to Fast Fourier Transform(from the accelerometer)",names(detailed_data))
names(detailed_data)<-gsub("fBodyAccJerk-","Body acceleration jerk signal in frequence domain (from the accelerometer)",names(detailed_data))
names(detailed_data)<-gsub("fBodyGyro-","Body acceleration signal in frequence domain (from the gyroscope)",names(detailed_data))
names(detailed_data)<-gsub("fBodyAccJerkMag-","Body acceleration jerk signal in frequence domain applied to Fast Fourrier Transform (from the accelerometer)",names(detailed_data))
names(detailed_data)<-gsub("fBodyGyroMag-","Body acceleration signal in frequence domain applied to Fast Fourier Transform (from the gyroscope)",names(detailed_data))
names(detailed_data)<-gsub("mean", " MEAN", names(detailed_data))
names(detailed_data)<-gsub("std", " SD", names(detailed_data))
names(detailed_data)<-gsub("MEAN"," mean",names(detailed_data))
names(detailed_data)<-gsub("[()]","",names(detailed_data))

#Create second tidy dataset with the average of each variable for each activity and each subject

tidydata<-group_by(detailed_data,subject,activitys)
tidydata1<-summarise_all(tidydata,mean)
write.table(tidydata1,"TidyData.txt",row.names = FALSE)
