#Reading and creating a vector of feature names
features=as.vector(t(read.table("features.txt",header=F)[2]))

#Reading the test set
X_test=read.table("test/X_test.txt",header=F)
names(X_test)=features

#Reading the test activities labels vector
y_test=read.table("test/y_test.txt")
names(y_test)="activity"

#Reading test subjects vector
subject_test=read.table("test/subject_test.txt")
names(subject_test)="subject"

#cbind the test set with test subjects vector and test activities labels vector
test=cbind(subject_test,y_test,X_test)


#Reading the training set
X_train=read.table("train/X_train.txt",header=F)
names(X_train)=features

#Reading the training activities labels vector
y_train=read.table("train/y_train.txt")
names(y_train)="activity"

#Reading training subjects vector
subject_train=read.table("train/subject_train.txt")
names(subject_train)="subject"

#cbind the train set with train subjects vector and train activities labels vector
train=cbind(subject_train,y_train,X_train)

#Merging the training and the test sets to create one data set
data=rbind(train,test)

library(plyr)

#Extracting only the measurements on the mean and standard deviation for each measurement
data_mean_std=select(data,subject,activity,grep("mean|std",features)+2)

#Reading the activity labels file
activity_labels=read.table("activity_labels.txt",header=F)

#Using descriptive activity names to name the activities in the data set
for(i in 1:6){
  data_mean_std=mutate(data_mean_std,activity=ifelse(activity==activity_labels[i,1],activity_labels[i,2],activity))
}

#Labeling the data set with descriptive variable names
new_names=names(data_mean_std)
new_names=gsub("[(][)]", "", new_names)
new_names=gsub("^t", "TimeDomain_", new_names)
new_names=gsub("^f", "FrequencyDomain_", new_names)
new_names=gsub("Acc", "Accelerometer", new_names)
new_names=gsub("Gyro", "Gyroscope", new_names)
new_names=gsub("Mag", "Magnitude", new_names)
new_names=gsub("-mean-", "_Mean_", new_names)
new_names=gsub("-std-", "_StandardDeviation_", new_names)
new_names=gsub("-", "_", new_names)
names(data_mean_std)=new_names

#Creating tidy data set with the average of each variable for each activity and each subject
tidy_data=aggregate(data_mean_std[,3:81], by = list( subject = data_mean_std$subject,activity = data_mean_std$activity),FUN = mean)

#Writing out the tidy data
write.table(x=tidy_data,file="tidy_data.txt",row.names=FALSE)
