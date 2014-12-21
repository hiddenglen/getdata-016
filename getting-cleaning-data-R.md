Getting and Cleaning Data - Course Project
===================================================
##Cleaning Human Activity Recognition Using Smartphones Dataset  
  
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.  
  
The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

##To Clean Data
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected. 

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: <http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones>

Here are the data for the project: <https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>

###Create one R script called run_analysis.R that does the following. 
1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

##Step 1: Read data and merge data
###Set Working Directory 
```{r setworkingdirectory}

```

###Read x and y variables for Training and Testing 
The data sets are available in two different sets - training set and testing set. We will now read all the data sets to appropriate variables.

```{r readTrainTestxy, cache=TRUE}
##setwd(file_path_work_dir)
xtrain<-read.table("train/X_train.txt")
xtest<-read.table("test/X_test.txt")
#variable Names
features<-read.table("features.txt")
#activity 
ytrain<-read.table("train/y_train.txt")
ytest<-read.table("test/y_test.txt")
#activity Labels for the data above
activityLabels<-read.table("activity_labels.txt")
#Read suubject from files as numbers
subjectTrain<-read.table("./train/subject_train.txt")
subjectTest<-read.table("./test/subject_test.txt")
```

###Merge training and testing data to x and y variable
We will now merge xtrain and xtest datasets to x variable and ytraiing and ytest datasets to y variable and subjectTrain and SubjectTest to subject variable.
```{r combineTestTrainToxy}
x<-rbind(xtrain,xtest)
y<-rbind(ytrain,ytest)
subject<-rbind(subjectTrain,subjectTest)
```
With this, training and test variables for x and y are merged to x and y variables.

The features table shows two columns. we don't need the first column. We only need the second column as variable names. Also, the variable contains uppercase and lowercase characters. Change all to lowercase to avoid confusion.
```{r convertfeaturesvectortolower}
features<-tolower(features[,2])
```

###Apply the feature name labels as names of x variable
```{r giveVariablenamesTox}
names(x)<-features
names(y)<-"activity"
names(subject)<-"subject"
```

###Merge the three variables x y and subject to one
```{r mergeVariablesXYandSubject}
data<-cbind(y,subject,x)
```


##Step 2: Extract only Mean and Standard Deviation values

###Now find out only variables that are mean values and standard deviation values.
We want only the means or std outside the braces eg
 - mean(abcsum,efg) will count
 - sum(abcmean,abcstd) does not count as mean or std
```{r meanVariableFind}
meanStdColumns<-grep("[m|M]ean\\(.*?\\)|[s|S]td\\(.*?\\)",names(data))
# also the regex "mean\\(\\)|std\\(\\)" works as it is already lowercase
```

With this code, the meanStdColumns has missed activity and subject, so include these too in our final result. In our case, they are column 1 and 2 of data variable.
```{r addColumns1And2WhichWasMissedDuringFiltering}
newColIndex<-c(1,2,meanStdColumns)
```

###Extract those columns to new variable
```{r extractMeanStdColumns}
reduced<-data[,newColIndex]
```


##Step 3: Use descriptive activity names to name the activities in the data set
Now we will replace numbers in activity columns
###Replace activities in variable y with labels
```{r replaceActivityLabessIny}
#There are six activity labels so loop 6 times 
#and replace y with  labels
for(a in 1:6){
  #find a in y
  aInData = which(reduced$activity==a)
  reduced[aInData,1] <- as.character(activityLabels[a,2])
}
```

##Step 4: Appropriately label the data set with descriptive variable names. 

Now change the number to labels like Subject1, Subject2...
```{r changeSubjectNumberToLabel}
reduced[,2]<-paste("Subject-",as.character(reduced[,2]),sep="")
```
###Clean and apply variable Names
```{r cleanData}
varName<-colnames(reduced)
#remove () from varName
varName<-gsub("\\()","",varName)
#replace t with Time and f with Frequency
varName<-gsub("^(t)","Time",varName)
varName<-gsub("^(f)","Frequency",varName)
#Capitalize initial letters of body, gravity, accleration, jerk, mag
varName<-gsub("[bB]ody","Body",varName)
varName<-gsub("[gG]ravity","Gravity",varName)
varName<-gsub("[aA]cc","Accleration",varName)
varName<-gsub("[jJ]erk","Jerk",varName)
varName<-gsub("[mM]ag","Magnitude",varName)

#Now Apply the variable names to colnames
colnames(reduced)<-varName
```


##Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Change the subjectname to factor variable so that we can use tapply function to find mean of factors
```{r changeSubjectNameToFactor}
data$subject<-as.factor(data$subject)
```

Finding Mean
```{r findMeanByFactor}
#with aggregate function (this gives some warnings)
#by subject and activity
#newdata1<-aggregate(reduced,by=list(reduced$subject,reduced$activity),mean)[,-c(3,4)]


library(dplyr)
#Again, find mean by subjectname and activity with dplyr
#for that, combine values of these 
subjectAndActivities<-paste(reduced$subject,reduced$activity,sep="-")
#now merge the new column with old column 
#and remove unnecessary and old columns
data1<-cbind(subjectAndActivities,reduced)[,-c(2,3)]
#find mean by newcol
newdata<-data1 %>% group_by(subjectAndActivities) %>% summarise_each(funs(mean))
#now write this data to a file
write.table(newdata,file="tidydata_mean_subject-activities.txt",row.name=FALSE)
#write.table(colnames(newdata),file="codebook.md",row.name=FALSE)
#original data
dim(data)
dim(reduced)
dim(newdata)

```