#This is the code for the function that will :

#1)Merges the training and the test data sets from the  
# Human Activity Recognition Using Smartphones Data Set into one data ser call MergeData

#2)Extracts  the measurements on the mean and standard deviation for each measurement

#3)Extracts  average of each variable for each activity and each subject.

#--------------------------------------------------------------------#
#DOWNLOAD FILE#
#URL from the .zip file#
URL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

zipfile<-download.file(URL, "./data.zip")  #downoad file#  
unzipfile<-unzip("./data.zip")             #unzip.file#


#--------------------------------------------------------------------#
##READ FILES#

#read file that Links the class labels with their activity name#
activitylabel<-read.table("./UCI HAR Dataset/activity_labels.txt")

#read file that Links the features name#
featureslabel<-read.table("./UCI HAR Dataset/features.txt")
names<-as.character(featureslabel[,2])
dicnames<-strsplit(names, split="-")
goalrow<-as.table(matrix(data=rep(0,length(names)),nrow=length(names), ncol=2))
colnames(goalrow)<-c("row", "measure")
goalrow[,1]<-c(1:length(names))
for(i in 1:length((names))){
  goalrow[i,2]<-dicnames[[i]][2]
}

selectrow<-matrix(data=rep(NA,length(names)), nrow=length(names), ncol=1)
for(i in 1:length(names)){
  if(goalrow[i,2]=="mean()"){selectrow[i,1]<-i }
  if(goalrow[i,2]=="std()"){selectrow[i,1]<-i }
}

library(functional)
selectrow<-selectrow[apply(selectrow, 1, Compose(is.finite, all)),]
rm(names,dicnames,goalrow,i)

testfeaturesnames<-paste("test",featureslabel[,2],sep="")
trainfeaturesnames<-paste("train",featureslabel[,2],sep="")
rm(featureslabel)
#TRAIN SETS#
#read file that  identifies the subject who performed the test activity#
subjecttrain<-read.table("./UCI HAR Dataset/train/subject_train.txt")
names(subjecttrain)<-c("Subject")
#read X Training set file#
xtrain<-read.table("./UCI HAR Dataset/train/X_train.txt")
colnames(xtrain)<-trainfeaturesnames
xtrain2<-xtrain[, selectrow]
#read Training label set file#
ytrain<-read.table("./UCI HAR Dataset/train/y_train.txt")


#TEST SETS#
#read file that  identifies the subject who performed the test activity#
subjecttest<-read.table("./UCI HAR Dataset/test/subject_test.txt")
names(subjecttest)<-c("Subject")
#read Training set file#
xtest<-read.table("./UCI HAR Dataset/test/X_test.txt")
colnames(xtest)<-testfeaturesnames
xtest2<-xtest[, selectrow]
#read Training label set file#
ytest<-read.table("./UCI HAR Dataset/test/y_test.txt")
rm(testfeaturesnames,trainfeaturesnames,xtest,xtrain)
#--------------------------------------------------------------------#
#Change ytest and ytraining by its labels#

trainlabel<-matrix(data= (rep(NA, (nrow(ytrain*nrow(ytrain))))),
                   nrow=nrow(ytrain), ncol=ncol(ytrain))
colnames(trainlabel)<-c("activitylabel")
for(i in 1:(nrow(ytrain))){
    if(ytrain[i,]==1){ trainlabel[i,1]<-"WALKING"}
    if(ytrain[i,]==2){ trainlabel[i,1]<-"WALKINGUPSTAIRS"}
    if(ytrain[i,]==3){ trainlabel[i,1]<-"WALKINGDOWNSTAIRS"}
    if(ytrain[i,]==4){ trainlabel[i,1]<-"SITTING"}
    if(ytrain[i,]==5){ trainlabel[i,1]<-"STANDING"}
    if(ytrain[i,]==6){ trainlabel[i,1]<-"LAYING"}
}
rm(ytrain)

testlabel<-matrix(data= (rep(NA, (nrow(ytest*nrow(ytest))))),
                   nrow=nrow(ytest), ncol=ncol(ytest))
colnames(testlabel)<-c("activitylabel")
for(i in 1:(nrow(ytest))){
  if(ytest[i,]==1){ testlabel[i,1]<-"WALKING"}
  if(ytest[i,]==2){ testlabel[i,1]<-"WALKINGUPSTAIRS"}
  if(ytest[i,]==3){ testlabel[i,1]<-"WALKINGDOWNSTAIRS"}
  if(ytest[i,]==4){ testlabel[i,1]<-"SITTING"}
  if(ytest[i,]==5){ testlabel[i,1]<-"STANDING"}
  if(ytest[i,]==6){ testlabel[i,1]<-"LAYING"}
}
rm(ytest,activitylabel)

#--------------------------------------------------------------------#
datatest<-cbind(subjecttest,testlabel,xtest2)
datatrain<-cbind(subjecttrain,trainlabel,xtrain2)
rm(subjecttest,subjecttrain,testlabel,trainlabel,xtest2,xtrain2,i,selectrow)

meanmeasuretest<-aggregate(datatest[,3:(ncol(datatest))], 
                       by=list(subject=datatest$Subject, activity=datatest$activitylabel), mean)

meanmeasuretrain<-aggregate(datatrain[,3:(ncol(datatrain))], 
                           by=list(subject=datatrain$Subject, activity=datatrain$activitylabel), mean)

# The Test data set had lest subject than the Train data, we merge by all train set#
Tidydata<-merge(meanmeasuretest,meanmeasuretrain, by=c("subject", "activity"), all.x = T)

write.table(Tidydata,"Tidydata.txt", row.names = F)

