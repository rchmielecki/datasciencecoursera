### load in the data 

train <- read.table("X_train.txt")
test <- read.table("X_test.txt")
train.subj <- read.table("subject_train.txt")
test.subj <- read.table("subject_test.txt")
features <- read.table("features.txt")
y.train <- read.table("y_train.txt")
y.test <- read.table("y_test.txt")

### merge into one dataset

feat.text<-as.character(features[,2])
y.train <- unlist(y.train)
y.test <- unlist(y.test)
y <- c(y.train,y.test)
train.subj <- unlist(train.subj)
test.subj <- unlist(test.subj)
subj <- c(train.subj,test.subj)
data <- rbind(train,test)

### extract only those features pertaining to the mean 
### or standard deviation of a measurement

mean.in.name <- regexpr('mean\\(\\)',feat.text)
std.in.name <- regexpr('std\\(\\)',feat.text)
feat.new <- c(which(mean.in.name>-1),	
	which(std.in.name>-1))
data <- data[,feat.new]

### assign descriptive names to the variables
### 1 -> walking
### 2 -> walking_upstairs
### 3 -> walking_downstairs
### 4 -> sitting
### 5 -> standing
### 6 -> laying

act.labels <- c('walking','walking_upstairs','walking_downstairs',
	'sitting','standing','laying')
activity <- factor(y,labels = act.labels)
data <- cbind(data,subj,activity)
desc_vars <- c(feat.text[feat.new],'subject','activity')
names(data) <- desc_vars
attach(data)

### create independent, tidy data set
### with average of each variable for each activity
### and each subject

aggdata <- aggregate(data[,1:(ncol(data)-2)], by=list(subj,activity),
	FUN=mean,na.rm=TRUE)
tidydata <- aggdata
names(tidydata)[1]<-'Subject'
names(tidydata)[2]<-'Activity'
write.table(tidydata,file='tidydata.txt',row.name=FALSE)




