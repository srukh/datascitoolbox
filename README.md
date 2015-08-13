1: Merges the training and the test sets to create one data set.

Read x train file which contains train data 
td <- read.table("./UCI HAR Dataset/train/X_train.txt")
dim(td) 
head(td)

Read y train file which contains train label 
tl <- read.table("./UCI HAR Dataset/train/y_train.txt")
table(tl)

Read  subject_train file which subject train data 
ts <- read.table("./UCI HAR Dataset/train/subject_train.txt")

Read  x_test file which contains test data 
tsd<- read.table("./UCI HAR Dataset/test/X_test.txt")
dim(tsd) 

Read  y_test file which contains test label 
tsl <- read.table("./UCI HAR Dataset/test/y_test.txt") 
table(tsl) 

Read  subject_test file subject test data
tss <- read.table("./UCI HAR Dataset/test/subject_test.txt")

Joining Train Data and Test Data 
jd <- rbind(td, tsd)
dim(jd) 

Joining Train labels and Test lables
jl <- rbind(trainLabel, testLabel)
dim(joinLabel) 

Joining Train subject and Test subect
js <- rbind(trainSubject, testSubject)
dim(joinSubject) 



2: Extracts only the measurements on the mean and standard deviation for each measurement. 

 read features file
f <- read.table("./UCI HAR Dataset/features.txt")
dim(f)  

matched mean or standard deviation (std)
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", f[, 2])
length(meanStdIndices) 
jd<- jd[, meanStdIndices]
dim(jd) 

 removing paranthiese, capatilize M and S and hypern "-"
names(jd) <- gsub("\\(\\)", "", f[meanStdIndices, 2])
names(jd) <- gsub("mean", "Mean", names(jd)) 
names(jd) <- gsub("std", "Std", names(jd)) 
names(jd) <- gsub("-", "", names(jd)) 

3: Uses descriptive activity names to name the activities in the data set

Read Acitivity Label files 
a<- read.table("./UCI HAR Dataset/activity_labels.txt")
a[, 2] <- tolower(gsub("_", "", a[, 2]))

capturing captial letters in string 
substr(a[2, 2], 8, 8) <- toupper(substr(a[2, 2], 8, 8))
substr(a[3, 2], 8, 8) <- toupper(substr(a[3, 2], 8, 8))

 Assigning Activity to data set
al <- a[jl[, 1], 2]
jl[, 1] <- al
names(jl) <- "activity"

4: Appropriately labels the data set with descriptive variable names.

names(js) <- "subject"
cd <- cbind(js, jl, jd)
dim(cd) 
write.table(cd, "merged_data.txt") 


5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

subject Length 
sl <- length(table(js)) 

Activity Length 
acl <- dim(a)[1] 
column Length 
cl <- dim(cd)[2]

 result file being written out 
r <- matrix(NA, nrow=sl*acl, ncol=cl) 
r <- as.data.frame(r)
colnames(r) <- colnames(cd)
row <- 1
for(i in 1:sl) {
  for(j in 1:acl) {
    r[row, 1] <- sort(unique(js)[, 1])[i]
    r[row, 2] <- a[j, 2]
    bool1 <- i == cd$subject
    bool2 <- a[j, 2] == cd$activity
    r[row, 3:cl] <- colMeans(cd[bool1&bool2, 3:cl])
    row <- row + 1
  }
}
head(r)
write.table(r, "clean_Data.txt") 
