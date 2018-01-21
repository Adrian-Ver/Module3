# importing relevant files to R --------

getwd()
setwd("C:/Users/Adrian-Ver/Documents/Data Science JHU/Module 3 [Getting and Cleaning Data]/Week 4/M3W4 Project/UCI HAR Dataset")
test_s <- read.table("test/subject_test.txt")
test_X <- read.table("test/X_test.txt")
test_Y <- read.table("test/Y_test.txt")
train_s <- read.table("train/subject_train.txt")
train_X <- read.table("train/X_train.txt")
train_Y <- read.table("train/Y_train.txt")
features <- read.table("features.txt")

# renaming the columns of the data frames -----------

colnames(test_X) <- features[,2]; colnames(train_X) <- features[,2]
make_label <- function(df){
    label <- rep(0,nrow(df))
    for(i in 1:nrow(df)){
        label[i] <- switch(df[i,],"WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
    }
    return(label)
}
test_labels <- make_label(test_Y)
train_labels <- make_label(train_Y)

# merging the test and training to one dataset, adding more column names -----

test <- data.frame(test_s,test_labels,rep("test",nrow(test_X)),test_X)
train <- data.frame(train_s, train_labels,rep("train",nrow(train_X)),train_X)
colnames(test)[1:3] <- c("respondent","activity","group")
colnames(train)[1:3] <- c("respondent","activity","group")
mydata <- rbind(test, train)

# extracting only the mean and sd of each measurement ---------

library(dplyr)
mymeans <- mydata %>% group_by(respondent, activity, group) %>% summarise_all(funs(mean))
mysd <- mydata %>% group_by(respondent, activity, group) %>% summarise_all(funs(sd))
View(mymeans)

# writing a csv file for the tidy data set
write.csv(mydata, 'tidy_data.csv')
write.csv(mymeans, 'tidy_means.csv')
write.csv(mysd, 'tidy_sd.csv')
write.table(mydata, 'tidy_data.txt')
