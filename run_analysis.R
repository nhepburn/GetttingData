# R script for the Getting and Cleaning Data Course project
# requires dplyr and tidyr
library(tidyr)
library(dplyr)

# read in the training dataset
train <- read.table(file="X_train.txt")
test <- read.table(file="X_test.txt")

# assign subject variable to each data set. Since the subject numbers both
# start at 1 in each table, the train dataset subjects will be train1, train2, etc
# the test dataset subjects are test1, test2, test3, etc

trainsubjects <- read.table("subject_train.txt")
train$subject <- trainsubjects$V1

testsubjects <- read.table("subject_test.txt")
test$subject <- testsubjects$V1

# combine both the test and train datasets into one big dataframe
combined <- tbl_df(rbind(test,train))

# determine which variables involve measurements on the mean
# read in the descriptive names of all variables in the set
varnames <- read.table("features.txt")

# use grepl to determine which variables report means or std deviations
varnames$cond <- grepl("mean()|std()",varnames$V2)
varnames <- varnames[varnames$cond==TRUE,c(1,2)]

# modify so that the column numbers are V1, V2, etc
varnames$picks <- paste("V",varnames$V1,sep="")
vars.2.pick <- c("subject",varnames$picks)

# get rid of hyphens in variable names
labels <- gsub("-","",varnames$V2)
labels <- c("subject",labels)

# select appropriate variables from combined dataset into final dataframe
final <- combined[,vars.2.pick]

# now compute means for each subject
final.tidy <- final %>%
      group_by(subject)%>%
      summarise_each(funs(mean))

# assign descriptive labels to variables
names(final.tidy) <- labels

write.table(final.tidy,file="tidy.txt",row.name=FALSE)
