#Run Analysis (Data Wrangling Project)

#Training Data

#subject_train, X_train, y_train
setwd('C:/Users/Samuel/Documents/Slide Rule/Data/UCI HAR Dataset/train')
temp <- list.files(pattern = "*_train")
myfiles <- lapply(temp, read.table) 

#total_acc_x_train, total_acc_y_train, total_acc_z_train
#body_acc_x_train, body_acc_y_train, body_acc_z_train
#body_gyro_x_train, body_gyro_y_train, body_gyro_z_train 
setwd('C:/Users/Samuel/Documents/Slide Rule/Data/UCI HAR Dataset/train/Inertial Signals')
tempIS <- list.files(pattern = "*_train")
myfilesIS <- lapply(tempIS, read.table) #9 files in the train Inertial Signals folder


#Testing Data

#subject_test, X_test, y_test
setwd('C:/Users/Samuel/Documents/Slide Rule/Data/UCI HAR Dataset/test')
tempTest <- list.files(pattern = "*_test")
myfilesTest <- lapply(tempTest, read.table)

#total_acc_x_test, total_acc_y_test, total_acc_z_test
#body_acc_x_test, body_acc_y_test, body_acc_z_test
#body_gyro_x_test, body_gyro_y_test, body_gyro_z_test 
setwd('C:/Users/Samuel/Documents/Slide Rule/Data/UCI HAR Dataset/test/Inertial Signals')
tempISTest <- list.files(pattern = "*_test")
myfilesISTest <- lapply(tempISTest, read.table)

#Rename Data Frames
for (i in 1:3){
  assign(temp[i], myfiles[[i]])
  assign(tempTest[i], myfilesTest[[i]])
}
for (j in 1:9){
  assign(tempIS[j], myfilesIS[[j]])
  assign(tempISTest[j], myfilesISTest[[j]])
}


#Create one data frame with all data
df_train <- cbind(subject_train.txt, y_train.txt, X_train.txt, total_acc_x_train.txt, total_acc_y_train.txt,
                  total_acc_z_train.txt, body_acc_x_train.txt, body_acc_y_train.txt, body_acc_z_train.txt,
                  body_gyro_x_train.txt, body_gyro_y_train.txt, body_gyro_z_train.txt)

df_test <- cbind(subject_test.txt, y_test.txt, X_test.txt, total_acc_x_test.txt, total_acc_y_test.txt,
                 total_acc_z_test.txt, body_acc_x_test.txt, body_acc_y_test.txt, body_acc_z_test.txt,
                 body_gyro_x_test.txt, body_gyro_y_test.txt, body_gyro_z_test.txt)

df_combined <- rbind(df_train, df_test)

df_combined$subject <- df_combined[, 1]
df_combined$activity_code <- df_combined[, 2]
df_combined[, 1] <- NULL
df_combined[, 2] <- NULL
#Extract the means and standard deviations #####RETURN HERE TO FIX: group here and summarise
library(reshape2)
df_combined <- melt(df_combined, id = c("subject", "activity_code"))
head(df_combined)

df_means_sd <- df_combined %>%
  group_by(subject, activity_code) %>%
  summarise(mean = mean(value),
            std_dev = sd(value)) %>%
  ungroup() %>%
  arrange(subject)
              
#Activity Label
setwd('C:/Users/Samuel/Documents/Slide Rule/Data/UCI HAR Dataset')
act_labels <- read.table("activity_labels.txt")
act_labels$activity_code <-act_labels$V1
act_labels$activity <- act_labels$V2
act_labels$V1 <- NULL
act_labels$V2 <- NULL
df_merge <- inner_join(df_means_sd, act_labels, by = "activity_code")
df_merge[, 2] <- NULL

df_merge
