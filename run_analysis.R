#fetching the data from the files
fetchData <- function(folder_name, file_path) {
  filepath <- file.path(file_path, paste0("y_", folder_name, ".txt"))
  y_data <- read.table(filepath, header=F, col.names=c("ActivityID"))
  
  filepath <- file.path(file_path, paste0("subject_", folder_name, ".txt"))
  subject_data <- read.table(filepath, header=F, col.names=c("SubjectID"))
  
  # read column names and data file
  data_cols <- read.table("features.txt", header=F, as.is=T, col.names=c("MeasureID", "MeasureName"))
  
  filepath <- file.path(file_path, paste0("X_", folder_name, ".txt"))
  data <- read.table(filepath, header=F, col.names=data_cols$MeasureName)
  
  # create a subset variable to store required signal measures
  subset_data_cols <- grep(".*mean\\(\\)|.*std\\(\\)", data_cols$MeasureName)
  data <- data[,subset_data_cols]
  
  # assign the activity id and subject id columns 
  data$ActivityID <- y_data$ActivityID
  data$SubjectID <- subject_data$SubjectID
  
  data
}

# Merge both train and test data sets using fetchdata function

mergeData <- function() {
  data <- rbind(fetchData("test", "test"), fetchData("train", "train"))
  cnames <- colnames(data)
  cnames <- gsub("\\.+mean\\.+", cnames, replacement="Mean")
  cnames <- gsub("\\.+std\\.+",  cnames, replacement="Std")
  colnames(data) <- cnames
  data
}

# Add the activity names as another column to the output
applyActivityLabel <- function(data) {
  activity_labels <- read.table("activity_labels.txt", header=F, as.is=T, col.names=c("ActivityID", "ActivityName"))
  activity_labels$ActivityName <- as.factor(activity_labels$ActivityName)
  data_labeled <- merge(data, activity_labels)
  data_labeled
}

# Add the activity label to the merged data
getMergedLabeledData <- function() {
  applyActivityLabel(mergeData())
}

# Create a tidy data set that has the average of each variable for each activity and each subject.
getTidyData <- function(merged_labeled_data) {
  library(reshape2)
  
  id_vars = c("ActivityID", "ActivityName", "SubjectID")
  measure_vars = setdiff(colnames(merged_labeled_data), id_vars)
  melted_data <- melt(merged_labeled_data, id=id_vars, measure.vars=measure_vars)
  
  dcast(melted_data, ActivityName + SubjectID ~ variable, mean)    
}

# Create the tidy data set and store it to the working directory
createTidyDataFile <- function(fname) {
  tidy_data <- getTidyData(getMergedLabeledData())
  write.table(tidy_data, fname)
}

createTidyDataFile("tidy.txt")
print("File has been created")
