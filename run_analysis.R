library(dplyr)
data_directory <- "UCI HAR Dataset"

get_activity_label <- function()
{
  read.table(paste('.',data_directory,'activity_labels.txt',sep='/'), stringsAsFactors = FALSE)
}

get_features <- function()
{
  read.table(paste('.',data_directory,'features.txt',sep='/'))
}

#step1

get_data <- function(name)
{
  directory <- paste('.', data_directory, name, sep='/')
  labels <- read.table(paste(directory, sprintf("y_%s.txt", name), sep='/'))
  subjects <- read.table(paste(directory, sprintf("subject_%s.txt", name), sep='/'))
  data <- read.table(paste(directory, sprintf("X_%s.txt", name), sep='/'), 
                     col.names=get_features()$V2)
  data <- mutate(data, labels=labels$V1)
  data <- mutate(data, subjects=subjects$V1)
  data
}

test_data <- get_data('test')
train_data <- get_data('train')
data <- rbind(test_data,train_data)

#step2

extract_data <- function(data)
{
  select(data, matches("labels|subjects|(.*mean.*)|(.*std.*)"))
}

data <- extract_data(data)

#step3

attach_name <- function(data)
{
  get_name <- function(label)
  {
    label_name <- get_activity_label()
    label_name[label_name$V1==label, ]$V2
  }
  mutate(data, names=lapply(labels, get_name))
}

data <- attach_name(data)

#step4
attach_description <- function(data)
{
  
  get_description <- function(names, subjects)
  {
    sprintf("subject %s do the activity %s", subjects, names)
  }
  
  descriptions=get_description(data$names, data$subjects)
  
  mutate(date, descriptions=descriptions)
  
}

data <- attach_description(data)

#step5

create_group_data <- function(data)
{
  group_data <- group_by(data, subjects, labels)
  summarise(group_data, funs(mean))
}

group_data <- create_group_data(data)

write.table(group_data, file="result.txt", row.name=FALSE)

