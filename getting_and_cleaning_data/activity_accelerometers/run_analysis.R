merge_tidy_accelerometer<-function(basePath="UCI HAR Dataset",folderName1="test",folderName2="train"){
  #call the function on the train set
  train<-tidy_accelerometer(basePath,folderName2,paste("X_",folderName2,".txt",sep = ""),paste("y_",folderName2,".txt",sep = ""))
  #call the function on the test set
  test<-tidy_accelerometer(basePath,folderName1,paste("X_",folderName1,".txt",sep = ""),paste("y_",folderName1,".txt",sep = ""))
  #perform the union of the two data set
  rbind(train,test)
}


tidy_accelerometer<-function(basePath="UCI HAR Dataset",folderName="test",fileName="X_test.txt",activityFileName="y_test.txt"){
  #load dplyr
  library(dplyr)
  path_x_test<-file.path(basePath,folderName,fileName)
  #read the file with data
  x_test<-read.csv(path_x_test,sep = "",header = F)
  path_col_names<-file.path(basePath,"features.txt")
  #read the file with column names
  col_names<-read.csv(path_col_names,sep = "",header = F)
  #consider only the names
  col_names<-col_names[,2]
  #convert the factors to string
  col_names<-as.character(levels(col_names))[col_names]
  #clean the column names
  col_names<-gsub(pattern = "-|\\(|\\)", replacement = "", x = col_names)
  #give the column names to the data
  names(x_test)<-col_names
  #get only the column with std or mean in the name
  x_test<-x_test[,grep("std|mean",names(x_test))]
  #read the file with the activity id
  activity_id_test<-read.csv(file.path(basePath,folderName,activityFileName),header = F)
  #read the files with the activity labels
  activity_labels<-read.csv(file.path(basePath,"activity_labels.txt"),header = F, sep = "")
  #merge the files together
  activity_labels<-merge(x=activity_id_test,y=activity_labels)
  #consider only the names
  activity_labels<-activity_labels[,2]
  #convert the factor activities to string
  activity_labels<-as.character(levels(activity_labels))[activity_labels]
  #add the activity labels to the data
  x_test<-mutate(x_test,activity=activity_labels)
  #read the subjects
  subjects<-read.csv(file.path(basePath,folderName,paste("subject_",folderName,".txt",sep = "")),header = F)
  #convert to numeric
  s<-as.numeric(subjects[,1])
  #add the subjects to the data
  x_test<-mutate(x_test,subject=s)
}

#this code is for the point 5 of the assignment
#data_set_group<-data_set%>%group_by(subjects,activity)
#summarise_at(data_set_group,vars(everything()), funs(mean(.)))