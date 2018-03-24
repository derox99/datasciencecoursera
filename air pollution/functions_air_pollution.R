pollutantmean<-function(directory,pollutant,id=1:332){
  allPollutant<-0
  numCampioni<-0
  for (fileName in id) {
    file<-read.csv(file.path(directory,paste(formatC(fileName,width = 3,flag = "0", format='d'),".csv",sep = '')))
    allPollutant<-allPollutant+sum(file[[pollutant]],na.rm = TRUE)
    numCampioni<-numCampioni+sum(!is.na(file[[pollutant]]))
  }
  allPollutant/numCampioni
}

complete<-function(directory,id=1:332){
  df<-data.frame()
  for (fileName in id) {
    file<-read.csv(file.path(directory,paste(formatC(fileName,width = 3,flag = "0", format='d'),".csv",sep = '')))
    df<-rbind(df, data.frame(id=fileName, nobs=sum(complete.cases(file))))
  }
  df
}

corr<-function(directory,threshold=0){
  compl<-complete(directory)
  compl<-compl[compl$nobs>threshold,]
  ret<-c();
  for (fileName in compl$id) {
    file<-read.csv(file.path(directory,paste(formatC(fileName,width = 3,flag = "0", format='d'),".csv",sep = '')))
    ret<-c(ret,cor(file$nitrate,file$sulfate,use = "complete.obs"))
  }
  ret
}