Taken from features_info.txt since the data set returned by my work is a subset of these data.

Feature Selection 
=================

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ
tGravityAcc-XYZ
tBodyAccJerk-XYZ
tBodyGyro-XYZ
tBodyGyroJerk-XYZ
tBodyAccMag
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc-XYZ
fBodyAccJerk-XYZ
fBodyGyro-XYZ
fBodyAccMag
fBodyAccJerkMag
fBodyGyroMag
fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

mean(): Mean value
std(): Standard deviation

There are also two variables:
activity: the activity that leads to the values measured
subject: the person who perform the activity

The list of variables is:
tBodyAccmeanX,tBodyAccmeanY,tBodyAccmeanZ,tBodyAccstdX,tBodyAccstdY,tBodyAccstdZ,tGravityAccmeanX,tGravityAccmeanY,tGravityAccmeanZ,tGravityAccstdX,tGravityAccstdY,tGravityAccstdZ,tBodyAccJerkmeanX,tBodyAccJerkmeanY,tBodyAccJerkmeanZ,tBodyAccJerkstdX,tBodyAccJerkstdY,tBodyAccJerkstdZ,tBodyGyromeanX,tBodyGyromeanY,tBodyGyromeanZ,tBodyGyrostdX,tBodyGyrostdY,tBodyGyrostdZ,tBodyGyroJerkmeanX,tBodyGyroJerkmeanY,tBodyGyroJerkmeanZ,tBodyGyroJerkstdX,tBodyGyroJerkstdY,tBodyGyroJerkstdZ,tBodyAccMagmean,tBodyAccMagstd,tGravityAccMagmean,tGravityAccMagstd,tBodyAccJerkMagmean,tBodyAccJerkMagstd,tBodyGyroMagmean,tBodyGyroMagstd,tBodyGyroJerkMagmean,tBodyGyroJerkMagstd,fBodyAccmeanX,fBodyAccmeanY,fBodyAccmeanZ,fBodyAccstdX,fBodyAccstdY,fBodyAccstdZ,fBodyAccmeanFreqX,fBodyAccmeanFreqY,fBodyAccmeanFreqZ,fBodyAccJerkmeanX,fBodyAccJerkmeanY,fBodyAccJerkmeanZ,fBodyAccJerkstdX,fBodyAccJerkstdY,fBodyAccJerkstdZ,fBodyAccJerkmeanFreqX,fBodyAccJerkmeanFreqY,fBodyAccJerkmeanFreqZ,fBodyGyromeanX,fBodyGyromeanY,fBodyGyromeanZ,fBodyGyrostdX,fBodyGyrostdY,fBodyGyrostdZ,fBodyGyromeanFreqX,fBodyGyromeanFreqY,fBodyGyromeanFreqZ,fBodyAccMagmean,fBodyAccMagstd,fBodyAccMagmeanFreq,fBodyBodyAccJerkMagmean,fBodyBodyAccJerkMagstd,fBodyBodyAccJerkMagmeanFreq,fBodyBodyGyroMagmean,fBodyBodyGyroMagstd,fBodyBodyGyroMagmeanFreq,fBodyBodyGyroJerkMagmean,fBodyBodyGyroJerkMagstd,fBodyBodyGyroJerkMagmeanFreq,activity,subject
