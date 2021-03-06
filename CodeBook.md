# Getting and Cleaning Data Course Project Code Book
Jason McKenna  
September 26, 2017  

CodeBook: Getting and Cleaning Data Course Project (http://jrmckenna.github.io/gettingandcleaningdata)

The purpose of this project was to demonstrate an ability to collect, work with, and clean a data set. The goal was to prepare tidy data that can be used for later analysis. Included in the Github repository (gettingandcleaningdata) is a tidy data set as described below, a code book that describes the variables and the data processing, the unprocessed data, the R script ued to construct the tidy data, anda README file that explains how the script works.


1. Background (from course asignment)

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip


2. The UCI Data (from authors website 

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.


3.Analysis

The included R script (run_analysis.R) does the following.

    -Merges the training and the test sets to create one data set.
    -Extracts only the measurements on the mean and standard deviation for each measurement.
    -Uses descriptive activity names to name the activities in the data set
    -Appropriately labels the data set with descriptive variable names.
    -From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

The original data variable names corresponding to the mean and standard variations are:



The cleaned data variables coreesponding to the means and standard deviations of the original data are:

 [1] "subject"                         "tBodyAcc-mean()-X"               "tBodyAcc-mean()-Y"               "tBodyAcc-mean()-Z"              
 [5] "tBodyAcc-std()-X"                "tBodyAcc-std()-Y"                "tBodyAcc-std()-Z"                "tGravityAcc-mean()-X"           
 [9] "tGravityAcc-mean()-Y"            "tGravityAcc-mean()-Z"            "tGravityAcc-std()-X"             "tGravityAcc-std()-Y"            
[13] "tGravityAcc-std()-Z"             "tBodyAccJerk-mean()-X"           "tBodyAccJerk-mean()-Y"           "tBodyAccJerk-mean()-Z"          
[17] "tBodyAccJerk-std()-X"            "tBodyAccJerk-std()-Y"            "tBodyAccJerk-std()-Z"            "tBodyGyro-mean()-X"             
[21] "tBodyGyro-mean()-Y"              "tBodyGyro-mean()-Z"              "tBodyGyro-std()-X"               "tBodyGyro-std()-Y"              
[25] "tBodyGyro-std()-Z"               "tBodyGyroJerk-mean()-X"          "tBodyGyroJerk-mean()-Y"          "tBodyGyroJerk-mean()-Z"         
[29] "tBodyGyroJerk-std()-X"           "tBodyGyroJerk-std()-Y"           "tBodyGyroJerk-std()-Z"           "tBodyAccMag-mean()"             
[33] "tBodyAccMag-std()"               "tGravityAccMag-mean()"           "tGravityAccMag-std()"            "tBodyAccJerkMag-mean()"         
[37] "tBodyAccJerkMag-std()"           "tBodyGyroMag-mean()"             "tBodyGyroMag-std()"              "tBodyGyroJerkMag-mean()"        
[41] "tBodyGyroJerkMag-std()"          "fBodyAcc-mean()-X"               "fBodyAcc-mean()-Y"               "fBodyAcc-mean()-Z"              
[45] "fBodyAcc-std()-X"                "fBodyAcc-std()-Y"                "fBodyAcc-std()-Z"                "fBodyAcc-meanFreq()-X"          
[49] "fBodyAcc-meanFreq()-Y"           "fBodyAcc-meanFreq()-Z"           "fBodyAccJerk-mean()-X"           "fBodyAccJerk-mean()-Y"          
[53] "fBodyAccJerk-mean()-Z"           "fBodyAccJerk-std()-X"            "fBodyAccJerk-std()-Y"            "fBodyAccJerk-std()-Z"           
[57] "fBodyAccJerk-meanFreq()-X"       "fBodyAccJerk-meanFreq()-Y"       "fBodyAccJerk-meanFreq()-Z"       "fBodyGyro-mean()-X"             
[61] "fBodyGyro-mean()-Y"              "fBodyGyro-mean()-Z"              "fBodyGyro-std()-X"               "fBodyGyro-std()-Y"              
[65] "fBodyGyro-std()-Z"               "fBodyGyro-meanFreq()-X"          "fBodyGyro-meanFreq()-Y"          "fBodyGyro-meanFreq()-Z"         
[69] "fBodyAccMag-mean()"              "fBodyAccMag-std()"               "fBodyAccMag-meanFreq()"          "fBodyBodyAccJerkMag-mean()"     
[73] "fBodyBodyAccJerkMag-std()"       "fBodyBodyAccJerkMag-meanFreq()"  "fBodyBodyGyroMag-mean()"         "fBodyBodyGyroMag-std()"         
[77] "fBodyBodyGyroMag-meanFreq()"     "fBodyBodyGyroJerkMag-mean()"     "fBodyBodyGyroJerkMag-std()"      "fBodyBodyGyroJerkMag-meanFreq()"
[81] "activity"    



Note that although the actual activity remain unchanged as the data names were updated in the program, the values were updated from an integer representation to a character description:

    WALKING (value 1): Subject was walking during the activity
    WALKING_UPSTAIRS (value 2): Subject was walking up a staircase during the activity
    WALKING_DOWNSTAIRS (value 3): Subject was walking down a staircase during the activity
    SITTING (value 4): Subject was sitting during the activity
    STANDING (value 5): Subject was standing during the activity
    LAYING (value 6): Subject was laying down during the activity


3. Tidy Data

The "tidy_data.txt"" output from the "run_analysis.R" program was created using RStudio (version 1.0.143) and the dplyr library (version 0.7.3) and is summarized below:


Classes ‘grouped_df’, ‘tbl_df’, ‘tbl’ and 'data.frame':	180 obs. of  81 variables:
 $ subject                                                       : int  1 1 1 1 1 1 2 2 2 2 ...
 $ activity                                                      : Factor w/ 6 levels "WALKING","WALKING_UPSTAIRS",..: 1 2 3 4 5 6 1 2 3 4 ...
 $ timeDomainBodyAccelerometerMeanX                              : num  0.277 0.255 0.289 0.261 0.279 ...
 $ timeDomainBodyAccelerometerMeanY                              : num  -0.01738 -0.02395 -0.00992 -0.00131 -0.01614 ...
 $ timeDomainBodyAccelerometerMeanZ                              : num  -0.1111 -0.0973 -0.1076 -0.1045 -0.1106 ...
 $ timeDomainBodyAccelerometerstandardDeviationX                 : num  -0.284 -0.355 0.03 -0.977 -0.996 ...
 $ timeDomainBodyAccelerometerstandardDeviationY                 : num  0.11446 -0.00232 -0.03194 -0.92262 -0.97319 ...
 $ timeDomainBodyAccelerometerstandardDeviationZ                 : num  -0.26 -0.0195 -0.2304 -0.9396 -0.9798 ...
 $ timeDomainGravityAccelerometerMeanX                           : num  0.935 0.893 0.932 0.832 0.943 ...
 $ timeDomainGravityAccelerometerMeanY                           : num  -0.282 -0.362 -0.267 0.204 -0.273 ...
 $ timeDomainGravityAccelerometerMeanZ                           : num  -0.0681 -0.0754 -0.0621 0.332 0.0135 ...
 $ timeDomainGravityAccelerometerstandardDeviationX              : num  -0.977 -0.956 -0.951 -0.968 -0.994 ...
 $ timeDomainGravityAccelerometerstandardDeviationY              : num  -0.971 -0.953 -0.937 -0.936 -0.981 ...
 $ timeDomainGravityAccelerometerstandardDeviationZ              : num  -0.948 -0.912 -0.896 -0.949 -0.976 ...
 $ timeDomainBodyAccelerometerJerkMeanX                          : num  0.074 0.1014 0.0542 0.0775 0.0754 ...
 $ timeDomainBodyAccelerometerJerkMeanY                          : num  0.028272 0.019486 0.02965 -0.000619 0.007976 ...
 $ timeDomainBodyAccelerometerJerkMeanZ                          : num  -0.00417 -0.04556 -0.01097 -0.00337 -0.00369 ...
 $ timeDomainBodyAccelerometerJerkstandardDeviationX             : num  -0.1136 -0.4468 -0.0123 -0.9864 -0.9946 ...
 $ timeDomainBodyAccelerometerJerkstandardDeviationY             : num  0.067 -0.378 -0.102 -0.981 -0.986 ...
 $ timeDomainBodyAccelerometerJerkstandardDeviationZ             : num  -0.503 -0.707 -0.346 -0.988 -0.992 ...
 $ timeDomainBodyGyroscopeMeanX                                  : num  -0.0418 0.0505 -0.0351 -0.0454 -0.024 ...
 $ timeDomainBodyGyroscopeMeanY                                  : num  -0.0695 -0.1662 -0.0909 -0.0919 -0.0594 ...
 $ timeDomainBodyGyroscopeMeanZ                                  : num  0.0849 0.0584 0.0901 0.0629 0.0748 ...
 $ timeDomainBodyGyroscopestandardDeviationX                     : num  -0.474 -0.545 -0.458 -0.977 -0.987 ...
 $ timeDomainBodyGyroscopestandardDeviationY                     : num  -0.05461 0.00411 -0.12635 -0.96647 -0.98773 ...
 $ timeDomainBodyGyroscopestandardDeviationZ                     : num  -0.344 -0.507 -0.125 -0.941 -0.981 ...
 $ timeDomainBodyGyroscopeJerkMeanX                              : num  -0.09 -0.1222 -0.074 -0.0937 -0.0996 ...
 $ timeDomainBodyGyroscopeJerkMeanY                              : num  -0.0398 -0.0421 -0.044 -0.0402 -0.0441 ...
 $ timeDomainBodyGyroscopeJerkMeanZ                              : num  -0.0461 -0.0407 -0.027 -0.0467 -0.049 ...
 $ timeDomainBodyGyroscopeJerkstandardDeviationX                 : num  -0.207 -0.615 -0.487 -0.992 -0.993 ...
 $ timeDomainBodyGyroscopeJerkstandardDeviationY                 : num  -0.304 -0.602 -0.239 -0.99 -0.995 ...
 $ timeDomainBodyGyroscopeJerkstandardDeviationZ                 : num  -0.404 -0.606 -0.269 -0.988 -0.992 ...
 $ timeDomainBodyAccelerometerMagnitudeMean                      : num  -0.137 -0.1299 0.0272 -0.9485 -0.9843 ...
 $ timeDomainBodyAccelerometerMagnitudestandardDeviation         : num  -0.2197 -0.325 0.0199 -0.9271 -0.9819 ...
 $ timeDomainGravityAccelerometerMagnitudeMean                   : num  -0.137 -0.1299 0.0272 -0.9485 -0.9843 ...
 $ timeDomainGravityAccelerometerMagnitudestandardDeviation      : num  -0.2197 -0.325 0.0199 -0.9271 -0.9819 ...
 $ timeDomainBodyAccelerometerJerkMagnitudeMean                  : num  -0.1414 -0.4665 -0.0894 -0.9874 -0.9924 ...
 $ timeDomainBodyAccelerometerJerkMagnitudestandardDeviation     : num  -0.0745 -0.479 -0.0258 -0.9841 -0.9931 ...
 $ timeDomainBodyGyroscopeMagnitudeMean                          : num  -0.161 -0.1267 -0.0757 -0.9309 -0.9765 ...
 $ timeDomainBodyGyroscopeMagnitudestandardDeviation             : num  -0.187 -0.149 -0.226 -0.935 -0.979 ...
 $ timeDomainBodyGyroscopeJerkMagnitudeMean                      : num  -0.299 -0.595 -0.295 -0.992 -0.995 ...
 $ timeDomainBodyGyroscopeJerkMagnitudestandardDeviation         : num  -0.325 -0.649 -0.307 -0.988 -0.995 ...
 $ frequencyDomainBodyAccelerometerMeanX                         : num  -0.2028 -0.4043 0.0382 -0.9796 -0.9952 ...
 $ frequencyDomainBodyAccelerometerMeanY                         : num  0.08971 -0.19098 0.00155 -0.94408 -0.97707 ...
 $ frequencyDomainBodyAccelerometerMeanZ                         : num  -0.332 -0.433 -0.226 -0.959 -0.985 ...
 $ frequencyDomainBodyAccelerometerstandardDeviationX            : num  -0.3191 -0.3374 0.0243 -0.9764 -0.996 ...
 $ frequencyDomainBodyAccelerometerstandardDeviationY            : num  0.056 0.0218 -0.113 -0.9173 -0.9723 ...
 $ frequencyDomainBodyAccelerometerstandardDeviationZ            : num  -0.28 0.086 -0.298 -0.934 -0.978 ...
 $ frequencyDomainBodyAccelerometerMeanFrequencyX                : num  -0.2075 -0.4187 -0.3074 -0.0495 0.0865 ...
 $ frequencyDomainBodyAccelerometerMeanFrequencyY                : num  0.1131 -0.1607 0.0632 0.0759 0.1175 ...
 $ frequencyDomainBodyAccelerometerMeanFrequencyZ                : num  0.0497 -0.5201 0.2943 0.2388 0.2449 ...
 $ frequencyDomainBodyAccelerometerJerkMeanX                     : num  -0.1705 -0.4799 -0.0277 -0.9866 -0.9946 ...
 $ frequencyDomainBodyAccelerometerJerkMeanY                     : num  -0.0352 -0.4134 -0.1287 -0.9816 -0.9854 ...
 $ frequencyDomainBodyAccelerometerJerkMeanZ                     : num  -0.469 -0.685 -0.288 -0.986 -0.991 ...
 $ frequencyDomainBodyAccelerometerJerkstandardDeviationX        : num  -0.1336 -0.4619 -0.0863 -0.9875 -0.9951 ...
 $ frequencyDomainBodyAccelerometerJerkstandardDeviationY        : num  0.107 -0.382 -0.135 -0.983 -0.987 ...
 $ frequencyDomainBodyAccelerometerJerkstandardDeviationZ        : num  -0.535 -0.726 -0.402 -0.988 -0.992 ...
 $ frequencyDomainBodyAccelerometerJerkMeanFrequencyX            : num  -0.209 -0.377 -0.253 0.257 0.314 ...
 $ frequencyDomainBodyAccelerometerJerkMeanFrequencyY            : num  -0.3862 -0.5095 -0.3376 0.0475 0.0392 ...
 $ frequencyDomainBodyAccelerometerJerkMeanFrequencyZ            : num  -0.18553 -0.5511 0.00937 0.09239 0.13858 ...
 $ frequencyDomainBodyGyroscopeMeanX                             : num  -0.339 -0.493 -0.352 -0.976 -0.986 ...
 $ frequencyDomainBodyGyroscopeMeanY                             : num  -0.1031 -0.3195 -0.0557 -0.9758 -0.989 ...
 $ frequencyDomainBodyGyroscopeMeanZ                             : num  -0.2559 -0.4536 -0.0319 -0.9513 -0.9808 ...
 $ frequencyDomainBodyGyroscopestandardDeviationX                : num  -0.517 -0.566 -0.495 -0.978 -0.987 ...
 $ frequencyDomainBodyGyroscopestandardDeviationY                : num  -0.0335 0.1515 -0.1814 -0.9623 -0.9871 ...
 $ frequencyDomainBodyGyroscopestandardDeviationZ                : num  -0.437 -0.572 -0.238 -0.944 -0.982 ...
 $ frequencyDomainBodyGyroscopeMeanFrequencyX                    : num  0.0148 -0.1875 -0.1005 0.1892 -0.1203 ...
 $ frequencyDomainBodyGyroscopeMeanFrequencyY                    : num  -0.0658 -0.4736 0.0826 0.0631 -0.0447 ...
 $ frequencyDomainBodyGyroscopeMeanFrequencyZ                    : num  0.000773 -0.133374 -0.075676 -0.029784 0.100608 ...
 $ frequencyDomainBodyAccelerometerMagnitudeMean                 : num  -0.1286 -0.3524 0.0966 -0.9478 -0.9854 ...
 $ frequencyDomainBodyAccelerometerMagnitudestandardDeviation    : num  -0.398 -0.416 -0.187 -0.928 -0.982 ...
 $ frequencyDomainBodyAccelerometerMagnitudeMeanFrequency        : num  0.1906 -0.0977 0.1192 0.2367 0.2846 ...
 $ frequencyDomainBodyAccelerometerJerkMagnitudeMean             : num  -0.0571 -0.4427 0.0262 -0.9853 -0.9925 ...
 $ frequencyDomainBodyAccelerometerJerkMagnitudestandardDeviation: num  -0.103 -0.533 -0.104 -0.982 -0.993 ...
 $ frequencyDomainBodyAccelerometerJerkMagnitudeMeanFrequency    : num  0.0938 0.0854 0.0765 0.3519 0.4222 ...
 $ frequencyDomainBodyGyroscopeMagnitudeMean                     : num  -0.199 -0.326 -0.186 -0.958 -0.985 ...
 $ frequencyDomainBodyGyroscopeMagnitudestandardDeviation        : num  -0.321 -0.183 -0.398 -0.932 -0.978 ...
 $ frequencyDomainBodyGyroscopeMagnitudeMeanFrequency            : num  0.268844 -0.219303 0.349614 -0.000262 -0.028606 ...
 $ frequencyDomainBodyGyroscopeJerkMagnitudeMean                 : num  -0.319 -0.635 -0.282 -0.99 -0.995 ...
 $ frequencyDomainBodyGyroscopeJerkMagnitudestandardDeviation    : num  -0.382 -0.694 -0.392 -0.987 -0.995 ...
 $ frequencyDomainBodyGyroscopeJerkMagnitudeMeanFrequency        : num  0.191 0.114 0.19 0.185 0.334 ...
 - attr(*, "vars")= chr "subject"
 - attr(*, "drop")= logi TRUE
