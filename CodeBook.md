The columns of the final data set contain the average of each variable for each activity and each subject. 
Those variables are the original/untouched mean and standard deviation computed for each measurement. 

I didn't modified the original values to compute that data set, thus the units have not changed.

The only transformations were :
- merge training and test data set.
- translate numerical code for activities into readable labels.
- subset to columns of interest (mean and standard deviation for measurements)

The original code book can be found below.

List of data set columns:

Column # | Column name | Original feature name
---|---|---
1|Subject|NA
2|Activity|NA
3|time_domain_body_acceleration_mean_x_axis|tBodyAcc-mean()-X
4|time_domain_body_acceleration_mean_y_axis|tBodyAcc-mean()-Y
5|time_domain_body_acceleration_mean_z_axis|tBodyAcc-mean()-Z
6|time_domain_body_acceleration_standard_deviation_x_axis|tBodyAcc-std()-X
7|time_domain_body_acceleration_standard_deviation_y_axis|tBodyAcc-std()-Y
8|time_domain_body_acceleration_standard_deviation_z_axis|tBodyAcc-std()-Z
9|time_domain_gravity_acceleration_mean_x_axis|tGravityAcc-mean()-X
10|time_domain_gravity_acceleration_mean_y_axis|tGravityAcc-mean()-Y
11|time_domain_gravity_acceleration_mean_z_axis|tGravityAcc-mean()-Z
12|time_domain_gravity_acceleration_standard_deviation_x_axis|tGravityAcc-std()-X
13|time_domain_gravity_acceleration_standard_deviation_y_axis|tGravityAcc-std()-Y
14|time_domain_gravity_acceleration_standard_deviation_z_axis|tGravityAcc-std()-Z
15|time_domain_body_acceleration_jerk_mean_x_axis|tBodyAccJerk-mean()-X
16|time_domain_body_acceleration_jerk_mean_y_axis|tBodyAccJerk-mean()-Y
17|time_domain_body_acceleration_jerk_mean_z_axis|tBodyAccJerk-mean()-Z
18|time_domain_body_acceleration_jerk_standard_deviation_x_axis|tBodyAccJerk-std()-X
19|time_domain_body_acceleration_jerk_standard_deviation_y_axis|tBodyAccJerk-std()-Y
20|time_domain_body_acceleration_jerk_standard_deviation_z_axis|tBodyAccJerk-std()-Z
21|time_domain_body_gyroscope_mean_x_axis|tBodyGyro-mean()-X
22|time_domain_body_gyroscope_mean_y_axis|tBodyGyro-mean()-Y
23|time_domain_body_gyroscope_mean_z_axis|tBodyGyro-mean()-Z
24|time_domain_body_gyroscope_standard_deviation_x_axis|tBodyGyro-std()-X
25|time_domain_body_gyroscope_standard_deviation_y_axis|tBodyGyro-std()-Y
26|time_domain_body_gyroscope_standard_deviation_z_axis|tBodyGyro-std()-Z
27|time_domain_body_gyroscope_jerk_mean_x_axis|tBodyGyroJerk-mean()-X
28|time_domain_body_gyroscope_jerk_mean_y_axis|tBodyGyroJerk-mean()-Y
29|time_domain_body_gyroscope_jerk_mean_z_axis|tBodyGyroJerk-mean()-Z
30|time_domain_body_gyroscope_jerk_standard_deviation_x_axis|tBodyGyroJerk-std()-X
31|time_domain_body_gyroscope_jerk_standard_deviation_y_axis|tBodyGyroJerk-std()-Y
32|time_domain_body_gyroscope_jerk_standard_deviation_z_axis|tBodyGyroJerk-std()-Z
33|time_domain_body_acceleration_magnitude_mean|tBodyAccMag-mean()
34|time_domain_body_acceleration_magnitude_standard_deviation|tBodyAccMag-std()
35|time_domain_gravity_acceleration_magnitude_mean|tGravityAccMag-mean()
36|time_domain_gravity_acceleration_magnitude_standard_deviation|tGravityAccMag-std()
37|time_domain_body_acceleration_jerk_magnitude_mean|tBodyAccJerkMag-mean()
38|time_domain_body_acceleration_jerk_magnitude_standard_deviation|tBodyAccJerkMag-std()
39|time_domain_body_gyroscope_magnitude_mean|tBodyGyroMag-mean()
40|time_domain_body_gyroscope_magnitude_standard_deviation|tBodyGyroMag-std()
41|time_domain_body_gyroscope_jerk_magnitude_mean|tBodyGyroJerkMag-mean()
42|time_domain_body_gyroscope_jerk_magnitude_standard_deviation|tBodyGyroJerkMag-std()
43|frequency_domain_body_acceleration_mean_x_axis|fBodyAcc-mean()-X
44|frequency_domain_body_acceleration_mean_y_axis|fBodyAcc-mean()-Y
45|frequency_domain_body_acceleration_mean_z_axis|fBodyAcc-mean()-Z
46|frequency_domain_body_acceleration_standard_deviation_x_axis|fBodyAcc-std()-X
47|frequency_domain_body_acceleration_standard_deviation_y_axis|fBodyAcc-std()-Y
48|frequency_domain_body_acceleration_standard_deviation_z_axis|fBodyAcc-std()-Z
49|frequency_domain_body_acceleration_jerk_mean_x_axis|fBodyAccJerk-mean()-X
50|frequency_domain_body_acceleration_jerk_mean_y_axis|fBodyAccJerk-mean()-Y
51|frequency_domain_body_acceleration_jerk_mean_z_axis|fBodyAccJerk-mean()-Z
52|frequency_domain_body_acceleration_jerk_standard_deviation_x_axis|fBodyAccJerk-std()-X
53|frequency_domain_body_acceleration_jerk_standard_deviation_y_axis|fBodyAccJerk-std()-Y
54|frequency_domain_body_acceleration_jerk_standard_deviation_z_axis|fBodyAccJerk-std()-Z
55|frequency_domain_body_gyroscope_mean_x_axis|fBodyGyro-mean()-X
56|frequency_domain_body_gyroscope_mean_y_axis|fBodyGyro-mean()-Y
57|frequency_domain_body_gyroscope_mean_z_axis|fBodyGyro-mean()-Z
58|frequency_domain_body_gyroscope_standard_deviation_x_axis|fBodyGyro-std()-X
59|frequency_domain_body_gyroscope_standard_deviation_y_axis|fBodyGyro-std()-Y
60|frequency_domain_body_gyroscope_standard_deviation_z_axis|fBodyGyro-std()-Z
61|frequency_domain_body_acceleration_magnitude_mean|fBodyAccMag-mean()
62|frequency_domain_body_acceleration_magnitude_standard_deviation|fBodyAccMag-std()
63|frequency_domain_bodybody_acceleration_jerk_magnitude_mean|fBodyBodyAccJerkMag-mean()
64|frequency_domain_bodybody_acceleration_jerk_magnitude_standard_deviation|fBodyBodyAccJerkMag-std()
65|frequency_domain_bodybody_gyroscope_magnitude_mean|fBodyBodyGyroMag-mean()
66|frequency_domain_bodybody_gyroscope_magnitude_standard_deviation|fBodyBodyGyroMag-std()
67|frequency_domain_bodybody_gyroscope_jerk_magnitude_mean|fBodyBodyGyroJerkMag-mean()
68|frequency_domain_bodybody_gyroscope_jerk_magnitude_standard_deviation|fBodyBodyGyroJerkMag-std()

---------------------------
Original code book starts here

---------------------------

Feature Selection 
=================

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

- tBodyAcc-XYZ
- tGravityAcc-XYZ
- tBodyAccJerk-XYZ
- tBodyGyro-XYZ
- tBodyGyroJerk-XYZ
- tBodyAccMag
- tGravityAccMag
- tBodyAccJerkMag
- tBodyGyroMag
- tBodyGyroJerkMag
- fBodyAcc-XYZ
- fBodyAccJerk-XYZ
- fBodyGyro-XYZ
- fBodyAccMag
- fBodyAccJerkMag
- fBodyGyroMag
- fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

- mean(): Mean value
- std(): Standard deviation
- mad(): Median absolute deviation 
- max(): Largest value in array
- min(): Smallest value in array
- sma(): Signal magnitude area
- energy(): Energy measure. Sum of the squares divided by the number of values. 
- iqr(): Interquartile range 
- entropy(): Signal entropy
- arCoeff(): Autorregresion coefficients with Burg order equal to 4
- correlation(): correlation coefficient between two signals
- maxInds(): index of the frequency component with largest magnitude
- meanFreq(): Weighted average of the frequency components to obtain a mean frequency
- skewness(): skewness of the frequency domain signal 
- kurtosis(): kurtosis of the frequency domain signal 
- bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
- angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

- gravityMean
- tBodyAccMean
- tBodyAccJerkMean
- tBodyGyroMean
- tBodyGyroJerkMean

The complete list of variables of each feature vector is:
- tBodyAcc-mean()-X
- tBodyAcc-mean()-Y
- tBodyAcc-mean()-Z
- tBodyAcc-std()-X
- tBodyAcc-std()-Y
- tBodyAcc-std()-Z
- tBodyAcc-mad()-X
- tBodyAcc-mad()-Y
- tBodyAcc-mad()-Z
- tBodyAcc-max()-X
- tBodyAcc-max()-Y
- tBodyAcc-max()-Z
- tBodyAcc-min()-X
- tBodyAcc-min()-Y
- tBodyAcc-min()-Z
- tBodyAcc-sma()
- tBodyAcc-energy()-X
- tBodyAcc-energy()-Y
- tBodyAcc-energy()-Z
- tBodyAcc-iqr()-X
- tBodyAcc-iqr()-Y
- tBodyAcc-iqr()-Z
- tBodyAcc-entropy()-X
- tBodyAcc-entropy()-Y
- tBodyAcc-entropy()-Z
- tBodyAcc-arCoeff()-X,1
- tBodyAcc-arCoeff()-X,2
- tBodyAcc-arCoeff()-X,3
- tBodyAcc-arCoeff()-X,4
- tBodyAcc-arCoeff()-Y,1
- tBodyAcc-arCoeff()-Y,2
- tBodyAcc-arCoeff()-Y,3
- tBodyAcc-arCoeff()-Y,4
- tBodyAcc-arCoeff()-Z,1
- tBodyAcc-arCoeff()-Z,2
- tBodyAcc-arCoeff()-Z,3
- tBodyAcc-arCoeff()-Z,4
- tBodyAcc-correlation()-X,Y
- tBodyAcc-correlation()-X,Z
- tBodyAcc-correlation()-Y,Z
- tGravityAcc-mean()-X
- tGravityAcc-mean()-Y
- tGravityAcc-mean()-Z
- tGravityAcc-std()-X
- tGravityAcc-std()-Y
- tGravityAcc-std()-Z
- tGravityAcc-mad()-X
- tGravityAcc-mad()-Y
- tGravityAcc-mad()-Z
- tGravityAcc-max()-X
- tGravityAcc-max()-Y
- tGravityAcc-max()-Z
- tGravityAcc-min()-X
- tGravityAcc-min()-Y
- tGravityAcc-min()-Z
- tGravityAcc-sma()
- tGravityAcc-energy()-X
- tGravityAcc-energy()-Y
- tGravityAcc-energy()-Z
- tGravityAcc-iqr()-X
- tGravityAcc-iqr()-Y
- tGravityAcc-iqr()-Z
- tGravityAcc-entropy()-X
- tGravityAcc-entropy()-Y
- tGravityAcc-entropy()-Z
- tGravityAcc-arCoeff()-X,1
- tGravityAcc-arCoeff()-X,2
- tGravityAcc-arCoeff()-X,3
- tGravityAcc-arCoeff()-X,4
- tGravityAcc-arCoeff()-Y,1
- tGravityAcc-arCoeff()-Y,2
- tGravityAcc-arCoeff()-Y,3
- tGravityAcc-arCoeff()-Y,4
- tGravityAcc-arCoeff()-Z,1
- tGravityAcc-arCoeff()-Z,2
- tGravityAcc-arCoeff()-Z,3
- tGravityAcc-arCoeff()-Z,4
- tGravityAcc-correlation()-X,Y
- tGravityAcc-correlation()-X,Z
- tGravityAcc-correlation()-Y,Z
- tBodyAccJerk-mean()-X
- tBodyAccJerk-mean()-Y
- tBodyAccJerk-mean()-Z
- tBodyAccJerk-std()-X
- tBodyAccJerk-std()-Y
- tBodyAccJerk-std()-Z
- tBodyAccJerk-mad()-X
- tBodyAccJerk-mad()-Y
- tBodyAccJerk-mad()-Z
- tBodyAccJerk-max()-X
- tBodyAccJerk-max()-Y
- tBodyAccJerk-max()-Z
- tBodyAccJerk-min()-X
- tBodyAccJerk-min()-Y
- tBodyAccJerk-min()-Z
- tBodyAccJerk-sma()
- tBodyAccJerk-energy()-X
- tBodyAccJerk-energy()-Y
- tBodyAccJerk-energy()-Z
- tBodyAccJerk-iqr()-X
- tBodyAccJerk-iqr()-Y
- tBodyAccJerk-iqr()-Z
- tBodyAccJerk-entropy()-X
- tBodyAccJerk-entropy()-Y
- tBodyAccJerk-entropy()-Z
- tBodyAccJerk-arCoeff()-X,1
- tBodyAccJerk-arCoeff()-X,2
- tBodyAccJerk-arCoeff()-X,3
- tBodyAccJerk-arCoeff()-X,4
- tBodyAccJerk-arCoeff()-Y,1
- tBodyAccJerk-arCoeff()-Y,2
- tBodyAccJerk-arCoeff()-Y,3
- tBodyAccJerk-arCoeff()-Y,4
- tBodyAccJerk-arCoeff()-Z,1
- tBodyAccJerk-arCoeff()-Z,2
- tBodyAccJerk-arCoeff()-Z,3
- tBodyAccJerk-arCoeff()-Z,4
- tBodyAccJerk-correlation()-X,Y
- tBodyAccJerk-correlation()-X,Z
- tBodyAccJerk-correlation()-Y,Z
- tBodyGyro-mean()-X
- tBodyGyro-mean()-Y
- tBodyGyro-mean()-Z
- tBodyGyro-std()-X
- tBodyGyro-std()-Y
- tBodyGyro-std()-Z
- tBodyGyro-mad()-X
- tBodyGyro-mad()-Y
- tBodyGyro-mad()-Z
- tBodyGyro-max()-X
- tBodyGyro-max()-Y
- tBodyGyro-max()-Z
- tBodyGyro-min()-X
- tBodyGyro-min()-Y
- tBodyGyro-min()-Z
- tBodyGyro-sma()
- tBodyGyro-energy()-X
- tBodyGyro-energy()-Y
- tBodyGyro-energy()-Z
- tBodyGyro-iqr()-X
- tBodyGyro-iqr()-Y
- tBodyGyro-iqr()-Z
- tBodyGyro-entropy()-X
- tBodyGyro-entropy()-Y
- tBodyGyro-entropy()-Z
- tBodyGyro-arCoeff()-X,1
- tBodyGyro-arCoeff()-X,2
- tBodyGyro-arCoeff()-X,3
- tBodyGyro-arCoeff()-X,4
- tBodyGyro-arCoeff()-Y,1
- tBodyGyro-arCoeff()-Y,2
- tBodyGyro-arCoeff()-Y,3
- tBodyGyro-arCoeff()-Y,4
- tBodyGyro-arCoeff()-Z,1
- tBodyGyro-arCoeff()-Z,2
- tBodyGyro-arCoeff()-Z,3
- tBodyGyro-arCoeff()-Z,4
- tBodyGyro-correlation()-X,Y
- tBodyGyro-correlation()-X,Z
- tBodyGyro-correlation()-Y,Z
- tBodyGyroJerk-mean()-X
- tBodyGyroJerk-mean()-Y
- tBodyGyroJerk-mean()-Z
- tBodyGyroJerk-std()-X
- tBodyGyroJerk-std()-Y
- tBodyGyroJerk-std()-Z
- tBodyGyroJerk-mad()-X
- tBodyGyroJerk-mad()-Y
- tBodyGyroJerk-mad()-Z
- tBodyGyroJerk-max()-X
- tBodyGyroJerk-max()-Y
- tBodyGyroJerk-max()-Z
- tBodyGyroJerk-min()-X
- tBodyGyroJerk-min()-Y
- tBodyGyroJerk-min()-Z
- tBodyGyroJerk-sma()
- tBodyGyroJerk-energy()-X
- tBodyGyroJerk-energy()-Y
- tBodyGyroJerk-energy()-Z
- tBodyGyroJerk-iqr()-X
- tBodyGyroJerk-iqr()-Y
- tBodyGyroJerk-iqr()-Z
- tBodyGyroJerk-entropy()-X
- tBodyGyroJerk-entropy()-Y
- tBodyGyroJerk-entropy()-Z
- tBodyGyroJerk-arCoeff()-X,1
- tBodyGyroJerk-arCoeff()-X,2
- tBodyGyroJerk-arCoeff()-X,3
- tBodyGyroJerk-arCoeff()-X,4
- tBodyGyroJerk-arCoeff()-Y,1
- tBodyGyroJerk-arCoeff()-Y,2
- tBodyGyroJerk-arCoeff()-Y,3
- tBodyGyroJerk-arCoeff()-Y,4
- tBodyGyroJerk-arCoeff()-Z,1
- tBodyGyroJerk-arCoeff()-Z,2
- tBodyGyroJerk-arCoeff()-Z,3
- tBodyGyroJerk-arCoeff()-Z,4
- tBodyGyroJerk-correlation()-X,Y
- tBodyGyroJerk-correlation()-X,Z
- tBodyGyroJerk-correlation()-Y,Z
- tBodyAccMag-mean()
- tBodyAccMag-std()
- tBodyAccMag-mad()
- tBodyAccMag-max()
- tBodyAccMag-min()
- tBodyAccMag-sma()
- tBodyAccMag-energy()
- tBodyAccMag-iqr()
- tBodyAccMag-entropy()
- tBodyAccMag-arCoeff()1
- tBodyAccMag-arCoeff()2
- tBodyAccMag-arCoeff()3
- tBodyAccMag-arCoeff()4
- tGravityAccMag-mean()
- tGravityAccMag-std()
- tGravityAccMag-mad()
- tGravityAccMag-max()
- tGravityAccMag-min()
- tGravityAccMag-sma()
- tGravityAccMag-energy()
- tGravityAccMag-iqr()
- tGravityAccMag-entropy()
- tGravityAccMag-arCoeff()1
- tGravityAccMag-arCoeff()2
- tGravityAccMag-arCoeff()3
- tGravityAccMag-arCoeff()4
- tBodyAccJerkMag-mean()
- tBodyAccJerkMag-std()
- tBodyAccJerkMag-mad()
- tBodyAccJerkMag-max()
- tBodyAccJerkMag-min()
- tBodyAccJerkMag-sma()
- tBodyAccJerkMag-energy()
- tBodyAccJerkMag-iqr()
- tBodyAccJerkMag-entropy()
- tBodyAccJerkMag-arCoeff()1
- tBodyAccJerkMag-arCoeff()2
- tBodyAccJerkMag-arCoeff()3
- tBodyAccJerkMag-arCoeff()4
- tBodyGyroMag-mean()
- tBodyGyroMag-std()
- tBodyGyroMag-mad()
- tBodyGyroMag-max()
- tBodyGyroMag-min()
- tBodyGyroMag-sma()
- tBodyGyroMag-energy()
- tBodyGyroMag-iqr()
- tBodyGyroMag-entropy()
- tBodyGyroMag-arCoeff()1
- tBodyGyroMag-arCoeff()2
- tBodyGyroMag-arCoeff()3
- tBodyGyroMag-arCoeff()4
- tBodyGyroJerkMag-mean()
- tBodyGyroJerkMag-std()
- tBodyGyroJerkMag-mad()
- tBodyGyroJerkMag-max()
- tBodyGyroJerkMag-min()
- tBodyGyroJerkMag-sma()
- tBodyGyroJerkMag-energy()
- tBodyGyroJerkMag-iqr()
- tBodyGyroJerkMag-entropy()
- tBodyGyroJerkMag-arCoeff()1
- tBodyGyroJerkMag-arCoeff()2
- tBodyGyroJerkMag-arCoeff()3
- tBodyGyroJerkMag-arCoeff()4
- fBodyAcc-mean()-X
- fBodyAcc-mean()-Y
- fBodyAcc-mean()-Z
- fBodyAcc-std()-X
- fBodyAcc-std()-Y
- fBodyAcc-std()-Z
- fBodyAcc-mad()-X
- fBodyAcc-mad()-Y
- fBodyAcc-mad()-Z
- fBodyAcc-max()-X
- fBodyAcc-max()-Y
- fBodyAcc-max()-Z
- fBodyAcc-min()-X
- fBodyAcc-min()-Y
- fBodyAcc-min()-Z
- fBodyAcc-sma()
- fBodyAcc-energy()-X
- fBodyAcc-energy()-Y
- fBodyAcc-energy()-Z
- fBodyAcc-iqr()-X
- fBodyAcc-iqr()-Y
- fBodyAcc-iqr()-Z
- fBodyAcc-entropy()-X
- fBodyAcc-entropy()-Y
- fBodyAcc-entropy()-Z
- fBodyAcc-maxInds-X
- fBodyAcc-maxInds-Y
- fBodyAcc-maxInds-Z
- fBodyAcc-meanFreq()-X
- fBodyAcc-meanFreq()-Y
- fBodyAcc-meanFreq()-Z
- fBodyAcc-skewness()-X
- fBodyAcc-kurtosis()-X
- fBodyAcc-skewness()-Y
- fBodyAcc-kurtosis()-Y
- fBodyAcc-skewness()-Z
- fBodyAcc-kurtosis()-Z
- fBodyAcc-bandsEnergy()-1,8
- fBodyAcc-bandsEnergy()-9,16
- fBodyAcc-bandsEnergy()-17,24
- fBodyAcc-bandsEnergy()-25,32
- fBodyAcc-bandsEnergy()-33,40
- fBodyAcc-bandsEnergy()-41,48
- fBodyAcc-bandsEnergy()-49,56
- fBodyAcc-bandsEnergy()-57,64
- fBodyAcc-bandsEnergy()-1,16
- fBodyAcc-bandsEnergy()-17,32
- fBodyAcc-bandsEnergy()-33,48
- fBodyAcc-bandsEnergy()-49,64
- fBodyAcc-bandsEnergy()-1,24
- fBodyAcc-bandsEnergy()-25,48
- fBodyAcc-bandsEnergy()-1,8
- fBodyAcc-bandsEnergy()-9,16
- fBodyAcc-bandsEnergy()-17,24
- fBodyAcc-bandsEnergy()-25,32
- fBodyAcc-bandsEnergy()-33,40
- fBodyAcc-bandsEnergy()-41,48
- fBodyAcc-bandsEnergy()-49,56
- fBodyAcc-bandsEnergy()-57,64
- fBodyAcc-bandsEnergy()-1,16
- fBodyAcc-bandsEnergy()-17,32
- fBodyAcc-bandsEnergy()-33,48
- fBodyAcc-bandsEnergy()-49,64
- fBodyAcc-bandsEnergy()-1,24
- fBodyAcc-bandsEnergy()-25,48
- fBodyAcc-bandsEnergy()-1,8
- fBodyAcc-bandsEnergy()-9,16
- fBodyAcc-bandsEnergy()-17,24
- fBodyAcc-bandsEnergy()-25,32
- fBodyAcc-bandsEnergy()-33,40
- fBodyAcc-bandsEnergy()-41,48
- fBodyAcc-bandsEnergy()-49,56
- fBodyAcc-bandsEnergy()-57,64
- fBodyAcc-bandsEnergy()-1,16
- fBodyAcc-bandsEnergy()-17,32
- fBodyAcc-bandsEnergy()-33,48
- fBodyAcc-bandsEnergy()-49,64
- fBodyAcc-bandsEnergy()-1,24
- fBodyAcc-bandsEnergy()-25,48
- fBodyAccJerk-mean()-X
- fBodyAccJerk-mean()-Y
- fBodyAccJerk-mean()-Z
- fBodyAccJerk-std()-X
- fBodyAccJerk-std()-Y
- fBodyAccJerk-std()-Z
- fBodyAccJerk-mad()-X
- fBodyAccJerk-mad()-Y
- fBodyAccJerk-mad()-Z
- fBodyAccJerk-max()-X
- fBodyAccJerk-max()-Y
- fBodyAccJerk-max()-Z
- fBodyAccJerk-min()-X
- fBodyAccJerk-min()-Y
- fBodyAccJerk-min()-Z
- fBodyAccJerk-sma()
- fBodyAccJerk-energy()-X
- fBodyAccJerk-energy()-Y
- fBodyAccJerk-energy()-Z
- fBodyAccJerk-iqr()-X
- fBodyAccJerk-iqr()-Y
- fBodyAccJerk-iqr()-Z
- fBodyAccJerk-entropy()-X
- fBodyAccJerk-entropy()-Y
- fBodyAccJerk-entropy()-Z
- fBodyAccJerk-maxInds-X
- fBodyAccJerk-maxInds-Y
- fBodyAccJerk-maxInds-Z
- fBodyAccJerk-meanFreq()-X
- fBodyAccJerk-meanFreq()-Y
- fBodyAccJerk-meanFreq()-Z
- fBodyAccJerk-skewness()-X
- fBodyAccJerk-kurtosis()-X
- fBodyAccJerk-skewness()-Y
- fBodyAccJerk-kurtosis()-Y
- fBodyAccJerk-skewness()-Z
- fBodyAccJerk-kurtosis()-Z
- fBodyAccJerk-bandsEnergy()-1,8
- fBodyAccJerk-bandsEnergy()-9,16
- fBodyAccJerk-bandsEnergy()-17,24
- fBodyAccJerk-bandsEnergy()-25,32
- fBodyAccJerk-bandsEnergy()-33,40
- fBodyAccJerk-bandsEnergy()-41,48
- fBodyAccJerk-bandsEnergy()-49,56
- fBodyAccJerk-bandsEnergy()-57,64
- fBodyAccJerk-bandsEnergy()-1,16
- fBodyAccJerk-bandsEnergy()-17,32
- fBodyAccJerk-bandsEnergy()-33,48
- fBodyAccJerk-bandsEnergy()-49,64
- fBodyAccJerk-bandsEnergy()-1,24
- fBodyAccJerk-bandsEnergy()-25,48
- fBodyAccJerk-bandsEnergy()-1,8
- fBodyAccJerk-bandsEnergy()-9,16
- fBodyAccJerk-bandsEnergy()-17,24
- fBodyAccJerk-bandsEnergy()-25,32
- fBodyAccJerk-bandsEnergy()-33,40
- fBodyAccJerk-bandsEnergy()-41,48
- fBodyAccJerk-bandsEnergy()-49,56
- fBodyAccJerk-bandsEnergy()-57,64
- fBodyAccJerk-bandsEnergy()-1,16
- fBodyAccJerk-bandsEnergy()-17,32
- fBodyAccJerk-bandsEnergy()-33,48
- fBodyAccJerk-bandsEnergy()-49,64
- fBodyAccJerk-bandsEnergy()-1,24
- fBodyAccJerk-bandsEnergy()-25,48
- fBodyAccJerk-bandsEnergy()-1,8
- fBodyAccJerk-bandsEnergy()-9,16
- fBodyAccJerk-bandsEnergy()-17,24
- fBodyAccJerk-bandsEnergy()-25,32
- fBodyAccJerk-bandsEnergy()-33,40
- fBodyAccJerk-bandsEnergy()-41,48
- fBodyAccJerk-bandsEnergy()-49,56
- fBodyAccJerk-bandsEnergy()-57,64
- fBodyAccJerk-bandsEnergy()-1,16
- fBodyAccJerk-bandsEnergy()-17,32
- fBodyAccJerk-bandsEnergy()-33,48
- fBodyAccJerk-bandsEnergy()-49,64
- fBodyAccJerk-bandsEnergy()-1,24
- fBodyAccJerk-bandsEnergy()-25,48
- fBodyGyro-mean()-X
- fBodyGyro-mean()-Y
- fBodyGyro-mean()-Z
- fBodyGyro-std()-X
- fBodyGyro-std()-Y
- fBodyGyro-std()-Z
- fBodyGyro-mad()-X
- fBodyGyro-mad()-Y
- fBodyGyro-mad()-Z
- fBodyGyro-max()-X
- fBodyGyro-max()-Y
- fBodyGyro-max()-Z
- fBodyGyro-min()-X
- fBodyGyro-min()-Y
- fBodyGyro-min()-Z
- fBodyGyro-sma()
- fBodyGyro-energy()-X
- fBodyGyro-energy()-Y
- fBodyGyro-energy()-Z
- fBodyGyro-iqr()-X
- fBodyGyro-iqr()-Y
- fBodyGyro-iqr()-Z
- fBodyGyro-entropy()-X
- fBodyGyro-entropy()-Y
- fBodyGyro-entropy()-Z
- fBodyGyro-maxInds-X
- fBodyGyro-maxInds-Y
- fBodyGyro-maxInds-Z
- fBodyGyro-meanFreq()-X
- fBodyGyro-meanFreq()-Y
- fBodyGyro-meanFreq()-Z
- fBodyGyro-skewness()-X
- fBodyGyro-kurtosis()-X
- fBodyGyro-skewness()-Y
- fBodyGyro-kurtosis()-Y
- fBodyGyro-skewness()-Z
- fBodyGyro-kurtosis()-Z
- fBodyGyro-bandsEnergy()-1,8
- fBodyGyro-bandsEnergy()-9,16
- fBodyGyro-bandsEnergy()-17,24
- fBodyGyro-bandsEnergy()-25,32
- fBodyGyro-bandsEnergy()-33,40
- fBodyGyro-bandsEnergy()-41,48
- fBodyGyro-bandsEnergy()-49,56
- fBodyGyro-bandsEnergy()-57,64
- fBodyGyro-bandsEnergy()-1,16
- fBodyGyro-bandsEnergy()-17,32
- fBodyGyro-bandsEnergy()-33,48
- fBodyGyro-bandsEnergy()-49,64
- fBodyGyro-bandsEnergy()-1,24
- fBodyGyro-bandsEnergy()-25,48
- fBodyGyro-bandsEnergy()-1,8
- fBodyGyro-bandsEnergy()-9,16
- fBodyGyro-bandsEnergy()-17,24
- fBodyGyro-bandsEnergy()-25,32
- fBodyGyro-bandsEnergy()-33,40
- fBodyGyro-bandsEnergy()-41,48
- fBodyGyro-bandsEnergy()-49,56
- fBodyGyro-bandsEnergy()-57,64
- fBodyGyro-bandsEnergy()-1,16
- fBodyGyro-bandsEnergy()-17,32
- fBodyGyro-bandsEnergy()-33,48
- fBodyGyro-bandsEnergy()-49,64
- fBodyGyro-bandsEnergy()-1,24
- fBodyGyro-bandsEnergy()-25,48
- fBodyGyro-bandsEnergy()-1,8
- fBodyGyro-bandsEnergy()-9,16
- fBodyGyro-bandsEnergy()-17,24
- fBodyGyro-bandsEnergy()-25,32
- fBodyGyro-bandsEnergy()-33,40
- fBodyGyro-bandsEnergy()-41,48
- fBodyGyro-bandsEnergy()-49,56
- fBodyGyro-bandsEnergy()-57,64
- fBodyGyro-bandsEnergy()-1,16
- fBodyGyro-bandsEnergy()-17,32
- fBodyGyro-bandsEnergy()-33,48
- fBodyGyro-bandsEnergy()-49,64
- fBodyGyro-bandsEnergy()-1,24
- fBodyGyro-bandsEnergy()-25,48
- fBodyAccMag-mean()
- fBodyAccMag-std()
- fBodyAccMag-mad()
- fBodyAccMag-max()
- fBodyAccMag-min()
- fBodyAccMag-sma()
- fBodyAccMag-energy()
- fBodyAccMag-iqr()
- fBodyAccMag-entropy()
- fBodyAccMag-maxInds
- fBodyAccMag-meanFreq()
- fBodyAccMag-skewness()
- fBodyAccMag-kurtosis()
- fBodyBodyAccJerkMag-mean()
- fBodyBodyAccJerkMag-std()
- fBodyBodyAccJerkMag-mad()
- fBodyBodyAccJerkMag-max()
- fBodyBodyAccJerkMag-min()
- fBodyBodyAccJerkMag-sma()
- fBodyBodyAccJerkMag-energy()
- fBodyBodyAccJerkMag-iqr()
- fBodyBodyAccJerkMag-entropy()
- fBodyBodyAccJerkMag-maxInds
- fBodyBodyAccJerkMag-meanFreq()
- fBodyBodyAccJerkMag-skewness()
- fBodyBodyAccJerkMag-kurtosis()
- fBodyBodyGyroMag-mean()
- fBodyBodyGyroMag-std()
- fBodyBodyGyroMag-mad()
- fBodyBodyGyroMag-max()
- fBodyBodyGyroMag-min()
- fBodyBodyGyroMag-sma()
- fBodyBodyGyroMag-energy()
- fBodyBodyGyroMag-iqr()
- fBodyBodyGyroMag-entropy()
- fBodyBodyGyroMag-maxInds
- fBodyBodyGyroMag-meanFreq()
- fBodyBodyGyroMag-skewness()
- fBodyBodyGyroMag-kurtosis()
- fBodyBodyGyroJerkMag-mean()
- fBodyBodyGyroJerkMag-std()
- fBodyBodyGyroJerkMag-mad()
- fBodyBodyGyroJerkMag-max()
- fBodyBodyGyroJerkMag-min()
- fBodyBodyGyroJerkMag-sma()
- fBodyBodyGyroJerkMag-energy()
- fBodyBodyGyroJerkMag-iqr()
- fBodyBodyGyroJerkMag-entropy()
- fBodyBodyGyroJerkMag-maxInds
- fBodyBodyGyroJerkMag-meanFreq()
- fBodyBodyGyroJerkMag-skewness()
- fBodyBodyGyroJerkMag-kurtosis()
- angle(tBodyAccMean,gravity)
- angle(tBodyAccJerkMean),gravityMean)
- angle(tBodyGyroMean,gravityMean)
- angle(tBodyGyroJerkMean,gravityMean)
- angle(X,gravityMean)
- angle(Y,gravityMean)
- angle(Z,gravityMean)

