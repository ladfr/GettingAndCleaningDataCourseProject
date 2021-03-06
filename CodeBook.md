---
title: "CodeBook"
author: "TF"
date: "5/2/2021"
output: html_document
---

## Base data set

The base data set is the 'Human Activity Recognition Using Smartphones Dataset
Version 1.0'
See the files README.txt, features_info.txt, features.txt and activity_labels.txt in the UCI HAR Dataset subdirectory.

## Dataset x_tbl_mean_std_rc: Measurements on the mean and standard deviation for each measurement
Size: 10,299 x 81

# Variables
timebodyaccelerationmeanx	numeric     
timebodyaccelerationmeany	numeric     
timebodyaccelerationmeanz	numeric     
timebodyaccelerationstdx	numeric     
timebodyaccelerationstdy	numeric     
timebodyaccelerationstdz	numeric     
timegravityaccelerationmeanx	numeric     
timegravityaccelerationmeany	numeric     
timegravityaccelerationmeanz	numeric     
timegravityaccelerationstdx	numeric     
timegravityaccelerationstdy	numeric     
timegravityaccelerationstdz	numeric     
timebodyaccelerationjerkmeanx	numeric     
timebodyaccelerationjerkmeany	numeric     
timebodyaccelerationjerkmeanz	numeric     
timebodyaccelerationjerkstdx	numeric     
timebodyaccelerationjerkstdy	numeric     
timebodyaccelerationjerkstdz	numeric     
timebodygyromeanx	numeric     
timebodygyromeany	numeric     
timebodygyromeanz	numeric     
timebodygyrostdx	numeric     
timebodygyrostdy	numeric     
timebodygyrostdz	numeric     
timebodygyrojerkmeanx	numeric     
timebodygyrojerkmeany	numeric     
timebodygyrojerkmeanz	numeric     
timebodygyrojerkstdx	numeric     
timebodygyrojerkstdy	numeric     
timebodygyrojerkstdz	numeric     
timebodyaccelerationmagnitudemean	numeric     
timebodyaccelerationmagnitudestd	numeric     
timegravityaccelerationmagnitudemean	numeric     
timegravityaccelerationmagnitudestd	numeric     
timebodyaccelerationjerkmagnitudemean	numeric     
timebodyaccelerationjerkmagnitudestd	numeric     
timebodygyromagnitudemean	numeric     
timebodygyromagnitudestd	numeric     
timebodygyrojerkmagnitudemean	numeric     
timebodygyrojerkmagnitudestd	numeric     
frequncybodyaccelerationmeanx	numeric     
frequncybodyaccelerationmeany	numeric     
frequncybodyaccelerationmeanz	numeric     
frequncybodyaccelerationstdx	numeric     
frequncybodyaccelerationstdy	numeric     
frequncybodyaccelerationstdz	numeric     
frequncybodyaccelerationmeanfrequencyx	numeric     
frequncybodyaccelerationmeanfrequencyy	numeric     
frequncybodyaccelerationmeanfrequencyz	numeric     
frequncybodyaccelerationjerkmeanx	numeric     
frequncybodyaccelerationjerkmeany	numeric     
frequncybodyaccelerationjerkmeanz	numeric     
frequncybodyaccelerationjerkstdx	numeric     
frequncybodyaccelerationjerkstdy	numeric     
frequncybodyaccelerationjerkstdz	numeric     
frequncybodyaccelerationjerkmeanfrequencyx	numeric     
frequncybodyaccelerationjerkmeanfrequencyy	numeric     
frequncybodyaccelerationjerkmeanfrequencyz	numeric     
frequncybodygyromeanx	numeric     
frequncybodygyromeany	numeric     
frequncybodygyromeanz	numeric     
frequncybodygyrostdx	numeric     
frequncybodygyrostdy	numeric     
frequncybodygyrostdz	numeric     
frequncybodygyromeanfrequencyx	numeric     
frequncybodygyromeanfrequencyy	numeric     
frequncybodygyromeanfrequencyz	numeric     
frequncybodyaccelerationmagnitudemean	numeric     
frequncybodyaccelerationmagnitudestd	numeric     
frequncybodyaccelerationmagnitudemeanfrequency	numeric     
frequncybodybodyaccelerationjerkmagnitudemean	numeric     
frequncybodybodyaccelerationjerkmagnitudestd	numeric     
frequncybodybodyaccelerationjerkmagnitudemeanfrequency	numeric     
frequncybodybodygyromagnitudemean	numeric     
frequncybodybodygyromagnitudestd	numeric     
frequncybodybodygyromagnitudemeanfrequency	numeric     
frequncybodybodygyrojerkmagnitudemean	numeric     
frequncybodybodygyrojerkmagnitudestd	numeric     
frequncybodybodygyrojerkmagnitudemeanfrequency	numeric     
subject	integer   
activity	character  

The values of the numeric variables are between -1 and 1
The values of subject are: 1,2,3,4, ... ,28,29,30
The values of activity are: WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS,  SITTING, STANDING, LAYING


## Dataset x_grouped_means: Average of each variable for each activity and each subject 
Size: 180 x 81
Grouped by: subject, integer

# Variables
subject	integer   	
activity	character   
meantimebodyaccelerationmeanx	numeric   
meantimebodyaccelerationmeany	numeric   
meantimebodyaccelerationmeanz	numeric   
meantimebodyaccelerationstdx	numeric   
meantimebodyaccelerationstdy	numeric   
meantimebodyaccelerationstdz	numeric   
meantimegravityaccelerationmeanx	numeric   
meantimegravityaccelerationmeany	numeric   
meantimegravityaccelerationmeanz	numeric    
meantimegravityaccelerationstdx	numeric    
meantimegravityaccelerationstdy	numeric    
meantimegravityaccelerationstdz	numeric    
meantimebodyaccelerationjerkmeanx	numeric    
meantimebodyaccelerationjerkmeany	numeric    
meantimebodyaccelerationjerkmeanz	numeric    
meantimebodyaccelerationjerkstdx	numeric    
meantimebodyaccelerationjerkstdy	numeric    
meantimebodyaccelerationjerkstdz	numeric    
meantimebodygyromeanx	numeric    
meantimebodygyromeany	numeric    
meantimebodygyromeanz	numeric    
meantimebodygyrostdx	numeric    
meantimebodygyrostdy	numeric    
meantimebodygyrostdz	numeric     
meantimebodygyrojerkmeanx	numeric    
meantimebodygyrojerkmeany	numeric    
meantimebodygyrojerkmeanz	numeric    
meantimebodygyrojerkstdx	numeric    
meantimebodygyrojerkstdy	numeric    
meantimebodygyrojerkstdz	numeric    
meantimebodyaccelerationmagnitudemean	numeric    
meantimebodyaccelerationmagnitudestd	numeric    
meantimegravityaccelerationmagnitudemean	numeric    
meantimegravityaccelerationmagnitudestd	numeric    
meantimebodyaccelerationjerkmagnitudemean	numeric    
meantimebodyaccelerationjerkmagnitudestd	numeric    
meantimebodygyromagnitudemean	numeric    
meantimebodygyromagnitudestd	numeric    
meantimebodygyrojerkmagnitudemean	numeric    
meantimebodygyrojerkmagnitudestd	numeric    
meanfrequncybodyaccelerationmeanx	numeric    
meanfrequncybodyaccelerationmeany	numeric    
meanfrequncybodyaccelerationmeanz	numeric    
meanfrequncybodyaccelerationstdx	numeric    
meanfrequncybodyaccelerationstdy	numeric    
meanfrequncybodyaccelerationstdz	numeric    
meanfrequncybodyaccelerationmeanfrequencyx	numeric    
meanfrequncybodyaccelerationmeanfrequencyy	numeric    
meanfrequncybodyaccelerationmeanfrequencyz	numeric    
meanfrequncybodyaccelerationjerkmeanx	numeric    
meanfrequncybodyaccelerationjerkmeany	numeric    
meanfrequncybodyaccelerationjerkmeanz	numeric    
meanfrequncybodyaccelerationjerkstdx	numeric    
meanfrequncybodyaccelerationjerkstdy	numeric    
meanfrequncybodyaccelerationjerkstdz	numeric    
meanfrequncybodyaccelerationjerkmeanfrequencyx	numeric    
meanfrequncybodyaccelerationjerkmeanfrequencyy	numeric    
meanfrequncybodyaccelerationjerkmeanfrequencyz	numeric    
meanfrequncybodygyromeanx	numeric    
meanfrequncybodygyromeany	numeric    
meanfrequncybodygyromeanz	numeric    
meanfrequncybodygyrostdx	numeric    
meanfrequncybodygyrostdy	numeric    
meanfrequncybodygyrostdz	numeric    
meanfrequncybodygyromeanfrequencyx	numeric    
meanfrequncybodygyromeanfrequencyy	numeric    
meanfrequncybodygyromeanfrequencyz	numeric    
meanfrequncybodyaccelerationmagnitudemean	numeric    
meanfrequncybodyaccelerationmagnitudestd	numeric    
meanfrequncybodyaccelerationmagnitudemeanfrequency	numeric    
meanfrequncybodybodyaccelerationjerkmagnitudemean	numeric    
meanfrequncybodybodyaccelerationjerkmagnitudestd	numeric    
meanfrequncybodybodyaccelerationjerkmagnitudemeanfrequency	numeric    
meanfrequncybodybodygyromagnitudemean	numeric    
meanfrequncybodybodygyromagnitudestd	numeric    
meanfrequncybodybodygyromagnitudemeanfrequency	numeric    
meanfrequncybodybodygyrojerkmagnitudemean	numeric    
meanfrequncybodybodygyrojerkmagnitudestd	numeric    
meanfrequncybodybodygyrojerkmagnitudemeanfrequency	numeric    

The values of the numeric variables are between -1 and 1
The values of subject are: 1,2,3,4, ... ,28,29,30
The values of activity are: WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS,  SITTING, STANDING, LAYING