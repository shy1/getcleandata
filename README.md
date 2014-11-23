getting and cleaning data course project
============
The run_analysis.R script reads the data from the "UCI HAR Dataset" subdirectory of the R session's working directory and combines the subject, x, and y data of both the test and train data sets into one data frame. It then extracts only the mean and standard deviation columns for each measurement. It also adds subject and activity columns to the data frame while converting activity numbers to descriptive names and adds descriptive column names to the variables. 

It then creates a wide form tidy data set with the averages of each variable for each of the 6 activities for each of the 30 subjects for a total 6 * 30 = 180 rows. It then writes that data set to comma separated text file that can be easily read with the read.csv() function.