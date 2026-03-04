# Import dataset player and library
library(readr)
dfplayers<-read.csv("C:/Users/aqilr/Downloads/players.csv")

# Handle outliers in the "Age” 
median_age <- median(dfplayers$Age, na.rm =TRUE)
dfplayers$Age[dfplayers$Age<18 | dfplayers$Age>38]<-median_age

# Using Boxplot to detect Outlier
data<-c(30,24,26,28,29,28,27,26,32,34,13,15,14,31,29,28,24,25,30,34,35,27,30,34,44,48)
boxplot(data, main = "Boxplot")

# Calculate quantile values at 25% and 75%
first_q<-quantile(data,0.25) #this is 26
third_q<-quantile(data,0.75) #this is 31.75

# Calculate Interquantile range
iqr<-IQR(data) #this produces 5.75

# Calculate the upper and lower extremes
le<-first_q - 1.5 * iqr #this produces 17.375
ue<-third_q + 1.5 * iqr #this produces 40.375

# Manage the values below than le and above the ue
data_new<-data
data_new <- data_new[!data_new<le]
data_new <- data_new[!data_new>ue]
data_new