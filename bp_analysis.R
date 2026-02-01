#Loading necessary packages
#readr is used for reading csv files
install.packages("readr")
library(readr)

#Set working directory
setwd("C:/Users/hp/Downloads")

#Reading the csv file
bp<-read.csv("bp_data.csv")

#Showing first 6 rows
head(bp)

#Showing number of rows
nrow(bp)

#Showing number of columns
ncol(bp)

#Showing column names
names(bp)

#Checking structure of dataset
str(bp)

#Basic descriptive statistics of data
summary(bp)

#Checking for missing values
colSums(is.na(bp))

#Removing rows where bp_K is missing
clean_data<-na.omit(bp)

#Viewing cleaned data
head(clean_data)

#Manual descriptive statistics
mean_bp<-mean(clean_data$bp_K)
median_bp<-median(clean_data$bp_K)
sd_bp<-sd(clean_data$bp_K)

mean_bp
median_bp
sd_bp

#Histogram of boiling point
hist(clean_data$bp_K,
     main="Histogram of Boiling Points",
     xlab="Boiling Point(K)",
     col="darkred",
     border="black")

#Scatter plot between id and boiling point
plot(clean_data$id,clean_data$bp_K,
     main="Scatter Plot:ID vs Boiling Point",
     xlab="ID",
     ylab="Boiling Point(K)",
     pch=19,
     col="blue")

#Finding range
range_bp<-function(x){
  max(x)-min(x)
}

range_bp(clean_data$bp_K)

#Converting kelvin to celsius
kelvin_to_celsius<-function(K){
  K-273.15
}
clean_data$bp_C<-
  kelvin_to_celsius(clean_data$bp_K)
head(clean_data)

#Correlation analysis
cor(clean_data$id,clean_data$bp_K)

#Hypothesis testing
#Calculating if the mean boiling point significantly different from 400K?
t.test(clean_data$bp_K,mu=400)

#Export clean data
write_csv(clean_data,"clean_bp_data.csv")