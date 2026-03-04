# import dataset of student & libraries
library(dplyr)
library(readr)
student_data<- read.csv("C:/Users/aqilr/Downloads/student_data.csv")
# Use filter for student_fail
Student_fail<- student_data %>% filter(final_exam_mark< 40)
View(Student_fail)

summary(student_data) # summarize
head(student_data) # display first 6 rows
tail(student_data) # display last 6 rows

# Arrange Student Data
mydata <- student_data %>% filter (final_exam_mark > 40) %>% arrange (final_exam_mark)
View(mydata)

# Select
mydata <- student_data%>% select(student_id,coursework_mark, final_exam_mark)
View(mydata)

glimpse(mydata)

# Mutate to create 2 new variables
mydata = student_data%>% mutate(Total_Mark=(coursework_mark + final_exam_mark/200*100))

data <- iris
View(data)
str(data) # describe the structure of the data
summary(data)

A<-c(170.2, 181.5, 188.9, 163.9, 166.4, 163.7, 160.4, 175.8, 181.5)
quantile(A)
sort(A)

quantile(A,0.25)
quantile(A,0.75)
IQR(A) # calculates the interquartile range

# Histogram of Sepal Length
hist(iris$Sepal.Length,
     main = "Histogram of Sepal Length",
     xlab = "Sepal Length (cm)",
     ylab = "Frequency",
     col = 'Lightblue',
     border = "black")

# Boxplot for Iris 
boxplot(Sepal.Length ~ Species,
        data = iris,
        main = "Sepal Length by Species",
        xlab = "Species",
        ylab = "Sepal Length (cm)",
        col = c("lightgreen", "lightpink", "lightyellow"))

#Scatter Plot
plot(iris$Sepal.Length, iris$Petal.Length,
     main = "Sepal Length vs Petal Length",
     xlab = "Sepal Length (cm)",
     ylab = "Petal Length (cm)",
     col = as.numeric(iris$Species),
     pch = 19)