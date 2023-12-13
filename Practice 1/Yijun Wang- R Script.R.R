#print the name
print("Yijun Wang")

#Import libraries
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("tidyr")
library(tidyr)
install.packages("plotly")
library(plotly)
install.packages("plotrix")
library(plotrix)
install.packages("scales")
library(scales)
install.packages("stringr")
library(stringr)
install.packages("plyr")
library(plyr)

#Import CSV file 
census <- read.csv("C:\\Users\\junni\\Desktop\\ALY 6010\\census_data.csv")

#string the file
str(census)

#summary the file
summary(census)

#remove the column
census$education <- NULL
census$occupation <- NULL
census$capital_gain <- NULL
census$capital_loss <- NULL

#Frequency of race and rename the column
Race_table <- data.frame(table(census$race)/length(census$race))
names(Race_table) <- c("Race","Frequency")
Race_table

#Frequency of workclass and rename the column
workclass_table <- data.frame(table(census$workclass)/length(census$workclass))
names(workclass_table) <- c("Wrok Type","Frequency")
workclass_table

#remove "?" from workclass
new_workclass <- sub("?",NA,census$workclass, fixed =TRUE)

#1.PIE CHART of Workclass 
ggplot(census, aes(x=factor(1), fill=new_workclass))+
        geom_bar(width = 1)+
        coord_polar("y")

#2.Gender Distribution between age and Race 
qplot(age, data = census, facets = race ~ gender, 
      ylab= "Counts",xlab="Age", bins= 30)

#3. boxplot between relationship and age
ggplot(census, aes(x=relationship, y=age)) + 
        geom_boxplot()

#4.status~gender
qplot(marital_status, data = census, fill = gender, ylab= "Counts")+ 
        coord_flip()

#5.Male and Female's working hours each week
p <- ggplot(census, aes(x=hours_per_week, color= gender)) + 
        geom_density()
p+scale_color_manual(values=c("#999999", "#E69F00"))

#6.Histogram of Gender's income_bracket
h <- qplot(gender, data = census, fill = income_bracket, position = "dodge",las = 2)
h+scale_color_manual(values=c("#999999", "#E69F00"))+
        scale_fill_manual(values=c("#999999", "#E69F00"))+
        labs(title="Gender's Income bracket",x="Gender", y = "Count")
