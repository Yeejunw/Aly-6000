#Print the name at the top of the script
paste("Yijun Wang")

#Import libraries
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("tidyr")
library(tidyr)
install.packages("tidyverse")
library(tidyverse)
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
install.packages("jmv")
library(jmv)
install.packages("table1")
library(table1)

#Import the csv file and rename to Movie
df <- read.csv("C:\\Users\\junni\\Desktop\\travel insurance.csv",header= TRUE)

#dim
dim(df)

#string 
str(df) 

#summary
summary(df)

#remove unneed column
df$Product.Name <- NULL
df$Duration <- NULL

#select the column need
new_df <- df %>% select(6,8,11)
new_df

#SD for new file
new_df %>% summarise_if(is.numeric, sd)

#mean for new file
new_df %>% summarise_if(is.numeric, mean)

#Min for new file
new_df %>% summarise_if(is.numeric, min)

#Max for new file
new_df %>% summarise_if(is.numeric, max)

#Three Line Table
table1::label(df$Duration) <- "Duration"
table1::label(df$Net.Sales) <- "Net Sales"
table1::label(df$Age) <- "Age"

table1::table1(~Duration + Net.Sales + Age, data=df)

#Boxplot: Age~Gender
qplot(Age, Gender, data = df, geom = "boxplot")

#scappter plot: Agency~Age
ggplot(df, aes(x=Agency,y=Age))+ 
  geom_point()+
  ggtitle("Different Age on Platforms")+
  geom_abline() 

#Jitter plot for Poplular(Agency~Gender)
qplot(Agency, Gender, data = df, geom = c("boxplot", "jitter"), alpha = I(1/5))+
  xlab("Agency")+
  ylab("Gender")
