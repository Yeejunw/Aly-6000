#Print the name at the top of the script
paste("Yijun Wang")

#Import libraries
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("plotly")
library(plotly)
install.packages("plotrix")
library(plotrix)
install.packages("scales")
library(scales)
install.packages("stringr")
library(stringr)

#Import the csv file and rename to Movie
df <- read.csv("C:\\Users\\junni\\Desktop\\kc_house_data.csv",header= TRUE)

df$id <- NULL
df$date <- NULL
df$sqft_living <- NULL
df$sqft_lot <- NULL
df$waterfront <- NULL
df$view <- NULL
df$condition <- NULL
df$grade <- NULL
df$yr_built <- NULL
df$yr_renovated <- NULL
df$zipcode <- NULL
df$lat <- NULL
df$long <- NULL
df$sqft_living15 <- NULL
df$sqft_lot15 <- NULL
df$sqft_above <- NULL

#String the data
str(df)

#summary the data
summary(df)

#correlation matrix
res = cor(df)
corr = round(cor(df),3)
head(corr)
ggcorrplot(corr, type = "lower",lab = TRUE)

#Price vs Sqft basement
ggplot(df, aes(x=sqft_basement, y=price, color=bathrooms))+
  geom_point()+
  ggtitle(" Price vs sqft basement on the bases of bathrooms")

qplot(bathrooms, bedrooms, data = df)

#regression table
Lm=lm(df,data=df)
Lm
summary(Lm)

