#Print the name at the top of the script
paste("Yijun Wang")

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

#Import the csv file and rename to Tvshows
df <- read.csv("C:\\Users\\junni\\Desktop\\tv_shows.csv",header= TRUE)
df

#Removing unwanted columns
df$Type <- NULL
df$X <- NULL

#string 
str(df)

#summary
summary(df)

#Remove /100 from Rotten Tomatoes and /10 from IMDb
df$Rotten.Tomatoes <- substring(df$Rotten.Tomatoes,0,2)
df$IMDb <- substring(df$IMDb,0,3)
df

#conver to number
df$IMDb= as.numeric(df$IMDb)
df$Rotten.Tomatoes= as.numeric(df$Rotten.Tomatoes)

#Age's Frequency
table <- table(df$Age)/length(df$ID)
table <- data.frame(count(df$Age),table)
names(table) <- c("Age","Total Count","Age1","Frequency")
table$Age1 <- NULL
table

#sum
sum1 <- sum(df$Netflix)
sum2 <- sum(df$Hulu)
sum3 <- sum(df$Prime.Video)
sum4 <- sum(df$Disney)

#platforms totle shows
platforms= c("Netflix","Hulu","Prime","Disney")
total_shows= c(sum1,sum2,sum3,sum4)
cbind(platforms,total_shows)

#3D PIE CHART: the Service has the highest number of the movie
slices <- c(sum1,sum2,sum3,sum4)
lbls <- c("Netflix", "Hulu", "Prime.Video", "Disney")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep= "")
colors <- c("#61799b", "#9daccb", "#ab594b", "#ffdba7")
pie3D(slices,labels= lbls,explode= 0.1,col= colors,
      main= "Pie Chart of Most Numbers of the Movie ")

#count Netflix over 90 on Rotten.Tomatoes
netflix_count <- nrow(df[df$Rotten.Tomatoes>90 & df$Netflix== 1,])
netflix_count

#count Hulu over 90 on Rotten.Tomatoes
Hulu_count <- nrow(df[df$Rotten.Tomatoes>90 & df$Hulu== 1,])
Hulu_count

#count Disney over 90 on Rotten.Tomatoes
disney_count <- nrow(df[df$Rotten.Tomatoes>90 & df$Disney.== 1,])
disney_count

#count prime.video over 90 on Rotten.Tomatoes
prime_count <- nrow(df[df$Rotten.Tomatoes>90 & df$Prime.Video== 1,])
prime_count

#Histgram of IMDb
hist(df$IMDb,col= "#D9D9D9",
     main= "Distribution of IMDb Rating for all movies",xlab= "IMDb",breaks= 7,
     ylim= c(0,2500))
grid(nx= NA, ny= NULL, lty= 2, col= "gray", lwd= 1)

#Histgram of Rotten Tomatoes
hist(df$Rotten.Tomatoes,col= "#85B4E0",
     main= "Distribution of Rotten Tomatoes for all movies",xlab= "Rotten Tomatoes",
     breaks= 7, ylim= c(0,1500))
grid(nx= NA, ny= NULL, lty= 2, col= "gray", lwd= 1)

#Plot of Age 
plot(df$Age,col= "#C1D8EA",
     main= "Distribution of Age group for all movies",xlab= "Age",breaks= 7,
     ylim= c(0,2500))
grid(nx= NA, ny= NULL, lty= 2, col= "gray", lwd= 1)

#Histgram of Year
hist(df$Year,col= "#727a93",
     main= "Distribution of yearwise for all movies",xlab= "Year",breaks= 200,
     ylim= c(0,700))
grid(nx= NA, ny= NULL, lty= 2, col= "gray", lwd= 1)

#Barplot of over 90 on Rotten.Tomatoes
names<-c("Disney","Hulu","Netflix","Prime.Video")
counts<-c(disney_count,Hulu_count, netflix_count,prime_count)
p <-barplot(counts,names.arg= names,las= 1,cex.names= 0.8, ylim= c(0,30),
            main= "Counts by Platform",col= heat.colors(5),border= "white",
            ylab= "Count over 90 on Rotten.Tomatoes",
            xlab= "Streaming Service")
cum_sums<-cumsum(counts)
lines(p, cum_sums, type= 'b', pch= 6, col= 'black')

#Boxplot of 4 Platform's year
list <-list(D$Year,H$Year,N$Year,P$Year)
boxplot(list, main= "Year Distribution By Streaming Service",
        xlab= "Streaming Service",ylab= "Year",names= names)
grid(nx= NA, ny= NULL, lty= 2, col= "gray", lwd= 1)

#Filter Netflix
N=df%>%filter(Netflix== 1,Hulu== 0,Prime.Video== 0,Disney.== 0)
#Filter Prime.Video
P=df%>%filter(Netflix== 0,Hulu== 0,Prime.Video== 1,Disney.== 0)
#Filter Hulu
H=df%>%filter(Netflix== 0,Hulu== 1,Prime.Video== 0,Disney.== 0)
#Filter Disney
D=df%>%filter(Netflix== 0,Hulu== 0,Prime.Video== 0,Disney.== 1)

#Distribution of Netflix
hist(N$IMDb,col= "#a7c2d5",
     main= "Netflix's IMDb",
     xlab= "IMDb", ylim= c(0,1000),breaks= 10)
grid(nx= NA, ny= NULL, lty= 2, col= "gray", lwd= 1)

#Distribution of prime.Video
hist(P$IMDb,col= "#49655c",
     main= "Prime.Video's IMDb",
     xlab= "IMDb", ylim= c(0,1000),breaks= 10)
grid(nx= NA, ny= NULL, lty= 2, col= "gray", lwd= 1)

#Distribution of Disney
hist(D$IMDb,col= "#dba89f",
     main= "Disney's IMDb",
     xlab= "IMDb", ylim= c(0,1000),breaks= 10)
grid(nx= NA, ny= NULL, lty= 2, col= "gray", lwd= 1)

#Distribution of Hulu
hist(H$IMDb,col= "#b4892e",
     main= "Hulu's IMDb",
     xlab= "IMDb", ylim= c(0,1000),breaks= 10)
grid(nx= NA, ny= NULL, lty= 2, col= "gray", lwd= 1)






