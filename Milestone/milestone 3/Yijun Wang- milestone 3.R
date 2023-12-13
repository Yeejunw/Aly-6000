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
install.packages("ggcorrplot")
library(ggcorrplot)

#Import the csv file and rename to Movie
df <- read.csv("C:\\Users\\junni\\Desktop\\movies.csv",header= TRUE)

#dim
dim(df)

#string 
str(df)

#Remove /100 from Rotten Tomatoes and /10 from IMDb
df$Rotten.Tomatoes <- substring(df$Rotten.Tomatoes,0,2)
df$IMDb <- substring(df$IMDb,0,3)
head(df)

#conver to number
df$IMDb= as.numeric(df$IMDb)
df$Rotten.Tomatoes= as.numeric(df$Rotten.Tomatoes)
class(df$IMDb)
class(df$Rotten.Tomatoes )

#summary
summary(df)

#correlation matrix of Rotten.Tomatoes and runtime
numdata <- df[, c(7,8,9,10,11,17)]
corr <- round(cor(numdata), 3)
ggcorrplot(corr, method = "circle",lab = TRUE)

#correlation matrix of Rotten.Tomatoes and year
numdata <- df[, c(4,8,9,10,11)]
corr <- round(cor(numdata), 3)
ggcorrplot(corr, method = "circle",lab = TRUE)

#1. which platforms has the highest number of Movies titles
sum1 <- sum(df$Netflix)
sum2 <- sum(df$Hulu)
sum3 <- sum(df$Prime.Video)
sum4 <- sum(df$Disney.)

slices <- c(sum1,sum2,sum3,sum4)
lbls <- c("Netflix", "Hulu", "Prime.Video", "Disney")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep= "")
colors <- c("#61799b", "#9daccb", "#ab594b", "#ffdba7")
pie3D(slices,labels= lbls,explode= 0.1,col= colors,
      main= "Pie Chart of Platforms ")

#Changing the data from wide to longer format
df <- gather(df, Service, Value, Netflix:Disney.)%>%
  filter(Value == 1)%>%
  select(-Value)

#2. How much is played on various platforms at various ages?
df$Age[df$Age==""] <- NA
ggplot(data = df, aes(x = Age)) +
  ggtitle("Different Age on Platforms")+
  geom_bar(aes(fill = Service), position = "dodge")+ 
  scale_fill_manual(values=c("#999999", "#E69F00","#ab594b","#61799b"))
  

#3. histogra of Rotten.Tomatioes on different Platforms
ggplot(df, aes(x=Rotten.Tomatoes))+
  geom_histogram(color="black", fill="white",bins = 30)+
  facet_grid(Service ~ .)+
  ggtitle("Rotten.Tomatioes on Platforms")

#4.histogra of IMDb on different Platforms
ggplot(df, aes(x=IMDb))+
   geom_histogram(color="black", fill="white",bins = 30)+
   facet_grid(Service ~ .)+
   ggtitle("IMDb on Platforms")

#5. Year
g<- ggplot(df, aes(x=Year, color=Service)) +
  geom_density()+
  labs(title="Weight density curve",x="Weight(kg)", y = "Density")
g + scale_color_manual(values=c("#999999", "#E69F00", "#ab594b","#c9e7ff"))+
  theme_minimal()

#Part 2
#a. The mean of Rotten.Tomatoes is greater equal to 55 or less then 55
t.test(df$Rotten.Tomatoes,alternative="two.sided", Var.equal=TRUE,conf.level = 0.95)

#b. "16+"&"all"has any difference on Rotten.Tomatoes score?
new_df <- df %>% select(5,7)
new_df = data.frame(new_df)
head(new_df)

A <- subset(new_df, subset=(new_df$Age=="16+"))
head(A)
B <- subset(new_df, subset=(new_df$Age=="all"))
head(B)
t.test(A$Rotten.Tomatoes, B$Rotten.Tomatoes,alternative="two.sided", Var.equal=TRUE,conf.level = 0.95)

#Part 3: Final Report
#distribution of Rotten.Tomatoes
hist(df$Rotten.Tomatoes)
qplot(Rotten.Tomatoes, data = df)

#correlation matrix of Rotten.Tomatoes and runtime
qplot(Runtime, Rotten.Tomatoes, data = df, geom = c("point", "smooth"), method = "lm")

#correlation matrix of Rotten.Tomatoes and runtime from each service
ggplot(df, aes(x=Runtime, y=Rotten.Tomatoes, color=Service))+
  geom_point()+
  ggtitle(" distributed of Rotten.Tomatoes and Runtime")+
  geom_smooth(method=lm)

#regression table of Rotten.Tomatoes and runtime
runtime_Lm=lm(df$Rotten.Tomatoes~df$Runtime,data=df)
runtime_Lm
summary(runtime_Lm)

#correlation matrix of Rotten.Tomatoes and year
qplot(Year, Rotten.Tomatoes, data = df, geom = c("point", "smooth"), method = "lm")

#correlation matrix of Rotten.Tomatoes and runtime from each service
ggplot(df, aes(x=Year, y=Rotten.Tomatoes, color=Service))+
  geom_point()+
  ggtitle(" distributed of Rotten.Tomatoes and Runtime")+
  geom_smooth(method=lm)

#regression table of Rotten.Tomatoes and year
year_Lm=lm(df$Rotten.Tomatoes~df$Year,data=df)
year_Lm
summary(year_Lm)


