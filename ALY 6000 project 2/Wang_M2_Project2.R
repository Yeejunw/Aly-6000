#Print the name at the top of the script
paste("Plotting Basics: Wang")

#Import multiple libraries
install.packages("FSA")
library(FSA)
install.packages("FSAdata")
library(FSAdata)
install.packages("magrittr")
library(magrittr)
install.packages("dplyr")
library(dplyr)
install.packages("plotrix")
library(plotrix)
install.packages("ggplot2")
library(ggplot2)
install.packages("moments")
library(moments)

#load dataset
dataset <- BullTroutRML2
dataset

#Print first records and last 3 records
head(dataset, 1)
tail(dataset,3)

#remove all records except Harrison Lake
BullTroutRML2
Lakefilter<- c(filterD(BullTroutRML2,lake == "Harrison"))
Lakefilter<- (data.frame(Lakefilter))
Lakefilter

#the first and last 5 records
head(Lakefilter, 1)
tail(Lakefilter,5)

#structure of the filtered
str(Lakefilter)

#summary of the filtered
summary(Lakefilter)


#Plot 1: scatterplot for "age" and "fl"
attach(Lakefilter)
plot(fl,age,main="Plot1:Harrison Lake Trout",
     xlab="Fork Length(mm)",ylab="Age(yrs)",pch=16,xlim=c(0,500),ylim=c(0,15))

#Plot 2: "Age" historgram
hist(age,main="Plot2: Harrison Fish Age Distribution",
     xlab="Age(yrs)",ylab="Frequency",
     xlim=c(0,15),ylim=c(0,15),
     col="cadetblue",col.main="cadetblue")

#Plot 3: Overdense plot
attach(Lakefilter)
smoothScatter(fl,age,main="Plot3:Harrison Density Shaded by Era",
              xlab="Fork Length(mm)",
              ylab="Age(yrs)",
              xlim=c(0,500),ylim=c(0,15),pch=16,col="green")

#create new "tmp" object
tmp<- headtail(dataset, n=3)
tmp

#Display the "era"column
era<- tmp[,c("era")]
era<- (data.frame(era))
era

#create a pchs
pchs <- c("+","*")

#create a cols
cols<- c("red","gray60")
cols

#convert the tmp era values to numberic values
num<- as.numeric(era)
num

#initialize the cols vector with tmp era values
colors_num <-cols[num]
colors_num

#plot 4
all_cols<- cols(Lakefilter$era)
all_cols
all_pchs<- pchs(Lakefilter$era)
all_pchs
attach(Lakefilter)
plot(fl, age,main="Plot4:Symbol& Color by Era",
     xlab="Fork Length(mm)",ylab="Age(yrs)",xlim=c(0,500),ylim=c(0,15),
     col=all_cols,pch=all_pchs)

#Plot 5: regression line
all_cols<- cols(Lakefilter$era)
all_cols
all_pchs<- pchs(Lakefilter$era)
all_pchs
attach(Lakefilter)
plot(fl, age,main="Plot 5: Regression Overlay",
     xlab="Fork Length(mm)",ylab="Age(yrs)",xlim=c(0,500),ylim=c(0,15),
     col=all_cols,pch=all_pchs)
reg_model<- lm(age~fl,data=Lakefilter)
abline(reg_model,col = "green")

#Plot 6: legend of plot 6
all_cols<- cols(Lakefilter$era)
all_cols
all_pchs<- pchs(Lakefilter$era)
all_pchs
attach(Lakefilter)
plot(fl, age,main="Plot 6: Legend Overlay",
     xlab="Fork Length(mm)",ylab="Age(yrs)",xlim=c(0,500),ylim=c(0,15),
     col=all_cols,pch=all_pchs)
reg_model<- lm(age~fl,data=Lakefilter)
abline(reg_model,col = "green")
legend(2,15,c("1977-80","1997-01"),pch=pchs,col=cols,box.lty=0)
