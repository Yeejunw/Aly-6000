#Print the name at the top of the script
paste("Yijun Wang")

install.packages("tidyverse")
library(tidyverse)
install.packages("MASS")
library(MASS)

#import the data
data("cats")

#1. do male and female cat samples have the same bodyweight ("Bwt")? 
male <- subset(cats, subset=(cats$Sex=="M"))
male
summary(male)

female<-subset(cats,subset=(cats$Sex=="F"))
female
summary(female)

#t-test
t.test(male$Bwt, female$Bwt,alternative="two.sided", Var.equal=TRUE)

#2. The researchers claimed that meditation improves sleeping quality. Is it true?
average_before <- c(4.6, 7.8, 9.1, 5.6, 6.9, 8.5, 5.3, 7.1, 3.2, 4.4)
average_after <- c(6.6, 7.7, 9.0, 6.2, 7.8, 8.3, 5.9, 6.5, 5.8, 4.9)

#t-test
t.test(average_before,average_after,conf.level = 0.95)