#Print the name at the top of the script
paste("Yijun Wang")

#Import libraries
install.packages("dplyr")
library(dplyr)
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

#import the file
df <- read.csv("C:\\Users\\junni\\Desktop\\census_data.CSV",header= TRUE)

#dim
dim(df)

#string 
str(df)

#H0:mu >45
#Ha:mu <45

#select the column need
new_df <- df %>% select(12)
new_df

#summary
summary(new_df)

#mean for new file
new_df %>% summarise_if(is.numeric, mean)

#SD for new file
new_df %>% summarise_if(is.numeric, sd)

#ttest
t.test(new_df, alternative="two.sided", mu = 35, conf.level = 0.95)


xbar = 40.43746             # sample mean 
mu0 = 45                # hypothesized value 
sigma = 12.34743           # population standard deviation 
n = 200                 # sample size 
z = (xbar-mu0)/(sigma/sqrt(n)) 
z                      # test statistic 

alpha = .05 
z.alpha = qt(1-alpha,df=n-1)
-z.alpha            # critical value 


pval = pt(z,df=n-1)
pval
