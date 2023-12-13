#Print the name at the tip of the script
paste("Yijun Wang")

#Install the vcd package
help.start()
install.packages("vcd")
help(package="vcd")

#Install the vcd library
library(vcd)
help (Arthritis)
Arthritis
example(Arthritis)
#q()

#Plot sales&temperature data
sales <- c(8,11,15,20,21,11,18,10,6,22)
temperature <- c(69,80,77,84,80,77,87,70,65,90)
plot(sales, temperature)

#The mean of temperature
mean(temperature)
#Delete the 3rd element from the sales vector
indices <- c(3)
result <- sales[-indices]
print(result)
#Insert 16 as the 3rd element
sales <- c(result[1:2],16,result[3:9])
sales

#create a vector names
names <- c("Tom", "Dick", "Harry")
names
#5 row and 2 column matrix of 10 int
names <- matrix(1:10, nrow=5, ncol=2)
names

#<icSales> with sales and temp attributes
icSales <- data.frame(sales, temperature)
icSales

#data fram structure of icScales
str(icSales)
#summary of icScales data frame
summary(icSales)

#import the dataset Student.csv
read.csv("C:\\Users\\junni\\Downloads\\Student.csv",header=TRUE,sep=",")

#Variable names of the Student.csv dataset
colnames(read.csv("C:\\Users\\junni\\Downloads\\Student.csv"))



