#1. Print the name at the tip of the script
paste("Yijun Wang")

#Import the libraries
install.packages("FSA")
library(FSA)
install.packages("FSAdata")
library(FSAdata)
install.packages("magrittr")
library(magrittr)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("plyr")
library(plyr)
install.packages("tidyverse")
library(tidyverse)

#2. Import the inchBio.csv and name the table<bio>
bio <- read.csv("C:\\Users\\junni\\Downloads\\inchBio.csv",header=TRUE,sep=",")
bio

#3. display the head,tail and structure
head(bio,n=3)
tail(bio,n=3)
str(bio)

#4. create an object<counts>
count(bio[1:677],vars="species")
counts <- count(bio$species)
counts
        
#5. display 8 level names
counts$freq<- NULL
counts

#6. create<tmp> display the different species and the number of record of each
tmp <- table(bio$species)
tmp <- (data.frame(tmp))
tmp

#7. Create a subset<tmp2>, species variable and display the first five records
tmp2 <- subset(bio,select = species)
head(tmp2,5)

#8. Create a table <w>, species variable.class w
w <- table(bio$species)
w
class(w)

#9. Convert <w> to a data frame named <t> and display the results
t <- (data.frame(w))
t

#10. Extract and display the frequency values from the <t> data frame
print(t$Freq)

#11. create a table <cSpec>
cSpec <- table(bio$species)
cSpec

#12. create a table <cSpecPct> and class
#&13. <cSpecPct> to data frame named <u>.
install.packages(scales)
library(scales)
cSpecPct <- table(bio$species)/676
u <- (data.frame(cSpecPct))
u
class(cSpecPct)
#13. confirm <u> is data frame
class(u)

#14.create barplot <cSpec>
barplot(cSpec, ylab="COUNTS", main = "Fish Count",
        col = "Light Green", las = 2, cex.names = 0.6)


#15. create barplot <cSpecPct>
barplot(cSpecPct, ylim=c(0,.4),col="light blue",las=2, yaxt="none",
        main= "Fish Relative Frequency",ylab="Frequency")
axis(2, col.axis="Light Blue")

#16.Rearrange the <u> cSpecPct data frame in descending order of relative frequency. 
#Save the rearranged data frame as the object <d>
d = arrange(u,desc(Freq))
d

#17. Rename the <d> columns Var 1 to Species, and Freq to RelFreq
names(d) <- c("Species", "RelFreq")
d

#18.Add new variables to <d> and call them cumfreq, counts, and cumcounts
d<- mutate(d, cumfreq= cumsum(RelFreq),
           counts= RelFreq*676,
           cumcounts=cumsum(counts))
d

#19. Create a parameter variable <def_par> to store parameter variables
def_par=par()
par(mar=c(8,5,2,5))

#20.Create a barplot <pc>
pc <- barplot(d$counts, width=1, space=0.15, boarder=NA, axes=F, 
              ylim = c(0, 3.05 * max(d$counts, na.rm = TRUE)),
              ylab = "Cumulative Counts",cex.axis = 1.7,
              names.arg = d$Species,
              main= "Species Pareto(By Yijun Wang)", las=2)
pc

#21. Add a cumulative counts line to the <pc> plot
lines(pc, d$cumcounts, type= 'b', pch= 19, col= 'cyan4')

#22. Place a grey box around the pareto plot
box(col = 'grey62')

#23.Add a left side axis
axis(side= 2, at= c(0, d$cumcounts), 
     tick = TRUE ,line = NA,
     col.ticks = "grey62",
     col= "grey62", cex.axis=0.8, las=2)

#24. Add axis details on right side of box
axis(side= 4, at = c(0, d$cumcounts), 
     col= "cyan4", cex.axis= 0.8, las= 2, tick= TRUE,
     line= NA, col.axis= "cyan4", 
     labels = paste0(round( c(0,d$cumfreq) * 100,digits = 0),'%'))

#25.display the name
