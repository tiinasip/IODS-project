#Starting with data reading
library(MASS)
library(tidyr)
library(dplyr)
library(stringr)
# load the data
human <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt", sep  =",", header = T)

str(human)
names(human)

#GNI string replacement and change to numeric, check that it is really numeric
human$GNI <- as.numeric(human$GNI)
str(human$GNI)

keep <- c("Country", "Edu2.FM", "Labo.FM", "Life.Exp", "Edu.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")
human <- select(human, one_of(keep))
data.frame(human[-1], comp = complete.cases(human))
# filter out all rows with NA values
human_ <- filter(human, complete.cases(human))

#taking regions out, updating rownames, check row numbero
tail(human_,10)
last <- nrow(human_) - 7
human_ <- human[1:last, ]
rownames(human_) <- human_$Country
#country names column removal
#human_<-human_[,2:9] OR like this
human_ <- select(human_, -Country)
dim(human_)

human <-human_

str(human)
