#Tiina Siponen
#1.11.2019
#This file is for exercise 2

# read the data into memory
learnings <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)

#check the dimensions - found 183 rows and 60 columns (variables)
dim(learnings)

#check the structure - 60 variables:
#Aa	Ab	Ac	Ad	Ae	Af	ST01	SU02	D03	ST04	SU05	D06	D07	SU08	ST09	SU10	D11	ST12	SU13	D14	D15	SU16	ST17	SU18	D19	ST20	SU21	D22	D23	SU24	ST25	SU26	D27	ST28	SU29	D30	D31	SU32	Ca	Cb	Cc	Cd	Ce	Cf	Cg	Ch	Da	Db	Dc	Dd	De	Df	Dg	Dh	Di	Dj	Age	Attitude	Points	gender
#59 are integers, one factor (gender)
#detailed information about variables available here https://www.mv.helsinki.fi/home/kvehkala/JYTmooc/JYTOPKYS3-meta.txt
str(learnings)

#install was required before library was in use, but it is now commented as it is not necessary anymore
#install.packages("dplyr")
# access the dplyr library
library(dplyr)

#group the questions
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

#create columns from grouped questions for deep, surf and stra and calculate row average from each group for scaling
deep_columns <- select(learnings, one_of(deep_questions))
learnings$deep <- rowMeans(deep_columns)
surface_columns <- select(learnings, one_of(surface_questions))
learnings$surf <- rowMeans(surface_columns)
strategic_columns <- select(learnings,one_of(strategic_questions))
learnings$stra <-rowMeans(strategic_columns)


# choose a handful of columns to keep -- tästä tulee herja
keep_columns <- c("gender", "Age","Attitude", "deep", "stra", "surf", "Points")

# select the 'keep_columns' to create a new dataset
learningsselected <- select(learnings, one_of(keep_columns))
#structure of created data frame
str(learningsselected)

#students who have 0 points are excluded from created dataframe
learningsselected <- filter(learningsselected, Points!=0)
#checking rows and columns, are 166 and 7 as expected
dim(learningsselected)
str(learningsselected)

#working directory changed by selecting Session - Set working directory - To project directory
#also possible as setwd("~/IODS-project")
#checking how to use write.csv
#?write.csv
#write csv file to data folder in  both ways
write.csv(learningsselected, "~/IODS-project/data/learning2014.csv", sep = ",", header = TRUE)
write.table(learningsselected, "~/IODS-project/data/learning2014_table.csv", sep = ",", header = TRUE)

#check how to read csv file
#?read.csv
ReadLearningsData <-read.csv("~/IODS-project/data/learning2014.csv", header=TRUE, sep=",", header = TRUE)
#compare first 6 rows of original data and read data, 
head(ReadLearningsData)
head(learningsselected)

#str comparison as well
str(ReadLearningsData)
str(learningsselected)



