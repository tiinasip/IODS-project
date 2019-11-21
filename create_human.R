#Tiina Siponen
#18.11.2019
#This file is for data wrangling part of human development and gender inequality

#Starting with data reading
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

#checking the dimension
dim(hd)
dim(gii)

#checking the structure
str(hd)
str(gii)
summary(hd)
summary(gii)

#library(tidyverse)
#library(dplyr)

#renaming
names(hd)[names(hd) == "Human.Development.Index..HDI."] <- "HDI"
names(hd)[names(hd) == "Expected.Years.of.Education"] <- "EYE"
head(hd)

