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
names(hd)[names(hd) == "Life.Expectancy.at.Birth"] <- "LEB"
names(hd)[names(hd) == "Mean.Years.of.Education"] <- "MYE"
names(hd)[names(hd) == "Gender.Inequality.Index..GII."] <- "GII"
names(hd)[names(hd) == "Maternal.Mortality.Ratio"] <- "MMR"
names(hd)[names(hd) == "Percent.Representation.in.Parliament"] <- "PerParliament"
names(hd)[names(hd) == "Population.with.Secondary.Education..Female."] <- "SecEducFemal"
names(hd)[names(hd) == "Gross.National.Income..GNI..per.Capita"] <- "GNIncPerCap"
names(hd)[names(hd) == "GNI.per.Capita.Rank.Minus.HDI.Rank"] <- "GNIMinusHDIRank"
names(gii)[names(gii) == "Adolescent.Birth.Rate"] <- "ADBR"
names(gii)[names(gii) == "Expected.Years.of.Education"] <- "EYE"
names(gii)[names(gii) == "Life.Expectancy.at.Birth"] <- "LEB"
names(gii)[names(gii) == "Mean.Years.of.Education"] <- "MYE"
names(gii)[names(gii) == "Gender.Inequality.Index..GII."] <- "GII"
names(gii)[names(gii) == "Maternal.Mortality.Ratio"] <- "MMR"
names(gii)[names(gii) == "Percent.Representation.in.Parliament"] <- "PerParliament"
names(gii)[names(gii) == "Population.with.Secondary.Education..Male."] <- "SecEducMale"
names(gii)[names(gii) == "Population.with.Secondary.Education..Female."] <- "SecEducFemal"
names(gii)[names(gii) == "Gross.National.Income..GNI..per.Capita"] <- "GNIncPerCap"
names(gii)[names(gii) == "Labour.Force.Participation.Rate..Female."] <- "LabForParFem"
names(gii)[names(gii) == "Labour.Force.Participation.Rate..Male."] <- "LabForParMale"
head(hd)
head(gii)

