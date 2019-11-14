#Tiina Siponen
#7.11.2019
#This file is for data wrangling part of student performance and alcohol consumption

#Starting with data reading
math <- read.csv("~/IODS-project/data/student-mat.csv", sep = ";" , header=TRUE)
por <- read.csv("~/IODS-project/data/student-por.csv", sep = ";" , header=TRUE)

#checking the dimension
dim(math)
dim(por)

#checking the structure
str(math)
str(por)

# access the dplyr library
library(dplyr)

# common columns to use as identifiers
join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

# join the two datasets by the selected identifiers
math_por <- inner_join(math, por, by = join_by, suffix =c(".math",".por"))

# see the new column names
colnames(math_por)

#checking the structure and dimension
dim(math_por)
str(math_por)

# glimpse at the data
glimpse(math_por)

# copy the solution from Data Camp as instructed
# create a new data frame with only the joined columns
alc <- select(math_por, one_of(join_by))
# the columns in the datasets which were not used for joining the data
notjoined_columns <- colnames(math)[!colnames(math) %in% join_by]

# for every column name not used for joining...
for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}
# glimpse at the data
glimpse(math_por)

# access the 'tidyverse' packages ggplot2
library(ggplot2)

# define a new column alc_use by combining weekday and weekend alcohol use
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

# define a new logical column 'high_use'
alc <- mutate(alc, high_use = alc_use > 2)

#should be 382 rows and 35 variables, check that by using glimpse
glimpse(alc)

#data is saved to data folder
setwd("~/IODS-project")
write.table(alc, file="~/IODS-project/data/alc.csv", sep = ",")

