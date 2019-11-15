#analysis part starts here, to be sure the data is read from the instructed location
#check the dimensions
alc <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/alc.txt", sep=",", header=TRUE)
dim(alc)
str(alc)
summary(alc)

#tidyverse installation
#install.packages("tidyr")


# access the dplyr library
library(dplyr)
library(tidyr)
library(ggplot2)
# gather columns into key-value pairs and then glimpse() at the resulting data
gather(alc) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free")+geom_bar()
glimpse(alc)

#summary statistics of chosen variable
library(dplyr) 
library(ggplot2)
library(GGally)
alc %>% group_by(sex, high_use) %>% summarise(count = n(),mean_grade=mean(G3))
alc %>% group_by(higher, high_use) %>% summarise(count = n(),mean_grade=mean(G3))
alc %>% group_by(romantic, high_use) %>% summarise(count = n(),mean_grade=mean(G3))
alc %>% group_by(studytime, high_use) %>% summarise(count = n(),mean_grade=mean(G3))
alc %>% group_by(activities, high_use) %>% summarise(count = n(),mean_grade=mean(G3))


#box-plots
alc_gender <- ggplot(alc, aes(x = high_use, y = G3, col=sex))
alc_gender + geom_boxplot(outlier.colour="green") + ylab("grade")+ggtitle("Student grades by alcohol consumption and sex, outliers green")

alc_higher <- ggplot(alc, aes(x = high_use, y = G3, col=higher))
alc_higher + geom_boxplot() + ylab("grade")+ggtitle("Student grades by alcohol consumption and higher education")

alc_romantic <- ggplot(alc, aes(x = high_use, y = G3, col=romantic))
alc_romantic + geom_boxplot() + ylab("grade")+ggtitle("Student grades by alcohol consumption and romantic relationship")

#alc$studytime <-as.factor(alc$studytime) -- no need to do factoring, because studytime is already int
alc_studytime <- ggplot(alc, aes(x = studytime, y = G3, col=high_use))
alc_studytime + geom_boxplot(outlier.colour="red") + ylab("grade")+ggtitle("Student grades by alcohol consumption and weekly studytime")

alc_activities <- ggplot(alc, aes(x = high_use, y = G3, col=activities))
alc_activities + geom_boxplot() + ylab("grade")+ggtitle("Student grades by alcohol consumption and activities")

#cross-tables
alc_gender_table <-table(alc$sex,alc$high_use)
prop.table(alc_gender_table,2) #based on column totals
prop.table(alc_gender_table,1) #based on row totals
print(alc_gender_table)
margin.table(alc_gender_table,1)

alc_higher_table <-table(alc$higher,alc$high_use)
prop.table(alc_higher_table,2) #based on column totals
prop.table(alc_higher_table,1) #based on row totals

alc_romantic_table <-table(alc$romantic,alc$high_use)
prop.table(alc_romantic_table,2) #based on column totals
prop.table(alc_romantic_table,1) #based on row totals

alc_studytime_table <-table(alc$studytime,alc$high_use)
prop.table(alc_studytime_table,2) #based on column totals
prop.table(alc_studytime_table,1) #based on row totals

alc_activities_table <-table(alc$activities,alc$high_use)
prop.table(alc_activities_table,2) #based on column totals
prop.table(alc_activities_table,1) #based on row totals

#bar plots
g1_ge <- ggplot(data = alc, aes(x = high_use, fill=sex))
g1_ge + geom_bar()+ggtitle("Gender and high alcohol use")

g1_hi <- ggplot(data = alc, aes(x = high_use, fill=higher))
g1_hi + geom_bar()+ggtitle("Higher education target and high alcohol use")

g1_ro <- ggplot(data = alc, aes(x = high_use, fill=romantic))
g1_ro + geom_bar()+ggtitle("Romantic relationship and high alcohol use")

g1_st <- ggplot(data = alc, aes(x = studytime, fill=high_use))
g1_st + geom_bar()+ggtitle("Studytime and high alcohol use")

g1_ac <- ggplot(data = alc, aes(x = high_use, fill=activities))
g1_ac + geom_bar()+ggtitle("Activities and high alcohol use")

#logistic regression model
# find the model with glm()
m <- glm(high_use ~ sex + higher+ romantic + activities + studytime, data = alc, family = "binomial")
summary(m)
#coefficients
coef(m)

# compute odds ratios (OR)
OR <- coef(m) %>% exp
# compute confidence intervals (CI)
CI <-confint(m)%>% exp
# print out the odds ratios with their confidence intervals
cbind(OR, CI)

#interpretation of odds ratios is the following: A value greater than one means the odds are getting larger.
#A value less than one means the odds are getter smaller. 
#A value of one means there is no change in the odds for a change in 

# find the model with glm()
m2 <- glm(high_use ~ sex +  studytime, data = alc, family = "binomial")
summary(m2)
#coefficients
coef(m2)

# predict() the probability of high_use
probabilities <- predict(m2, type = "response")
# add the predicted probabilities to 'alc'
alc <- mutate(alc, probability = probabilities)
# use the probabilities to make a prediction of high_use
alc <- mutate(alc, prediction = probability>0.5)
# see the last ten original classes, predicted probabilities, and class predictions
select(alc, sex, studytime, high_use, probability, prediction) %>% tail(10)
table(high_use = alc$high_use, prediction = alc$prediction)



##check if these are valid

# predict() the probability of high_use
probabilities <- predict(m2, type = "response")
# add the predicted probabilities to 'alc'
alc <- mutate(alc, probability = probabilities)
# use the probabilities to make a prediction of high_use
alc <- mutate(alc, prediction = probability>0.5)
# see the last ten original classes, predicted probabilities, and class predictions
select(alc, sex, higher, romantic, activities, studytime, high_use, probability, prediction) %>% tail(10)

# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$prediction)


#binary predictions
# initialize a plot of 'high_use' versus 'probability' in 'alc'
g <- ggplot(alc, aes(x = probability, y = high_use, col=prediction))

# define the geom as points and draw the plot
g+geom_point(aes(col=prediction))

# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$prediction)

table(high_use = alc$high_use, prediction = alc$prediction) %>% prop.table() %>% addmargins()

