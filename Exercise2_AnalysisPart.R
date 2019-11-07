# Analysis starts with reading the data
learnings2 <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/learning2014.txt ", sep=",", header=TRUE)

#dimenesions and structure check
dim(learnings2)
str(learnings2)
summary(learnings2)

str(learnings2)

#{r fig1, fig.path ="images"}
pairs(learnings2[-1],col = learnings2$gender, main="Scatter plot matrix")

# access the GGally and ggplot2 libraries
library(GGally)
library(ggplot2)

# create a more advanced plot matrix with ggpairs()
p <- ggpairs(learnings2, mapping = aes(col=gender, alpha = 0.3), title="Scatter plot matrix and gender ",
             lower = list(combo = wrap("facethist", bins = 20)))

# draw the plot
p

# create the plot without gender coloring and grouping
p2 <- ggpairs(learnings2, mapping = aes(), title="Scatter plot matrix, no gender as column ",lower = list(combo = wrap("facethist", bins = 20)))

# draw the plot without gender coloring and groupint
p2

#regression analysis part begins
#attitude and points linear modelling
qplot(attitude, points, data = learnings2) + geom_smooth(method = "lm")

#Simple linear model of attitude explaining points
my_model <- lm(points ~ attitude, data = learnings2)
summary(my_model)
plot(my_model, which = c(1,2,5))

#Second model, stra added to the model
my_model2 <- lm(points ~ attitude + stra, data = learnings2)
summary(my_model2)
plot(my_model2, which = c(1,2,5))

#Third model, attitude, stra and surf were selected as explanatory
#fitted model is created
my_model3 <- lm(points ~ attitude + stra +surf, data = learnings2)
summary(my_model3)
plot(my_model3, which = c(1,2,5))




