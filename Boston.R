#Starting with data reading
library(MASS)
library(tidyr)
library(corrplot)
# load the data
data("Boston")
sum(is.na(Boston))
#boston_dataset.d

#checking the dimension
dim(Boston)
str(Boston)
summary(Boston)
#correlation matrix
cor_matrix<-cor(Boston) 
corrplot(cor_matrix, method="circle", type="upper",cl.pos="b", tl.pos="d", tl.cex=0.6)

#pairing graph,
pairs(Boston[-1], main="Graphical summary")

library(GGally)
library(ggplot2)
ggpairs(Boston, mapping = aes(), title="Scatter plot matrix, distributions",lower = list(combo = wrap("facethist", bins = 20)))

# center and standardize variables
boston_scaled <- scale(Boston)
summary(boston_scaled)
# class of the boston_scaled object
class(boston_scaled)
# change the object to data frame
boston_scaled<-as.data.frame(boston_scaled)
# summary of the scaled crime rate
summary(boston_scaled$crim)


#category variable
bins <- quantile(boston_scaled$crim)
bins
crime <- cut(boston_scaled$crim, breaks = bins, include.lowest = TRUE, labels = c("low", "med_low", "med_high", "high"))
table(crime)
#str(crime)
str(boston_scaled)
# remove original crim from the dataset
boston_scaled <- dplyr::select(boston_scaled, -crim)
str(boston_scaled)
# add the new categorical value to scaled data
boston_scaled <- data.frame(boston_scaled, crime)

#testing and training sets
# number of rows in the Boston dataset 
n <- nrow(boston_scaled)

# choose randomly 80% of the rows
ind <- sample(n,  size = n * 0.8)

# create train set
train <- boston_scaled[ind,]
str(trai)
# create test set 
test <- boston_scaled[-ind,]

# save the correct classes from test data
correct_classes <- test$crime

#install.packages("robustfa")
#install.robustfa
# remove the crime variable from test data
class(test)
str(test)
#test <- dplyr::select(test, -crime)


#LDA
lda.fit <- lda(crime~., data = train)
lda.fit
# the function for lda biplot arrows
lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "red", tex = 0.75, choices = c(1,2)){
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex, col=color, pos=3)
}

# target classes as numeric
classes <- as.numeric(train$crime)

# plot the lda results
plot(lda.fit, dimen = 2, col=classes, pch=classes)
lda.arrows(lda.fit, myscale = 1)
# predict classes with test data
lda.pred <- predict(lda.fit, newdata = test)

# cross tabulate the results
table(correct = correct_classes, predicted = lda.pred$class)

#bonus
boston_scaled3 <- scale(Boston)
boston_scaled3<-as.data.frame(boston_scaled3)

#kmeans clusterin
km_4 <-kmeans(boston_scaled3, centers = 5)
#str(boston_scaled3$km_4)

#lda
lda.fit2 <- lda(km_4$cluster ~., data = boston_scaled3)
lda.fit2

#biplot
lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "red", tex = 0.75, choices = c(1,2)){
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex, col=color, pos=3)
}

# plot the lda results
plot(lda.fit2, dimen = 2, col=classes, pch=classes)
lda.arrows(lda.fit2, myscale = 1)




model_predictors <- dplyr::select(train, -crime)
# check the dimensions
dim(model_predictors)
dim(lda.fit$scaling)
# matrix multiplication
matrix_product <- as.matrix(model_predictors) %*% lda.fit$scaling
matrix_product <- as.data.frame(matrix_product)

#install.packages("plotly")
library(plotly)
plot_ly(x = matrix_product$LD1, y = matrix_product$LD2,z = matrix_product$LD3, type= 'scatter3d', mode='markers')
#color addition
plot_ly(x = matrix_product$LD1, y = matrix_product$LD2, z = matrix_product$LD3, type= 'scatter3d', mode='markers',color = ~crime)

