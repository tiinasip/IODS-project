#Starting with data reading
library(MASS)
library(tidyr)
library(dplyr)
library(stringr)
# load the data
human2 <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human2.txt", sep  =",", header = T)

str(human2)
names(human2)

library(GGally)
library(ggplot2)
ggpairs(human2, mapping = aes(), title="Scatter plot matrix, distributions ",lower = list(combo = wrap("facethist", bins = 20)))

library(corrplot)
summary(human2)
cor_matrix<-cor(human2)
cor_matrix
corrplot(cor_matrix, method="circle", type="upper",cl.pos="b", tl.pos="d", tl.cex=0.6)

#for nonstandardised data
# perform principal component analysis (with the SVD method)
pca_human <- prcomp(human2)
biplot(pca_human, choices = 1:2, cex = c(0.5, 2),col = c("grey40", "deeppink2"))

human_std <- scale(human2)
pca_human2 <- prcomp(human_std)
biplot(pca_human2, choices = 1:2, cex = c(0.5, 2),col = c("grey40", "deeppink2"))

s<-summary(pca_human2)
pca_pr <- round(100*s$importance[2,], digits = 1)
pca_pr
pc_lab <- paste0(names(pca_pr), " (", pca_pr, "%)")

# draw a biplot
biplot(pca_human2, cex = c(0.8, 1), col = c("grey40", "deeppink2"), xlab = pc_lab[1], ylab = pc_lab[2])
#install.packages("FactoMineR")
library(FactoMineR)
tea_time <-data(tea)
summary(tea_time)
str(tea_time)
dim(tea_time)