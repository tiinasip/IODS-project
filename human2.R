#Starting with data reading
#install.packages("MASS","tidyr","dplyr","stringr")
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
library(dplyr)
setwd("~/IODS-project")
tea_time <-(tea)
tea_time
summary(tea_time)
str(tea_time)
dim(tea_time)

#some columns are selected
keep_columns <- c("Tea", "How", "how", "sugar", "where", "lunch","where","price","lunch","friends","friendliness")
tea_time2 <- select(tea, keep_columns)

#MCA
mca_tea <- MCA(tea_time2, graph = TRUE)
# summary of the model
summary(mca_tea)
# visualize MCA
plot(mca_tea, invisible=c("ind"))
print(mca_tea)

#inspiration to the analysis was searched from
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials/

#install.packages("factoextra")
library(factoextra)
eig.val <- get_eigenvalue(mca_tea)
head(eig.val)
fviz_screeplot(mca_tea, addlabels = TRUE, ylim = c(0, 45))
fviz_mca_biplot(mca_tea, 
                repel = TRUE, # To avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())

#checking what are vars
var <- get_mca_var(mca_tea)
var

fviz_mca_var(mca_tea, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

fviz_mca_var(mca_tea, col.var="black", shape.var = 15,
             repel = TRUE)

#Contributions of rows to dimension 1
fviz_contrib(mca_tea, choice = "var", axes = 1, top = 20)
# Contributions of rows to dimension 2
fviz_contrib(mca_tea, choice = "var", axes = 2, top = 20)


