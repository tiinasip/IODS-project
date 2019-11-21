##reload data, different name is given
library(MASS)
library(ggplot2)
library(tidyr)
library(cluster)
data("Boston")
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

