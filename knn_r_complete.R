library(ggplot2)

set.seed(40)

# generate training data
df <- data.frame("x" = runif(n = 700, min = 0, max = 10), "y" = runif(n = 700, min = 0, max = 10))
df$l <- 0

# set label to data
df[df$x < 4 & df$y < 3,]$l <- 1
df[df$x < 4 & df$y >= 3,]$l <- 2
df[df$x >= 4 & df$y >= 3,]$l <- 3
df[df$x >= 4 & df$y < 3,]$l <- 4

# generate validation data
df.pred <- data.frame("x" = runif(n = 50, min = 0, max = 10), "y" = runif(n = 50, min = 0, max = 10), l = 0)

# determine distance
distance <- function(x_i, x_j) {
  d <- 0
  for (i in 1:(length(x_i) - 1)) {
    s <- x_i[i] - x_j[i]
    d <- d + (s * s)
  }
  return(sqrt(d))
}

# knn algorithm
knn <- function(k, x_i, points) {
  x_i <- as.numeric(x_i)
  distances <- c()
  categories <- c()
  
  for (j in 1:nrow(points)) {
    x_j <- as.numeric(points[j,])
    distances[j] <- distance(x_i, x_j)
    categories[j] <- points[j,]$l
  }
  
  cd <- data.frame(distances,categories)[order(distances),][1:k,]
  return(names(sort(summary(as.factor(cd$categories)), decreasing=T)[1]))
}

# colors for points
df.colors <- c("#177ACC", "#E8B542", "#BF0001", "#04CA95")

# plot settings
pl <- ggplot(data = df, aes(x = x, y = y))
pl2 <- pl + geom_point(data = df[df$x < 4 & df$y < 3,], color = df.colors[1])
pl2 <- pl2 + geom_point(data = df[df$x >= 4 & df$y >= 3,], color = df.colors[3])
pl2 <- pl2 + geom_point(data = df[df$x < 4 & df$y >= 3,], color = df.colors[2])
pl2 <- pl2 + geom_point(data = df[df$x >= 4 & df$y < 3,], color = df.colors[4])

# determine knn for every new point
for (i in 1:nrow(df.pred)) {
  p <- df.pred[i,]
  p$l <- as.numeric(knn(k = 10, x_i = p, points = df))
  pl2 <- pl2 + geom_point(shape = 23, size = 3, data = p, color = "black", fill = df.colors[p$l])
}

pl3 <- pl2 + theme(text = element_text(size=15, color = "#7B7B7B")) 
print(pl3)
