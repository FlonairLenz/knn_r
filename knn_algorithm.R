distance <- function(x_i, x_j) {
  d <- 0
  for (i in 1:(length(x_i) - 1)) {
    s <- x_i[i] - x_j[i]
    d <- d + (s * s)
  }
  return(sqrt(d))
}

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
