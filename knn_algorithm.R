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
  distances = c()
  categories <- c()
  
  for (j in 1:nrow(points)) {
    x_j <- as.numeric(points[j,])
    distances[j] <- distance(x_i, x_j)
    categories[j] <- points[j,]$l
  }
  
  cc <- c()
  for (i in 1:k) {
    ix <- which.min(distances)
    cc[i] <- categories[ix]
    distances <- distances[-ix]
    categories <- categories[-ix]
  }
  return(names(which.max(table(cc))))
}
