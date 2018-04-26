set.seed(40)
df <- data.frame("x" = runif(n = 700, min = 0, max = 10), "y" = runif(n = 700, min = 0, max = 10))
df$l <- 0
df[df$x < 4 & df$y < 3,]$l <- 1
df[df$x < 4 & df$y >= 3,]$l <- 2
df[df$x >= 4 & df$y >= 3,]$l <- 3
df[df$x >= 4 & df$y < 3,]$l <- 4
