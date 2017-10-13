library(plotly)

set.seed(6)

d1 <- runif(10, 0, 1)
d2 <- runif(10, 0, 1)

p1 <- mean(d1)
p2 <- mean(d2)

matSize <- 81
devDist <- .3

p1 <- seq(from = (p1 - devDist), to = (p1 + devDist), length.out = matSize)
p2 <- seq(from = (p2 - devDist), to = (p2 + devDist), length.out = matSize)

ymat <- matrix(0, nrow = matSize, ncol = matSize)

for(i in 1:matSize){
  for(j in 1:matSize){
    ymat[i,j] <- sum(sqrt((p1[i] - d1)**2 + (p2[j] - d2)**2))
  }
}

inds <- which(ymat == min(ymat), arr.ind = TRUE)
mean(d1)
p1[inds[1]]
mean(d2)
p2[inds[2]]



p <- plot_ly(z = ymat) %>% add_surface()

p





