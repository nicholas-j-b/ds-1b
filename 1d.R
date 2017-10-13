
set.seed(19)


find_dist <- function(x1, y1, x2, y2){
  sqrt((x1 - x2)**2 + (y1 - y2)**2)
}

k <- 6

xVals <- runif(k, 0, 1)
yVals <- runif(k, 0, 1)

nodeDists <<- c()
conn1 <<- 0
conn2 <<- 0


calc_dists <- function(g){
  g <- g - 1
  for(i in 1:(g)){
    newVal <- find_dist(xVals[i], yVals[i], xVals[g + 1], yVals[g + 1])
    nodeDists <<- c(nodeDists, newVal)
  }
  if(g > 1){
    calc_dists(g)
  }
}

get_conn <- function(a){
  conn1 <<- k
  for(i in 1:k){
    if(a <= (k - i)){
      conn2 <<- a
      break
    }else{
      conn1 <<- (conn1 - 1)
      a <- (a - k + i)
      print(a)
    }
  }
}


calc_dists(k)

nodeDists

#order nodeDists and use the shortest connection
#use get_conn to find the connected nodes
#find next shortest connection always adding used nodes to pile
#all nodes to be used precisely twice



