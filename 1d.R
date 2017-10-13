

#for stability
set.seed(19)

#find the distance between two points given x and y vals
find_dist <- function(x1, y1, x2, y2){
  sqrt((x1 - x2)**2 + (y1 - y2)**2)
}

#recursively find all dists between all points
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

#determine which connection was referenced by setting global conn1 and conn2
get_conn <- function(a){
  conn1 <<- k
  for(i in 1:k){
    if(a <= (k - i)){
      conn2 <<- a
      break
    }else{
      conn1 <<- (conn1 - 1)
      a <- (a - k + i)
    }
  }
}

#no. points
k <- 6

#gen points
xVals <- runif(k, 0, 1)
yVals <- runif(k, 0, 1)

#set globals
nodeDists <<- c()
conn1 <<- 0
conn2 <<- 0

calc_dists(k)

#print statement
nodeDists

distOrd <- order(nodeDists)
connects <- c()

#main strategy
for(i in distOrd){
  #set global conn1 and conn2 and add to list
  get_conn(i)
  connects <- c(connects, conn1, conn2)
  
  #check whether list still legit
  #--no repeating value more than once
  #--no short loop
  if((sum(conn1 == connects) == 2 & sum(conn2 == connects) == 2) | (sum(conn1 == connects) == 3 | sum(conn2 == connects) == 3)){
    connects <- connects[1:(length(connects) - 2)]
  }
}

#print connects
connects

#order nodeDists and use the shortest connection
#use get_conn to find the connected nodes
#find next shortest connection always adding used nodes to pile
#all nodes to be used precisely twice and no closing the loop early



