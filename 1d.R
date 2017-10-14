
#finds next shortest legal connection to add to path observing:
#1. don't close loop early
#2. don't visit same node twice

#for stability
#set.seed(10)

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

#check for short loop
check_loop <- function(funCon1, funCon2){
  #if end found then okay to use connection
  if(sum(funCon1 == connects) == 1 | sum(funCon2 == connects) == 1){
    print("allowed")
    print(funCon1)
    print(funCon2)
    print(conn1)
    print(conn2)
    print(connects)
    return(TRUE)
  }

  #set newPos to next node in each direction along already connected lines
  pos1 <- which(loopList %in% connects[c(TRUE, FALSE)])
  pos2 <- which(loopList %in% connects[c(FALSE, TRUE)])
  pos1 <- pos1[length(pos1)]
  pos2 <- pos2[length(pos2)]
  newPos1 <- connects[c(FALSE, TRUE)][pos1]
  newPos2 <- connects[c(TRUE, FALSE)][pos2]
  
  #if loop closes
  if(((sum(newPos1 %in% loopList)) | (sum(newPos2 %in% loopList)))){
    print("loop would have closed")
    print(newPos1)
    print(newPos2)
    print(funCon1)
    print(funCon2)
    print(loopList)
    print(connects)
    return(FALSE)
  }
  
  #if string goes on, keep checking
  loopList <<- c(loopList, newPos1, newPos2)
  check_loop(newPos1, newPos2)
}

#no. points
k <- 12

#gen points
xVals <- runif(k, 0, 1)
yVals <- runif(k, 0, 1)

#set globals
nodeDists <<- c()
loopList <<- c()
conn1 <<- 0
conn2 <<- 0
connects <<- c()

calc_dists(k)

#print statement
nodeDists

distOrd <- order(nodeDists)


#main strategy
for(i in distOrd){
  #set global conn1 and conn2 and add to list
  get_conn(i)
  #add conn1 and conn2 to connects (they may be removed later)
  connects <<- c(connects, conn1, conn2)
  
  #check whether connects still legit
  #--no repeating value more than once
  if(sum(conn1 == connects) == 3 | sum(conn2 == connects) == 3){
    connects <<- connects[1:(length(connects) - 2)]
    next
  }
  #--no short loop
  if(sum(conn1 == connects) == 2 & sum(conn2 == connects) == 2){
    loopList <<- c(conn1, conn2)
    if(!check_loop(conn1, conn2)){
      #pop dirty connects
      connects <<- connects[1:(length(connects) - 2)]
      next
    }
  }
}


xCoords <- c()
yCoords <- c()


# leftovers <- c()
# 
# for(i in 1:k){
#   if(sum(i == connects) == 1){
#     leftovers <- c(leftovers, i)
#   }
# }

plot(xVals, yVals, type = "n")
text(xVals, yVals, labels = (1:k))
for(i in seq(by = 2, length.out = (k * 2))){
  lines(xVals[connects[i:(i + 1)]], yVals[connects[i:(i + 1)]], col = "red")
}







