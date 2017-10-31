


# total.times <- c()
# total.accuracy <- c()

#timer
ptm <- proc.time()

#setup board
numPoints <- 500
xVals <- runif(numPoints, 0, 1)
yVals <- runif(numPoints, 0, 1)

#calculate guess
xMean <- mean(xVals)
yMean <- mean(yVals)

#constants
matSize <- 61

#setup raster
matx <- seq(from = 0, to = 1, length.out = matSize)
maty <- seq(from = 0, to = 1, length.out = matSize)
zVals <- matrix(0, nrow = matSize, ncol = matSize)

#populate matrix of z values
for(i in 1:matSize){
  for(j in 1:matSize){
    for(k in 1:numPoints){
      zVals[i,j] <- zVals[i,j] + (sqrt((matx[i] - xVals[k])**2 + (maty[j] - yVals[k])**2))
    }
  }
}

#xy coords for
minXY <- which(zVals == min(zVals), arr.ind = TRUE)
print(matx[minXY[1,1]])
print(maty[minXY[1,2]])

#zoom and repeat
h <- 1
while(TRUE){
  #setup raster
  matx <- seq(from = matx[minXY[1,1]] - (1 / (2^h)), to = matx[minXY[1,1]] + (1 / (2^h)), length.out = matSize)
  maty <- seq(from = maty[minXY[1,2]] - (1 / (2^h)), to = maty[minXY[1,2]] + (1 / (2^h)), length.out = matSize)
  zVals <- matrix(0, nrow = matSize, ncol = matSize)
  
  #populate matrix of z values
  for(i in 1:matSize){
    for(j in 1:matSize){
      for(k in 1:numPoints){
        zVals[i,j] <- zVals[i,j] + (sqrt((matx[i] - xVals[k])**2 + (maty[j] - yVals[k])**2))
      }
    }
  }
  
  #stop process if zoom made no difference
  tempXY <- which(zVals == min(zVals), arr.ind = TRUE)
  if(tempXY[1,1] == minXY[1,1] & tempXY[1,2] == minXY[1,2]){
    break
  }
  minXY <- tempXY
  h <- h + 1
}

#print data
cat("number of iterations: ", h)
cat("final solution: (", matx[minXY[1,1]], maty[minXY[1,2]], ")")

#end timer
calc.time <- (proc.time() - ptm) / h


#measure accuracy of guess
accuracy <- sqrt((xMean - matx[minXY[1]])**2 + (yMean - maty[minXY[2]])**2)

total.times <- c(total.times, calc.time[3])
total.accuracy <- c(total.accuracy, accuracy)

total.times
mean(total.times)

total.accuracy
mean(total.accuracy)




