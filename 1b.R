
# Copyright (C) 2017 Nicholas Brooking
# 
# 1b.R is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# 1b.R is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along
# with this program.  If not, see <http://www.gnu.org/licenses/>.



#small script to address the problem of finding the geometric median
#generates random points with x value and y value each between 0 and 1
#calculates the mean of the x and y values separately and uses that as guess
#builds a rasta over the [0,1]^2 region and calculates sum of distances to all points for each intersection



library(plotly)

#setup board
numPoints <- 8
xVals <- runif(numPoints, 0, 1)
yVals <- runif(numPoints, 0, 1)

#calculate guess
xMean <- mean(xVals)
yMean <- mean(yVals)

#constants
matSize <- 61

#setup rasta
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

#prepare surface graph before iterations begin
p <- plot_ly(x = matx, y = maty, z = zVals)
p <- p %>% add_surface()


h <- 1
while(TRUE){
  #setup rasta
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

q <- plot_ly(z = zVals, x = matx, y = maty, type = "heatmap")


#measure accuracy of guess
sqrt((xMean - matx[minXY[1]])**2 + (yMean - maty[minXY[2]])**2)

#graph points
r <- plot(xVals, yVals, xlim = c(0, 1), ylim = c(0, 1))
r <- points(matx[minXY[1,1]], maty[minXY[1,2]], col = "red")

#show graph
#type q into console for zoomed in heatmap or p for 3d surface
p



