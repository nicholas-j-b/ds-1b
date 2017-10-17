
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

#setup rasta
#devDist is how far either side of middle to look at
matSize <- 121
devDist <- .5
matx <- seq(from = .5 - devDist, to = .5 + devDist, length.out = matSize)
maty <- seq(from = .5 - devDist, to = .5 + devDist, length.out = matSize)
zVals <- matrix(0, nrow = matSize, ncol = matSize)

#populate matrix of z values
for(i in 1:matSize){
  for(j in 1:matSize){
    zVals[i,j] <- (sum(sqrt((matx[i] - xVals)**2 + (maty[j] - yVals)**2)))
  }
}

#xy coords for
inds <- which(zVals == min(zVals), arr.ind = TRUE)

#measure accuracy of guess
sqrt((xMean - matx[inds[1]])**2 + (yMean - maty[inds[2]])**2)

r <- plot(xVals, yVals, xlim = c(0, 1), ylim = c(0, 1))
p <- plot_ly(x = matx, y = maty, z = zVals)
p <- p %>% add_surface()
q <- plot_ly(z = zVals, x = matx, y = maty, type = "heatmap")

p





