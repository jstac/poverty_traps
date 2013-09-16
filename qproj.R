
#### projections using quah's estimated transition matrix.


pwt = read.csv("pwt61.csv")



gdpType = pwt$rgdptt
yearOne = 1960
yearTwo = 2000


gdp1Prelim = numeric(1)
gdp2Prelim = numeric(1)

counter = 1
for (i in 1:length(pwt$yr)) {
 if (pwt$yr[i] == yearOne) {
    gdp1Prelim[counter] = as.numeric(gdpType[i])
    counter = counter + 1
  }
}


counter = 1
for (i in 1:length(pwt$yr)) {
 if (pwt$yr[i] == yearTwo) {
    gdp2Prelim[counter] = as.numeric(gdpType[i])
    counter = counter + 1
  }
}


# Now parse vectors to get rid of the NAs


gdp1 = numeric(1)
gdp2 = numeric(1)

counter = 1
for (i in 1:length(gdp1Prelim)) {
  if ( ! is.na(gdp1Prelim[i]) )  {
    gdp1[counter] = gdp1Prelim[i]
    counter = counter + 1
  }
}

counter = 1
for (i in 1:length(gdp2Prelim)) {
  if ( ! is.na(gdp2Prelim[i]) )  {
    gdp2[counter] = gdp2Prelim[i]
    counter = counter + 1
  }
}


relgdp1 = gdp1 / mean(gdp1)
relgdp2 = gdp2 / mean(gdp2)


## Graphical output ##


pdf("foo.pdf")

close.screen(all = TRUE)

split.screen(c(2,1))


screen(1)

brq = c(0, 0.25, 0.5, 1, 2, 10)
h1 = hist(relgdp1, breaks = brq, plot=FALSE)

N = sum(h1$counts)  # number of countries
current = h1$counts / N  # the income distribution vector

v1 = rep(0.5, times = h1$counts[1])
v2 = rep(1.5, times = h1$counts[2])
v3 = rep(2.5, times = h1$counts[3])
v4 = rep(3.5, times = h1$counts[4])
v5 = rep(4.5, times = h1$counts[5])

new = c(v1, v2, v3, v4, v5)  # new vector

brn = c(0, 1, 2, 3, 4, 5)
hist(new, breaks=brn, freq=TRUE, col="gray", xlab="state", ylab="frequency", main="Income distribution, 1960", axes=FALSE)


axis(side=2)
axis(side=1, labels=FALSE)
mtext(side=1, at=0.5, 1)
mtext(side=1, at=1.5, 2)
mtext(side=1, at=2.5, 3)
mtext(side=1, at=3.5, 4)
mtext(side=1, at=4.5, 5)




h2 = hist(relgdp2, breaks = brq, plot = FALSE)

N = sum(h2$counts)  # number of countries

current = h2$counts / N  # the income distribution vector 



P = matrix(nrow = 5, ncol = 5)
P[1,] = c(0.97, 0.03, 0, 0, 0)
P[2,] = c(0.05, 0.92, 0.03, 0, 0)
P[3,] = c(0, 0.04, 0.92, 0.04, 0)
P[4,] = c(0, 0, 0.04, 0.94, 0.02)
P[5,] = c(0, 0, 0, 0.01, 0.99)

Q = P
for( j in 1:30) 
  Q = Q %*% P


P2 = matrix(nrow = 5, ncol = 5)  # the 23 year transition
P2[1,] = c(0.76, 0.12, 0.12, 0, 0)
P2[2,] = c(0.52, 0.31, 0.10, 0.07, 0)
P2[3,] = c(0.09, 0.20, 0.46, 0.26, 0)
P2[4,] = c(0, 0, 0.24, 0.52, 0.24)
P2[5,] = c(0, 0, 0, 0.05, 0.95)

Q2 = P2


proj = t(current) %*% Q  # project it forward

projCounts = proj * N  # vector of number of countries in each state

v1 = rep(0.5, times = projCounts[1])
v2 = rep(1.5, times = projCounts[2])
v3 = rep(2.5, times = projCounts[3])
v4 = rep(3.5, times = projCounts[4])
v5 = rep(4.5, times = projCounts[5])

new = c(v1, v2, v3, v4, v5)



screen(2)

h3 = hist(new, breaks = brn, freq=TRUE, col="gray", xlab="state", ylab="frequency", main="Income distribution, 2030 (projection)", axes=FALSE)

axis(side=2)
axis(side=1, labels=FALSE)
mtext(side=1, at=0.5, 1)
mtext(side=1, at=1.5, 2)
mtext(side=1, at=2.5, 3)
mtext(side=1, at=3.5, 4)
mtext(side=1, at=4.5, 5)


dev.off()
