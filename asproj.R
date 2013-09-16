## Azariadis-Stachurski projection



pwt = read.csv("pwt61.csv")

# Store the stuff we want in preliminary vectors

gdpType = pwt$rgdptt
yearOne = 1960
yearTwo = 2000

countryPrelim = character(1)
gdp1Prelim = numeric(1)
gdp2Prelim = numeric(1)

counter = 1
for (i in 1:length(pwt$yr)) {
 if (pwt$yr[i] == yearOne) {
    countryPrelim[counter] = as.character(pwt$isocode[i])
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

country = character(1)
gdp1 = numeric(1)
gdp2 = numeric(2)

counter = 1
for (i in 1:length(countryPrelim)) {
  if ( (! is.na(gdp1Prelim[i]) ) && (! is.na(gdp2Prelim[i]) ) ) {
    country[counter] = countryPrelim[i]
    gdp1[counter] = gdp1Prelim[i]
    gdp2[counter] = gdp2Prelim[i]
    counter = counter + 1
  }
}


ydata = log(gdp1)  # y_t
ypdata = log(gdp2) # y_{t+1}



#-----------Provide a nonparametric density estimator--------------#

# Set bandwidth given data for nonparametric density estimation.  The
# formula is standard (e.g. Hormann and Leydold, "Automatic Random
# Variate Generation for Simulation Input").

bandwidth = function(v) {  # v a vector of observations
  s = sqrt( var(v) ) # standard dev.
  r = IQR(v) # interquartile range
  return(  0.776 * 1.364 * min(c(s,r/1.34)) * (length(v)^(-1/5)) )
}  

# Now the nonparametric density estimate using Gaussian kernel.  Takes
# a vector x, a vector of data v; returns vector giving values of
# density at each point x

nonparaDensity = function(x, v) { 
  b = bandwidth(v)
  ndb = function(s,t) {
    return(dnorm(s,t,b)) 
  } # Val of Gaussian density at s with mean t, standard dev. = b
  z = outer(x,v, ndb) # matrix of kernel values
  u = rep(1,length(v)) # create vector of 1s
  rv = z %*% u # and sum each row
  return( (1/length(v)) * rv )
}

phi1 = function(z)
  return(nonparaDensity(z,ydata))


pdf("foo.pdf")

close.screen(all = TRUE)

split.screen(c(2,1))

screen(1)

xPoints = seq(5.3, 12.5, length=200)
yPoints1 = phi1(xPoints)

plot(xPoints, yPoints1, type="n", xlab="log income per capita", ylab="prob.", main="Income distribution, 1960")

xo = c(xPoints, xPoints[1])
yo = c(yPoints1, yPoints1[1])
#polygon(xo, yo, lty=3, density=8, angle=-45, border=NA)
polygon(xo, yo, lty=3, col="gray", border=NA)

lines(xPoints, yPoints1)



screen(2)

dat = read.table("2030.dat")
yPoints2 = dat[,2]

plot(xPoints, yPoints2, type="n", xlab="log income per capita", ylab="prob.", main="Income distribution, 2030 (projection)")

xo = c(xPoints, xPoints[1])
yo = c(yPoints2, yPoints2[1])
#polygon(xo, yo, lty=3, density=8, angle=-45, border=NA)
polygon(xo, yo, lty=3, col="gray", border=NA)

lines(xPoints, yPoints2)

dev.off()
