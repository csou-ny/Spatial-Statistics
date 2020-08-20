#St.Paddy's day of Manhattan
# Tuesday 3/17/15 
# Weekend: 3/20/15 to 3/22/15
St.Paddy2015 <- subset(d, Borough=="MANHATTAN", select = ID:LatLong) #create Manhattan subset
St.Paddy2015week <-rbind(St.Paddy2015[St.Paddy2015$Date.Start=="03/16/2015", ], St.Paddy2015[St.Paddy2015$Date.Start=="03/17/2015",], St.Paddy2015[St.Paddy2015$Date.Start=="03/18/2015", ], St.Paddy2015[St.Paddy2015$Date.Start=="03/19/2015", ], St.Paddy2015[St.Paddy2015$Date.Start=="03/20/2015", ], St.Paddy2015[St.Paddy2015$Date.Start=="03/21/2015", ], St.Paddy2015[St.Paddy2015$Date.Start=="03/22/2015", ])
St.Paddy2015weekFull<- subset(St.Paddy2015week, !is.na(St.Paddy2015week$Lat)& !is.na(St.Paddy2015week$Long))

library(sp)
library(rgdal)
dataobjSP2015 = data.frame(lat= St.Paddy2015weekFull$Lat,long=St.Paddy2015weekFull$Long)
coordinates(dataobjSP2015) <- ~long+lat
proj4string(dataobjSP2015) <- CRS("+init=epsg:4326")
data.projSP2015 <-spTransform(dataobjSP2015, CRS("+init=epsg:2831"))
XYConvertedSP2015 <- as.data.frame(data.projSP2015)

library(spatstat)
ManWindow2 <- owin(xrange=c(58000,80000) , yrange=c(295000,311000)   )
SP2015Rectangle <- ppp(XYConvertedSP2015$lat, XYConvertedSP2015$long, window=ManWindow2)
SP2015Flipped <- flipxy(SP2015Rectangle)
#after conversion, add unitname
#epsg:2831 is based on feet
unitname(SP2015Flipped) <- "feet"
plot(SP2015Flipped)

SP2015q2 <- quadratcount(ManRectangle2Flipped, nx=2, ny=2)
plot(SP2015q2)
#use q2 for quadrat test
quadrat.test(SP2015q2) #CSR is rejected

intensity(SP2015Flipped)

### sigma bandwidth selection
### default cross validation(likelihood) of sigma
b <- bw.ppl(SP2015Flipped)
b # returns sigma= 104.4193
denDefaultSP2015 <- density(rescale(SP2015Flipped), sigma= b)
plot(denDefaultSP2015)
plot(b)

#based on diggle
bw.diggle(SP2015Flipped) #returns sigma= 3.913894
denDiggleSP2015 <- density(rescale(SP2015Flipped), sigma= bw.diggle(SP2015Flipped))
plot(denDiggleSP2015)
plot(bw.diggle(SP2015Flipped))

#based on scott rule of thumb
bw.scott(SP2015Flipped) #576.9195 1254.6236 in x and y directions
denScottSP2015 <- density(rescale(SP2015Flipped), sigma= bw.scott(SP2015Flipped))
plot(denScottSP2015) # obvious gradual trend--- useful in its own rights

#--------- Assumes Poisson--------
#Standard error based on likelihood (default)
defaultseSP2015<- density(rescale(SP2015Flipped), sigma=b, se=TRUE)$SE
plot(defaultseSP2015) #NA's produced

#Standard error based on diggle
diggleseSP2015 <- density(rescale(SP2015Flipped), sigma= bw.diggle(SP2015Flipped), se=TRUE)$SE
plot(diggleseSP2015) 

#Standard error based on scott rule of thumb
scottseSP2015 <- density(rescale(SP2015Flipped), sigma= bw.scott(SP2015Flipped), se=TRUE)$SE
plot(scottseSP2015)

#Factor based ppp object creation chapter 14
# You should accumulate all 11 years worth of data for this
XYConvertedSP2015Full <- cbind(XYConvertedSP2015,St.Paddy2015weekFull$Offense.Level)
#rename third column 
names(XYConvertedSP2015Full)[3] <- "OffenseLevel"
MultiTypeSP2015 <- ppp(XYConvertedSP2015Full$lat,XYConvertedSP2015Full$long, marks= XYConvertedSP2015Full$OffenseLevel, window=ManWindow2)
MultiTypeSP2015Flipped <- flipxy(MultiTypeManhattan)
is.multitype(MultiTypeSP2015Flipped) #TRUE
plot(MultiTypeSP2015Flipped)
plot(split(MultiTypeSP2015Flipped))

#based on default bandwidth
intensity(rescale(MultiTypeSP2015Flipped))
#   FELONY  MISDEMEANOR    VIOLATION 
# 4.090909e-06 5.857955e-06 1.230114e-06 
plot(density(split(MultiTypeSP2015Flipped))) # beautiful!
# compare this to the intensiy for total weekend

#relative risk function and probability plot
plot(relrisk(MultiTypeSP2015Flipped))
ProbSP2015<- relrisk(MultiTypeSP2015Flipped, at="points")
plot(ProbSP2015) #relationship? inverse between misdemeanor and felony
ProbSP2015[1:3]
# returns [1] 0.3025828 0.3932092 0.3339663
# this is a preface to second order intensity

#-----Probabilities of each type pg577 (empirically)
lambdaMultiTypeSP2015 <-intensity(MultiTypeSP2015Flipped)
probsSP2015MultiType <- lambdaMultiTypeSP2015/sum(lambdaMultiTypeSP2015)
probsSP2015MultiType # how come it's different from ProbSP2015?
#returns    FELONY MISDEMEANOR   VIOLATION 
#       0.3659466   0.5240152   0.1100381 
# probabilities are spatially varying

#Nonparametric estimation of probabilities 
ProbSP2015MultiTypeDiggle <- relrisk(MultiTypeSP2015Flipped, diggle=TRUE, at="points")
plot(ProbSP2015MultiTypeDiggle) #same picture as above
ProbSP2015MultiTypeDiggleNoPoints <-relrisk(MultiTypeSP2015Flipped, diggle=TRUE)
ProbSP2015MultiTypeDiggle
#Now with SE and Intensity estimates----assuming Poisson
#ProbSP2015MultiTypeDiggle2 <- relrisk(MultiTypeSP2015Flipped, diggle=TRUE, at="points",relative = TRUE)
#plot(ProbSP2015MultiTypeDiggle2) #incomprehensible

#Multitype based on diggle
plot(density(split(MultiTypeSP2015Flipped), sigma=bw.diggle(MultiTypeSP2015Flipped)))
#beautiful and more informative than the above based on default

#----experimental code on pg579
dominant <- im.apply(ProbSP2015MultiTypeDiggleNoPoints, which.max)
CrimeTypes <- levels(marks(MultiTypeSP2015Flipped))
dominant <- eval.im(factor(dominant, labels=CrimeTypes))
plot(dominant)
#texture plot of probability of most likely to happen at a location
#segregation test
segregation.test(MultiTypeSP2015Flipped)


#look at section 14.6.8
# look at section 14.7.1
#look at section 15.2.3 for covariate ppp of call length


#-----------Assumes Homogeneous-------
#g function used for what? page 228
g <- pcf(SP2015Flipped)
plot(g)

#block variance estimation without poisson model assumption
SP2015FlippedBlockVar <- varblock(rescale(SP2015Flipped), Kest,nx=2,ny=2)
plot(SP2015FlippedBlockVar)
# Local kfunctions Bootstrap
SP2015FlippedKloh <- lohboot(SP2015Flipped, Kest)
plot(SP2015FlippedKloh)

#---------Inhomogeneous k and g functions Section 7.10.2
#based on default bandwidth
plot(Kinhom(SP2015Flipped, sigma= b))
#calculate inhomogeneous pair function
inGfunc <- pcfinhom(SP2015Flipped)
plot(inGfunc)
#test against hypothesis of inhomogeneous poisson model pg 246
#lam <- density(SP2015Flipped, bw.ppl(SP2015Flipped))
#Env <- envelope(SP2015Flipped, 
  #              Lcross.inhom, sigma=bw.ppl(SP2015Flipped),
 #               simulate=expression(rpoispp(lam)))
#plot(Env, . - r ~ r)
#mad.test(SP2015Flipped)
