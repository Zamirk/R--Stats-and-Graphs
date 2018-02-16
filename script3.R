
#Input dataset numeric weather into a dataframe
weathernum <-read.table(file ="c:/weathernumeric.txt", stringsAsFactors=FALSE, sep =",", header=TRUE)

#Show the first 10 records and all the columns
weathernum[1:10,]

#Examine the 5 number summary statistics with mean for temperature and humidity. From this calculate the
interquartile range

summary(weathernum$temperature)
summary(weathernum$humidity)

#Mi n- Max nor mal i sat i on f or t he weat her num$humi di t y at t r i but e
mmnorm.humidity <- (weathernum$humidity - min(weathernum$humidity))/(max(weathernum$humidity) -
min(weathernum$humidity))
summary(mmnorm.humidity)

#zScore Standardisation
zscore.humidity <-(weathernum$humidity - mean(weathernum$humidity))/sd(weathernum$humidity)
summary(zscore.humidity)

cars2 <- read.csv(file="c:/cars2.txt", stringsAsFactors=TRUE)

#Natural Log Transformation
natlog.weightlbs <- log(cars2$weightlbs)
natlog.weightlbs

#Square Root Transformation
sqrt.weightlbs <- sqrt(cars2$weightlbs)
sqrt.weightlbs

Standard normal Z-distribution
with µ=0 and σ=1

3
#Inverse Square Root Transformation
invsqrt.weightlbs <- 1/sqrt(cars2$weightlbs)
invsqrt.weightlbs


Binning Data
#Create a vector of values for binning
xdata <-c(1,11,2,1,1,2,12,11,44,2,12,1)

#get the sample size of the variable
n <- length(xdata)

#Declare number of bins and bin indicator
nbins <- 3
whichbin <- c(rep(0,n))

#Equal Frequency (equal-depth)
freq <- n/nbins

#sort the data
xsorted <- sort(xdata)
for(i in 1:nbins) {
 for(j in 1:n) {
 if ((i-1)*freq <j && j<=i*freq)
 whichbin[j] <- i
 }
}


#equal-width
range.xdata <- max(xdata) - min(xdata) +1
binwidth <- range.xdata/nbins
 for(i in 1:nbins) {
 for(j in 1:n) {
 if ((i-1)*binwidth < xdata[j] && xdata[j] <= (i)*binwidth)
 whichbin[j] <- i
 }
}
whichbin
xdata

#K-means clustering as a binning strategy where k = 3
kmeansclustering <- kmeans(xdata,centers=nbins)
whichbin <- kmeansclustering$cluster
whichbin

cars2 <- read.csv(file="c:/cars2.txt", stringsAsFactors=TRUE)
#Create a histogram
par(mfrow=c(1,1))

hist(cars2$weightlbs,
 breaks=30,
 xlim= c(0,5000),
 col="blue",
 border="black",
 ylim=c(0,40),
 xlab="Weight in lbs",
 ylab="Counts",
 main="Histogram of Car Weights")
box(which="plot", lty="solid", col="black")

par(mfrow = c(1,2))

plot(cars2$weightlbs, cars2$mpg,
 xlim= c(0,5000),
 ylim=c(0,600),
 xlab="Weight",
 ylab="MPG",
 main="Scatter Plot of MPG by Weight",
 type ="p",
 pch=16,
 col="blue")
points(cars2$weightlbs,
 cars2$mpg,
 type="p",
 col="red")

Create a Histogram with fitted Normal Distribution and
Normal Probability Plot
# A histogram inverse square root of weight

invsqrt.weightlbs <- 1/sqrt(cars2$weightlbs)
invsqrt.weightlbs
x <- rnorm(1000000,
mean=mean(invsqrt.weightlbs),
sd=sd(invsqrt.weightlbs))
par(mfrow=c(1,1))
hist(invsqrt.weightlbs,
 breaks=30,
 xlim= c(0.0125,0.0275),
 col="lightblue",
 prob= TRUE,
 border="black",
 xlab="Inverse Square Root of Weightlbs",
 ylab="Counts",
 main="Histogram of Inverse Square Root of Car Weightlbss")
lines(density(x),
 col="red")
box(which="plot",
 lty="solid",
 col="black")
lines(density(x),
 col="red")
 
# Normal Probability plot that indicates non-normality
par(mfrow= c(1,1))
qqnorm(invsqrt.weightlbs,
 datax =TRUE,
 col="red",
 ylim=c(0.01,0.03),
 main ="Normal Q-Q Plot of inverse Square Root of Weightlbs")
qqline(invsqrt.weightlbs,
 col="blue",
 datax=TRUE)

result <- shapiro.test( invsqrt.weightlbs)
result$p.value

x <- cars2$weightlbs[1]; x
[1] 4209
h1 <-head(cars2$weightlbs); h1
[1] 4209 1925 3449 3761 2051 3900

#Transform x using the inverse sqrt
y <- 1/sqrt(x); y
[1] 0.01541383
z <- 1/sqrt(h1); z
[1] 0.01541383 0.02279212 0.01702760 0.01630603 0.02208092
[6] 0.01601282

# Detransform x using 1/(y)^2
detransx <- 1/y^2; detransx
[1] 4209
detransz <- 1/z^2; detransz
[1] 4209 1925 3449 3761 2051 3900

#Finding Duplicate Records in a data frame
anyDuplicated(cars2)
[1] 0
duplicated(cars2)


new.cars2 <- rbind(cars2,cars2[1,])
8
anyDuplicated(new.cars2)
[1] 262

duplicated(new.cars2)
