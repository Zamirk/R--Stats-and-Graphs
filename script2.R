# set the working directory

setwd ('C:\\student')

perfdata <- read.table('week10.dat',header=TRUE)
	
	avgidle <-mean(perfdata$X.idle)
	avgutil <-mean (100-perfdata$X.idle)
	
	#R syntax for a function
	
	calcAvgUtil <- function(idea) {
	avgutil <- mean (100-perfdata$X.idle)
	}
	
	print(calcAvgUtil(perdata$X.idle))

	calcS <- function(){
Dcpu <- avgutil /100/ X0
}

	T <- 3600
	CU <- 10000
	X0 <- C0/T

	CALCsd <- function () {
	Dcpu <- (avgutil /100) /X0
	}

print(calcCD())

print(calcAvgUtil(perfdata$x.idle))


