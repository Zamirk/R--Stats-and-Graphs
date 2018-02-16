#X00110033
#Samir Kahvedzic
#CA2

#Four graphs are created in this script

#My Script
#Setting working directory to location of results.dat
setwd("C:\\Student")

#Reading results.dat file into a variable, including identifying the headers
table <- read.table("results.dat", header =TRUE)

#Ui
#Utilisation: 100 - CPU idle time value, Example (100% - 60% idle = 40%) This means 60% utilisation
#X0
#System Throughput: The Total completed transactions devide by the time used to capture data(table $CO/8)
#in other words, the transactions per seconds
#Di
#Service Demand: The Utilisation divided by the System Throughput
#R
#Responce Time: The total number of users(table $N), devided by the System Throughput

utilisation		<- (100 - table $Idle)
systemthroughput	<- (table $CO/8)
servicedemand	<- (utilisation/systemthroughput)
responcetime	<- (table $N /systemthroughput)

#Graph Comments that dont need to be said every time
#dev.new() Creates a new windows for the graph
#Adds some colour to the axis labels
#par(col.main="red") 
#par(col.lab="red");
#PCH symbols (pch=16) denotes the symbol chosen
#main,xlab,ylab are axis labels
#col="blue" sets the colour of the graph
#type = "l" sets the graph to a line graph,

#Graph 1: Ui vs N 	(The Utilisation vs the number of users)
dev.new()
par(col.main="red")
par(col.lab="red");
#Plotting the graph
plot(utilisation,pch=16,main="Utilisation vs the number of users (Ui vs N)",
xlab="(N) Number of Users",ylab="(Ui) Utilisation Percentage",col="blue")

#Function used to produce summaries of the results in the console
summary(utilisation)

#Graph 2: Di vs N 	(The Service demand vs the number of users)
dev.new()
par(col.main="Orange")
par(col.lab="red");
#Plotting the graph
plot(servicedemand,pch=16,main="Service demand vs the no. of users (Di vs N)", type = "l",
xlab="(N) Number of Users",ylab="(Di) Service demand",col="blue")

#Function used to produce summaries of the results in the console
summary(servicedemand)

#Graph 3: X0 vs N 	(The System Throughput vs the number of users)
dev.new()
par(col.main="Purple")
par(col.lab="red");
#Plotting the graph
plot(systemthroughput,pch=16,main="System Throughput vs the no. of users (X0 vs N)",
xlab="(N) Number of Users",ylab="(X0) System Throughput",col="blue")

#Function used to produce summaries of the results in the console
summary(systemthroughput)

#Graph 4: R vs N	 	(The Responce Time vs the number of users)
dev.new()
par(col.main="darkGreen")
par(col.lab="red");
#Plotting the graph
plot(responcetime,pch=16,main="Responce Time vs the no. of users (R vs N)", type = "l",
xlab="(N) Number of Users",ylab="(R) Responce time in seconds",col="blue")

#Function used to produce summaries of the results in the console
summary(responcetime)

