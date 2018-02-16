R

1.
setwd("C:/test")
library(e1071)
library(ggplot2)
phoneRecords <- read.csv("./eurocomData.csv")

1.1
nullsDataFrame <- data.frame(lapply(phoneRecords, function(VAR) sum(length(which(is.na(VAR) | VAR == "")))))
nullsDataFrame <- nullsDataFrame[!colSums(nullsDataFrame)%in%0] 

1.2
phoneRecords$MINUTES_3MONTHS_AGO[phoneRecords$MINUTES_3MONTHS_AGO == "" | phoneRecords$ MINUTES_3MONTHS_AGO [which(is.na(phoneRecords$MINUTES_3MONTHS_AGO ))]]  <-  median(phoneRecords$MINUTES_3MONTHS_AGO [which(!is.na(phoneRecords$MINUTES_3MONTHS_AGO ))])

phoneRecords$CUST_MOS[phoneRecords$ CUST_MOS [which(is.na(phoneRecords$CUST_MOS ))]]  <-  median(phoneRecords$CUST_MOS [which(!is.na(phoneRecords$CUST_MOS ))]) 

phoneRecords$TOT_MINUTES_USAGE[which(is.na(phoneRecords$TOT_MINUTES_USAGE ))]  <-  median(phoneRecords$TOT_MINUTES_USAGE [which(!is.na(phoneRecords$TOT_MINUTES_USAGE ))])

1.3
calculateMode <- function(VAR){
 	   value <- table(VAR) == max(table(VAR))
  	   return (names(table(VAR))[value])
 	}

phoneRecords$PHONE_PLAN[phoneRecords$PHONE_PLAN == ""] <- calculateMode(phoneRecords$PHONE_PLAN)
phoneRecords$EDUCATION[phoneRecords$ EDUCATION == ""] <- calculateMode(phoneRecords$ EDUCATION)

2
incLabels = c("Low income", "Medium income", "High income")
myBreaks = c(0, 37999, 88000, topCut)

phoneRecords$INCOME_BRACKETS <- cut(phoneRecords$INCOME,
                                    breaks = myBreaks,
                                    include.lowest = TRUE,
                                    labels = incLabels)


anyDuplicated(phoneRecords)

2.1
summary(phoneRecords)

					
3.
ggplot(data=phoneRecords,
       aes(phoneRecords$TOT_MINUTES_USAGE)) +
    geom_histogram(bins = 15,
                   colour = "black",
                   fill = "gray") +
    theme_bw()+
    scale_y_continuous(breaks = seq(0, 1500, 200)) +
    scale_x_continuous(breaks = seq(0, 35000, 5000)) +
    labs(x="Total minutes" ,
         y="Number of Accounts")


3.1
		 ggplot(data=phoneRecords,
       aes(phoneRecords$MINUTES_CURR_MONTH)) +
    geom_histogram(bins = 15,
                   colour = "black",
                   fill = "gray") +
    theme_bw()+
    scale_y_continuous(breaks = seq(0, 1500, 200)) +
    scale_x_continuous(breaks = seq(0, 15000, 2500)) +
    labs(x="Minutes current month" ,
         y="Number of Accounts")

		 
3.2
ggplot(data=phoneRecords,
       aes(phoneRecords$MINUTES_PREV_MONTH)) +
    geom_histogram(bins = 15,
                   colour = "black",
                   fill = "gray") +
    theme_bw()+
    scale_y_continuous(breaks = seq(0, 1700, 200)) +
    scale_x_continuous(breaks = seq(0, 18000, 2500)) +
    labs(x="Minutes previous month" ,
         y="Number of Accounts")

3.3
ggplot(data=phoneRecords,
       aes(phoneRecords$MINUTES_3MONTHS_AGO)) +
    geom_histogram(bins = 15,
                   colour = "black",
                   fill = "gray") +
    theme_bw()+
    scale_y_continuous(breaks = seq(0, 1400, 200)) +
    scale_x_continuous(breaks = seq(0, 12500, 2000)) +
    labs(x="Minutes 3 month ago" ,
         y="Number of Accounts")

3.4
ggplot(data=phoneRecords,
       aes(phoneRecords$CUST_MOS)) +
    geom_histogram(bins = 10,
                   colour = "black",
                   fill = "gray") +
    theme_bw()+
    scale_y_continuous(breaks = seq(0, 600, 100)) +
    scale_x_continuous(breaks = seq(0, 50, 10)) +
    labs(x="Number of continuous months (Customer Retention)" ,
         y="Number of Accounts")

3.5
ggplot(data=phoneRecords,
       aes(phoneRecords$NUM_LINES)) +
    geom_histogram(
        bins = 3, 
        colour = "black",
        fill = "red") +
    theme_bw()+
    labs(x="Number of lines per customer" ,
         y="Number of Accounts")
		 
3.6
		 ggplot(data=phoneRecords,
       aes(phoneRecords$INCOME_BRACKETS)) +
    geom_histogram(
        stat="count", 
        colour = "black",
        fill = "GREEN") +
    theme_bw()+
    labs(x="Income Brackets" ,
         y="Number of Accounts")
		 
3.8
ggplot(data=phoneRecords,
       aes(phoneRecords$PHONE_PLAN)) +
    geom_histogram(
        stat="count", 
        colour = "black",
        fill = "yellow") +
    theme_bw()+
    labs(x="Phone Plan" ,
         y="Number of usage")

		 
3.9
ggplot(data=phoneRecords,
       aes(phoneRecords$GENDER)) +
    geom_histogram(
        stat="count", 
        colour = "black",
        fill = "Red") +
    theme_bw()+
    labs(x="Gender" ,
         y="Number of usage")

3.9A
		 ggplot(data=phoneRecords,
       aes(phoneRecords$EDUCATION)) +
    geom_histogram(
        stat="count", 
        colour = "black",
        fill = "black") +
    theme_bw()+
    labs(x="Education" ,
         y="Number of usage")
		 
3.9B
ggplot(data=phoneRecords,
       aes(phoneRecords$CONVERGENT_BILLING)) +
    geom_histogram(
        stat="count", 
        colour = "black",
        fill = "Blue") +
    theme_bw()+
    labs(x="Convergent Billing" ,
         y="Number of usage")

3.9C
ggplot(data=phoneRecords,
       aes(
           phoneRecords$MINUTES_CURR_MONTH,
           group = phoneRecords$CHURNER,
           fill = phoneRecords$CHURNER)) +
    geom_histogram( bins = 3) +
    scale_x_continuous(breaks = seq(0, 30000, 5000)) +
    theme_bw() +
    
    labs(
        title = "Accounts vs Current month",
        fill = "Churn",
        x = "Current month" ,
        y = "No. of accounts")
	
3.9D
ggplot(data=phoneRecords,
       aes(
           phoneRecords$CUST_MOS,
           group = phoneRecords$CHURNER,
           fill = phoneRecords$CHURNER)) +
    geom_histogram(binwidth = 4) +
    scale_x_continuous(breaks = seq(0, 100, 2)) +
    scale_y_continuous(breaks = seq(0, 500, 50)) +
    theme_bw()+
    labs(
        title = "Account retention vs Time",
        fill = "Churn",
        x = "Time in months (Customer retention)" ,
        y = "No. of accounts")

3.9E
ggplot(data=phoneRecords,
       aes(phoneRecords$INCOME_BRACKETS,
           group = phoneRecords$CHURNER,
           fill = phoneRecords$CHURNER))+
    geom_histogram(
        stat="count", 
        colour = "black") +
    theme_bw()+
    labs(title = "Income vs Churn",
         x="Income" ,
         y="Number of usage",
         fill = "Churn")

3.9F
ggplot(data=phoneRecords,
       aes(
           phoneRecords$MINUTES_PREV_MONTH,
           group = phoneRecords$CHURNER,
           fill = phoneRecords$CHURNER)) +
    geom_histogram( bins = 3) +
    scale_x_continuous(breaks = seq(0, 30000, 5000)) +
    theme_bw() +
    
    labs(
        title = "Accounts vs Previous month",
        fill = "Churn",
        x = "Previous month" ,
        y = "No. of accounts")

3.9G
ggplot(data=phoneRecords,
       aes(
           phoneRecords$MINUTES_CURR_MONTH,
           group = phoneRecords$CHURNER,
           fill = phoneRecords$CHURNER)) +
    geom_histogram( bins = 3) +
    scale_x_continuous(breaks = seq(0, 30000, 5000)) +
    theme_bw() +
    
    labs(
        title = "Accounts vs Current month",
        fill = "Churn",
        x = "Current month" ,
        y = "No. of accounts")
 
3.9H
ggplot(data=phoneRecords,
       aes(phoneRecords$PHONE_PLAN,
           group = phoneRecords$CHURNER,
           fill = phoneRecords$CHURNER))+
    geom_histogram(
        stat="count", 
        colour = "black") +
    theme_bw()+
    labs(x="Phone plan" ,
         title = "Phone Plan vs Churner",
         y="Number of usage",
         fill = "Churn")

3.9I
ggplot(data=phoneRecords,
       aes(phoneRecords$GENDER,
           group = phoneRecords$CHURNER,
           fill = phoneRecords$CHURNER))+
    geom_histogram(
        stat="count", 
        colour = "black") +
    theme_bw()+
    labs(x="Gender" ,
         title = "Gender vs Churner",
         y="Number of usage",
         fill = "Churn")

3.9j
ggplot(data=phoneRecords,
       aes(phoneRecords$CONVERGENT_BILLING,
           group = phoneRecords$CHURNER,
           fill = phoneRecords$CHURNER))+
    geom_histogram(
        stat="count", 
        colour = "black") +
    theme_bw()+
    labs(title = "Convergent Billing vs Churner",
         x="CONVERGENT BILLING" ,
         y="Number of usage",
         fill = "Churn")

3.9K

ggplot(data=phoneRecords,
       aes(phoneRecords$EDUCATION,
           group = phoneRecords$CHURNER,
           fill = phoneRecords$CHURNER))+
    geom_histogram(
        stat="count", 
        colour = "black") +
    theme_bw()+
    labs(title = "Education vs Churner",
         x="Education" ,
         y="Number of usage",
         fill = "Churn")

4.
IQR <- IQR(phoneRecords$TOT_MINUTES_USAGE)

outliers <- sum(length(which(phoneRecords$TOT_MINUTES_USAGE >  (Q1 + (IQR*1.5))  ))) + sum(length(which(phoneRecords$TOT_MINUTES_USAGE <  (Q2  - ( IQR *1.5))   )))

head(outliers)
ggplot(data=phoneRecords ,aes(phoneRecords$TOT_MINUTES_USAGE,
                              phoneRecords$TOT_MINUTES_USAGE)) +  geom_boxplot()

4.1
 aa <- c(scale(phoneRecords$TOT_MINUTES_USAGE) > -2.68 & scale(phoneRecords$TOT_MINUTES_USAGE) < 2.68)

sum(aa[aa == FALSE] == FALSE)

5.
skewness(phoneRecords$INCOME)

5.1
aa <- c(scale(phoneRecords$TOT_MINUTES_USAGE) > -2.68 & scale(phoneRecords$TOT_MINUTES_USAGE) < 2.68)

sum(aa[aa == FALSE] == FALSE)

7.
ggplot(data=phoneRecords,
       aes(phoneRecords$CUST_MOS, phoneRecords$INCOME,
           colour = phoneRecords$INCOME,
           size = phoneRecords$INCOME,
           group = phoneRecords$INCOME,
           fill = phoneRecords$INCOME))+
    geom_point() +
    scale_colour_gradient(low = "red") +
    labs(x="Number of months" ,
         y="Income")

7.1
ggplot(data=phoneRecords,
       aes(phoneRecords$NUM_LINES, phoneRecords$INCOME,
           colour = phoneRecords$INCOME,
           size = phoneRecords$INCOME,
           group = phoneRecords$INCOME,
           fill = phoneRecords$INCOME))+
    geom_point() +
    scale_colour_gradient(low = "red") +
    labs(x="Number of lines" ,
         y="Income")
		 
7.2
ggplot(data=phoneRecords,
       aes(phoneRecords$CUST_MOS, phoneRecords$TOT_MINUTES_USAGE,
           colour = phoneRecords$TOT_MINUTES_USAGE,
           size = phoneRecords$TOT_MINUTES_USAGE,
           group = phoneRecords$TOT_MINUTES_USAGE,
           fill = phoneRecords$TOT_MINUTES_USAGE))+
    geom_point() +
    scale_colour_gradient(low = "red") +
    labs(x="Months" ,
         y="Total minues")
		 

