IQR <- IQR(phoneRecords$TOT_MINUTES_USAGE)

outliers <- sum(length(which(phoneRecords$TOT_MINUTES_USAGE >  (Q1 + (IQR*1.5))  ))) + sum(length(which(phoneRecords$TOT_MINUTES_USAGE <  (Q2  - ( IQR *1.5))   )))

head(outliers)
ggplot(data=phoneRecords ,aes(phoneRecords$TOT_MINUTES_USAGE,
                              phoneRecords$TOT_MINUTES_USAGE)) +  geom_boxplot()

aa <- c(scale(phoneRecords$TOT_MINUTES_USAGE) > -2.68 & scale(phoneRecords$TOT_MINUTES_USAGE) < 2.68)

sum(aa[aa == FALSE] == FALSE)