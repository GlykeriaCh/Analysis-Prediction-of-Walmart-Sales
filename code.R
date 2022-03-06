# Cleaning all objects from yout workspace
rm(list=ls(all=TRUE))

dataset_in <- read.csv("~/Desktop/Walmart Sales/Walmart.csv", stringsAsFactors = F)

# 1 Visualize
library(forecast)
t <- ts(dataset_in$Total, frequency = 365, start = c(2011,32))
ca <- ts(dataset_in$CA, frequency = 365, start = c(2011,32))
tx <- ts(dataset_in$TX, frequency = 365, start = c(2011,32))
wi <- ts(dataset_in$WI, frequency = 365, start = c(2011,32))
autoplot(cbind(t,ca,tx,wi), main="Visualization", y="Sales")


plot(as.Date(dataset_in$date), dataset_in$Total, type="l")
points(as.Date(dataset_in$date), dataset_in$CA, type="l", col="red")
points(as.Date(dataset_in$date), dataset_in$TX, type="l", col="blue")
points(as.Date(dataset_in$date), dataset_in$WI, type="l", col="green")


t <- ts(dataset_in$Total, frequency = 7)
ca <- ts(dataset_in$CA, frequency = 7)
tx <- ts(dataset_in$TX, frequency = 7)
wi <- ts(dataset_in$WI, frequency = 7)

# 2 Correlation
library(corrplot)
corrplot(cor(cbind(t,ca,tx,wi)),  method = "number",type="lower")

# 3 Proportions
c(sum(ca)/sum(t), sum(tx)/sum(t), sum(wi)/sum(t))*100

library(lubridate)
dataset_in$year <- year(dataset_in$date)
dataset_in$month <- month(dataset_in$date)
dataset_in$wday <- wday(dataset_in$date)

library(plyr)
week_avg <- ddply(dataset_in[,c("Total","CA","WI","TX","wday")], .(wday) , colwise(sum))
cbind(week_avg$wday, week_avg$CA*100/week_avg$Total, week_avg$TX*100/week_avg$Total, week_avg$WI*100/week_avg$Total)

month_avg <- ddply(dataset_in[,c("Total","CA","WI","TX","month")], .(month) , colwise(sum))
cbind(month_avg$month, month_avg$CA*100/month_avg$Total, month_avg$TX*100/month_avg$Total, month_avg$WI*100/month_avg$Total)

year_avg <- ddply(dataset_in[,c("Total","CA","WI","TX","year")], .(year) , colwise(sum))
cbind(year_avg$year, year_avg$CA*100/year_avg$Total, year_avg$TX*100/year_avg$Total, year_avg$WI*100/year_avg$Total)

# 4 Histogram and comparison with normal distribution
hist(t)
mv <- mean(t)
sd <- sd(t)
theoretical <- rnorm(length(t), mv, sd)
plot(density(t, bw=(1969/2)))
lines(density(theoretical, bw=1969/2), col="blue")

# 5 Outliers
outliers <- boxplot(t)
max_value <- outliers$stats[5,]
min_value <- outliers$stats[1,]

tadj <- t
tadj[tadj>max_value] <- max_value
tadj[tadj<min_value] <- min_value

detect <- data.frame(t,tadj)
detect$point <- NA
detect[detect$t!=detect$tadj,]$point <- detect[detect$t!=detect$tadj,]$t
length(detect[detect$t!=detect$tadj,]$poin)
plot(as.numeric(detect$t), type="l", ylab="Sales")
points(detect$point, col="red")

# 6 ACF
Acf(as.numeric(t),lag.max = 365)
Acf(diff(as.numeric(t),1),lag.max = 365) #remove trend
Acf(diff(diff(as.numeric(t),1),7),lag.max =365) #remove seasonality

# 7 Decomposition
d <- decompose(t, type="multiplicative")
plot(d$seasonal[1:7], type="l")

monthly <- ts(ddply(dataset_in[,c("Total","year","month")], .(year,month), colwise(sum))$Total, frequency = 12, start = c(2011,2))
autoplot(monthly)
dm <- decompose(monthly, type="multiplicative")
plot(dm$seasonal[1:12], type="l")

# 8 t-s features
library(tsfeatures)
tsfeatures(monthly, "stl_features")
tsfeatures(t, "stl_features")

# 9 Regression
library(caret)
data_d <- dataset_in[,c("month","wday","event_type")]
data_d$month <- as.character(data_d$month)
data_d$wday <- as.character(data_d$wday)
dummy <- dummyVars(" ~ .", data=data_d)
newdata <- data.frame(predict(dummy, newdata = data_d)) 
newdata[is.na(newdata$event_typeCultural),]$event_typeCultural <- 0
newdata[is.na(newdata$event_typeNational),]$event_typeNational <- 0
newdata[is.na(newdata$event_typeReligious),]$event_typeReligious <- 0
newdata[is.na(newdata$event_typeSporting),]$event_typeSporting <- 0
newdata$month12 = newdata$wday7 <- NULL
newdata$trend <- c(1:nrow(newdata))
CA_set <- cbind(dataset_in[,c("CA","snap_CA","year")],newdata)
TX_set <- cbind(dataset_in[,c("TX","snap_TX","year")],newdata)
WI_set <- cbind(dataset_in[,c("WI","snap_WI","year")],newdata)
Total_set <- cbind(dataset_in[,c("Total","snap_CA","snap_TX","snap_WI","year")],newdata)

CA_set$lag1 = CA_set$lag7 = CA_set$lag14 <- NA
TX_set$lag1 = TX_set$lag7 = TX_set$lag14 <- NA
WI_set$lag1 = WI_set$lag7 = WI_set$lag14 <- NA
Total_set$lag1 = Total_set$lag7 = Total_set$lag14 <- NA
for (i in 15:nrow(Total_set)){
  CA_set$lag1[i] <- CA_set$CA[i-1] ; CA_set$lag7[i] <- CA_set$CA[i-7] ; CA_set$lag14[i] <- CA_set$CA[i-14] 
  TX_set$lag1[i] <- TX_set$TX[i-1] ; TX_set$lag7[i] <- TX_set$TX[i-7] ; TX_set$lag14[i] <- TX_set$TX[i-14] 
  WI_set$lag1[i] <- WI_set$WI[i-1] ; WI_set$lag7[i] <- WI_set$WI[i-7] ; WI_set$lag14[i] <- WI_set$WI[i-14] 
  Total_set$lag1[i] <- Total_set$Total[i-1] ; Total_set$lag7[i] <- Total_set$Total[i-7] ; Total_set$lag14[i] <- Total_set$Total[i-14] 
}
CA_set <- na.omit(CA_set)
TX_set <- na.omit(TX_set)
WI_set <- na.omit(WI_set)
Total_set <- na.omit(Total_set)


model_CA <- lm(CA ~ ., data=CA_set)
frc <- predict(model_CA)
plot(CA_set$CA, type="l", main = 'Actual vs Predicted', ylab = 'CA')
lines(frc, col="red")
accuracy(CA_set$CA, frc)

model_TX <- lm(TX ~ ., data=TX_set)
frc <- predict(model_TX)
plot(TX_set$TX, type="l", main = 'Actual vs Predicted',  ylab = 'TX')
lines(frc, col="red")
accuracy(TX_set$TX, frc)

model_WI <- lm(WI ~ ., data=WI_set)
frc <- predict(model_WI)
plot(WI_set$WI, type="l", main = 'Actual vs Predicted', ylab = 'WI')
lines(frc, col="red")
accuracy(WI_set$WI, frc)

model_total <- lm(Total ~ ., data=Total_set)
frc <- predict(model_total)
plot(Total_set$Total, type="l", main = 'Actual vs Predicted', ylab = 'Total')
lines(frc, col="red")
accuracy(Total_set$Total, frc)

# cross validation
acc_res <- c()
for (sn in 1:500){
  set.seed(sn)
  sampleids <- sample(c(1:nrow(Total_set)),nrow(Total_set), replace=F)
  trainset <- Total_set[head(sampleids,1130),]
  testset <- Total_set[tail(sampleids,286),]
  model_total <- lm(Total ~ .-event_typeSporting-event_typeReligious-trend, data=trainset)
  frc <- predict(model_total, testset)
  acc_res <- c(acc_res,accuracy(testset$Total, frc)[5])
}
mean(acc_res)


