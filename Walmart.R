#Required Libraries
library(ggplot2)
library(dplyr)
#Loading datasets
setwd("C:/Users/rohan/OneDrive/Desktop/Revenue Datasets/Walmart")
stores <- read.csv("stores.csv")
features <- read.csv("features.csv")
train <- read.csv("train.csv")
test <- read.csv("test.csv")
dataset <- merge(stores,features)
train_features <- left_join(train,features, by = c("Store", "Date"))
train_features2 <- left_join(train_features1,features ,by = c("Store", "Date"))
train_features2$Date <- as.Date(train_features2$Date)
ts <- xts(train_features2$Weekly_Sales, train_features2$Date)
#Data Preprocessing and Exploration 
length(unique(train$Store)) # 45
length(unique(train$Dept)) # 81 
length(unique(stores$Type)) # 3
#Store size
p1 <- ggplot(stores) +
   geom_histogram(mapping = aes(stores$Size), fill = "lightgreen" ) + facet_wrap(Type ~.) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))
#Unique number of stores
unqiue_store <- summary(stores$Type)
#merging store and train
train_store <- inner_join(stores, train)

p2 <- ggplot(train_store) +
      geom_histogram(aes(train_store$Weekly_Sales), fill = "skyblue",bins = 15) + 
     facet_wrap(Type ~.) + scale_x_log10() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))


#left join train and features
train_features <- left_join(train,features, by = c("Store", "Date"))
test_features <- left_join(test,features, by = c("Store","Date"))
train_features <- train %>% left_join(stores, by = "Store") %>% left_join(features, by = c("Store", "Date")) %>% mutate(first_wk = ifelse(mday(Date) <= 7, TRUE, FALSE))

p3 <- ggplot(train_features) + 
    geom_bar(aes(x=train_features$Temperature, (y=train_features$Weekly_Sales)/1000),stat = "identity") +
    xlab("Temperature") + ylab("Sales")

p4 <- ggplot(train_features) +
      geom_bar(aes(x=train_features$Fuel_Price, (y=train_features$Weekly_Sales)/1000), stat = "identity") +
      xlab("Fuel") + ylab("Sales")

p5 <- ggplot(train_features) +
  geom_bar(aes((x=train_features$IsHoliday.x), (y=train_features$Weekly_Sales)/1000), stat = "identity") +
  xlab("CPI") + ylab("Sales")

#pairs(scale(train_features[,c(4,6,7,13,14)]))
# 'weeklySales', 'Fuel_Price', 'Size', 'CPI', 'Dept', 'Temperature', 'Unemployment'

#Modelling
library(caTools)
set.seed(123)
train_features <- train1[complete.cases(train_features$Weekly_Sales),]
train_features$Weekly_Sales <- scale(train1$Weekly_Sales)
train_features$Temperature <- scale(train1$Temperature)
split <- sample.split(train_features, SplitRatio = 2/3)
train1 <- subset(train_features, split == TRUE)
train1 <- train1[,-1]
train1 <- train1[complete.cases(train1),]
test1 <- subset(train_features, split == FALSE)
test1 <- test[,-1]


test1 <- test1[complete.cases(test1),]

linear_regression <- lm(formula = train1$Weekly_Sales~., data = train1)
lr <- lm(formula = train$Weekly_Sales~., data = train)
y_pred <- predict(lr, newdata = train)

decision_regressor <- rpart(formula = train1$Weekly_Sales ~.,
                  data = train1,
                  control = rpart.control(minsplit = 1))
summary(decision_regressor)
random_forest <-randomForest(x = train1[,-1],
                             y = train1$Weekly_Sales,
                             ntree = 5)
multiple_regression <- lm(formula = train1$Weekly_Sales~., data = train1)
y_pred <- predict(linear_regression, test1)
RMSE(mean(scale(test1$Weekly_Sales)),mean(scale(linear_regression$fitted.values)))
mean(linear_regression)



summary(train_features2)



# Forecasting -ARIMA
library(forecast)
getwd()
data<-read.csv("train.csv")
stores<-read.csv("stores.csv")
#Filtering dept1
data.dept<-subset(data,data$Dept == 1)

#Filtering storeA from Above
storeA<-subset(stores,stores$Type=='A')
uniqueStoresA <-unique(storeA$Store)
data.dept.StoreA<-subset(data.dept, is.element(data.dept$Store,uniqueStoresA))

#Filtering storeB from Above
storeB<-subset(stores,stores$Type=='B')
uniqueStoresB <-unique(storeB$Store)
data.dept.StoreB<-subset(data.dept, is.element(data.dept$Store,uniqueStoresB))

#Filtering storeC from Above
storeC<-subset(stores,stores$Type=='C')
uniqueStoresC <-unique(storeC$Store)
data.dept.StoreC<-subset(data.dept, is.element(data.dept$Store,uniqueStoresC))

uniqueDates <- unique(data.dept$Date)

data.dept.Total <-data.frame(Date=as.Date(character()), Weekly_Sales=numeric())
data.dept.Total.StoreA <-data.frame(Date=as.Date(character()), Weekly_Sales=numeric())
data.dept.Total.StoreB <-data.frame(Date=as.Date(character()), Weekly_Sales=numeric())
data.dept.Total.StoreC <-data.frame(Date=as.Date(character()), Weekly_Sales=numeric())
for(uDate in uniqueDates){
  ssTotal<-subset(data.dept,data.dept$Date==uDate)
  data.dept.Total <- rbind( data.dept.Total, data.frame("Date"=as.Date(uDate), "Weekly_Sales"= mean(ssTotal$Weekly_Sales)))
  
  ssTotalA<-subset(data.dept.StoreA,data.dept.StoreA$Date==uDate)
  data.dept.Total.StoreA <- rbind( data.dept.Total.StoreA, data.frame("Date"=as.Date(uDate), "Weekly_Sales"= mean(ssTotalA$Weekly_Sales)))
  
  ssTotalB<-subset(data.dept.StoreB,data.dept.StoreB$Date==uDate)
  data.dept.Total.StoreB <- rbind( data.dept.Total.StoreB, data.frame("Date"=as.Date(uDate), "Weekly_Sales"= mean(ssTotalB$Weekly_Sales)))
  
  ssTotalC<-subset(data.dept.StoreC,data.dept.StoreC$Date==uDate)
  data.dept.Total.StoreC <- rbind( data.dept.Total.StoreC, data.frame("Date"=as.Date(uDate), "Weekly_Sales"= mean(ssTotalC$Weekly_Sales)))
}


plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}


#Run Models for Aggregate Tests
tseries <- ts(data.dept.Total$Weekly_Sales, start=c(2010), end= c(2013),frequency=52)

fitAggreateAutoArima <- auto.arima(tseries, stepwise=FALSE, approximation=FALSE)
plot(fitAggreateAutoArima$x,col="red",xlab = 'Fit Aggreate Auto.Arima',ylab = 'Average Sales of All Stores')
lines(fitted(fitAggreateAutoArima),col="blue")
plotForecastErrors(fitAggreateAutoArima$residuals)

fitAggreate011011 <- Arima(tseries,order=c(0,1,1),seasonal = c(0,1,1))
plot(fitAggreate011011$x,col="red",xlab = 'Fit Aggreate (0,1,1)(0,1,1)',ylab = 'Average Sales of All Stores')
lines(fitted(fitAggreate011011),col="blue")
plotForecastErrors(fitAggreate011011$residuals)

fitAggreate010010 <- Arima(tseries,order=c(0,1,0),seasonal = c(0,1,0))
plot(fitAggreate010010$x,col="red",xlab = 'Fit Aggreate (0,1,0)(0,1,0)',ylab = 'Average Sales of All Stores')
lines(fitted(fitAggreate010010),col="blue")
plotForecastErrors(fitAggreate010010$residuals)

fitAggreate000010 <- Arima(tseries,order=c(0,0,0),seasonal = c(0,1,0))
plot(fitAggreate000010$x,col="red",xlab = 'Fit Aggreate (0,0,0)(0,1,0)',ylab = 'Average Sales of All Stores')
lines(fitted(fitAggreate000010),col="blue")
plotForecastErrors(fitAggreate000010$residuals)



#Run Models for Store Type A
tseriesA <- ts(data.dept.Total.StoreA$Weekly_Sales, start=c(2010,2), end= c(2013),frequency=52)

fitAAutoArima <- auto.arima(tseriesA, stepwise=FALSE, approximation=FALSE)
plot(fitAAutoArima$x,col="red",xlab = 'Fit Store Type-A Auto.Arima',ylab = 'Average Sales of Store Type A')
lines(fitted(fitAAutoArima),col="blue")
plotForecastErrors(fitAAutoArima$residuals)

fitStoreTypeA011011 <- Arima(tseriesA,order=c(0,1,1),seasonal = c(0,1,1))
plot(fitStoreTypeA011011$x,col="red",xlab = 'Fit Store Type-A (0,1,1)(0,1,1)',ylab = 'Average Sales of Store Type A')
lines(fitted(fitStoreTypeA011011),col="blue")
plotForecastErrors(fitStoreTypeA011011$residuals)

fitStoreTypeA010010 <- Arima(tseriesA,order=c(0,1,0),seasonal = c(0,1,0))
plot(fitStoreTypeA010010$x,col="red",xlab = 'Fit Store Type-A (0,1,0)(0,1,0)',ylab = 'Average Sales of Store Type A')
lines(fitted(fitStoreTypeA010010),col="blue")
plotForecastErrors(fitStoreTypeA010010$residuals)

fitStoreTypeA000010 <- Arima(tseriesA,order=c(0,0,0),seasonal = c(0,1,0))
plot(fitStoreTypeA000010$x,col="red",xlab = 'Fit Store Type-A (0,0,0)(0,1,0)',ylab = 'Average Sales of Store Type A')
lines(fitted(fitStoreTypeA000010),col="blue")
plotForecastErrors(fitStoreTypeA000010$residuals)



#Run Models for Store Type B
tseriesB <- ts(data.dept.Total.StoreB$Weekly_Sales, start=c(2010), end= c(2013),frequency=52)

fitBAutoArima <- auto.arima(tseriesB, stepwise=FALSE, approximation=FALSE)
plot(fitBAutoArima$x,col="red",xlab = 'Fit Store Type-B Auto.Arima',ylab = 'Average Sales of Store Type B')
lines(fitted(fitBAutoArima),col="blue")
plotForecastErrors(fitBAutoArima$residuals)

fitStoreTypeB011011 <- Arima(tseriesB,order=c(0,1,1),seasonal = c(0,1,1))
plot(fitStoreTypeB011011$x,col="red",xlab = 'Fit Store Type-B (0,1,1)(0,1,1)',ylab = 'Average Sales of Store Type B')
lines(fitted(fitStoreTypeB011011),col="blue")
plotForecastErrors(fitStoreTypeB011011$residuals)

fitStoreTypeB010010 <- Arima(tseriesB,order=c(0,1,0),seasonal = c(0,1,0))
plot(fitStoreTypeB010010$x,col="red",xlab = 'Fit Store Type-B (0,1,0)(0,1,0)',ylab = 'Average Sales of Store Type B')
lines(fitted(fitStoreTypeB010010),col="blue")
plotForecastErrors(fitStoreTypeB010010$residuals)

fitStoreTypeB000010 <- Arima(tseriesB,order=c(0,0,0),seasonal = c(0,1,0))
plot(fitStoreTypeB000010$x,col="red",xlab = 'Fit Store Type-B (0,0,0)(0,1,0)',ylab = 'Average Sales of Store Type B')
lines(fitted(fitStoreTypeB000010),col="blue")
plotForecastErrors(fitStoreTypeB000010$residuals)



#Run Models for Store Type C
tseriesC <- ts(data.dept.Total.StoreC$Weekly_Sales, start=c(2010), end= c(2013),frequency=52)

fitCAutoArima <- auto.arima(tseriesC, stepwise=FALSE, approximation=FALSE)
plot(fitCAutoArima$x,col="red",xlab = 'Fit Store Type-C Auto.Arima',ylab = 'Average Sales of Store Type C')
lines(fitted(fitCAutoArima),col="blue")
plotForecastErrors(fitCAutoArima$residuals)

fitStoreTypeC011011 <- Arima(tseriesC,order=c(0,1,1),seasonal = c(0,1,1))
plot(fitStoreTypeC011011$x,col="red",xlab = 'Fit Store Type-C (0,1,1)(0,1,1)',ylab = 'Average Sales of Store Type C')
lines(fitted(fitStoreTypeC011011),col="blue")
plotForecastErrors(fitStoreTypeC011011$residuals)

fitStoreTypeC010010 <- Arima(tseriesC,order=c(0,1,0),seasonal = c(0,1,0))
plot(fitStoreTypeC010010$x,col="red",xlab = 'Fit Store Type-C (0,1,0)(0,1,0)',ylab = 'Average Sales of Store Type C')
lines(fitted(fitStoreTypeC010010),col="blue")
plotForecastErrors(fitStoreTypeC010010$residuals)

fitStoreTypeC000010 <- Arima(tseriesC,order=c(0,0,0),seasonal = c(0,1,0))
plot(fitStoreTypeC000010$x,col="red",xlab = 'Fit Store Type-C (0,0,0)(0,1,0)',ylab = 'Average Sales of Store Type C')
lines(fitted(fitStoreTypeC000010),col="blue")
plotForecastErrors(fitStoreTypeC000010$residuals)
