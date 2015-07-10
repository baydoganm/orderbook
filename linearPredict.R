#first installation of the following packages is required
library(data.table)
library(zoo)
library(rpart)
library(rpart.plot)
library(gbm)
library(randomForest)


rawdata=suppressWarnings(fread('data4.csv'))
clndata=na.omit(rawdata[, lapply(.SD, function(x) replace(x, which(x=='?'), NA))])
clndata[,4:7 := lapply(.SD,as.numeric), .SDcols = 4:7]

source('~/GitHub/orderbook/generate_features.r')

#transform to zoo object
inputdata=zoo(clndata[,-1,with=FALSE])

#plot the zoo object to see if everything is OK
#plot(inputdata)

#date info is still there, remove it for now
inputdata=inputdata[,-c(1:2)]

#plot again
#plot(inputdata)

#if you want to bin data, use aggregate function
#lets say we have 6 observations in a min (which makes a minute in our case)
agglevel=6
#create aggregation indices
aggind=floor((time(inputdata)-1)/agglevel)+1
#aggregate
aggdata=aggregate(inputdata,aggind,mean)
#plot aggregated datra
plot(aggdata)

#go back to prediction\\
#use the generate_features function that is created
#generate_features require a feature mapping, details are available in the function source code
#there are 4 option for feature types, we will use lag features
genfeature=matrix(0,ncol(inputdata),4)
#this is binary representation, we say we will use lag type features for all input information
genfeature[,1]=1 
lags=rep(7,ncol(inputdata))

traindataBid=generate_features(inputdata, forecastVar=1,
                            which.features=genfeature,lagSize=lags)	


#this is a list with two different information:
#features is the data matrix
#days to predict is the corresponding times we do prediction

trainfeaturesBid=traindataBid$features


#try linear regression
lmFitBid=lm(actual~., trainfeaturesBid)

#linear regression on Ask data 
lags=rep(7,ncol(inputdata))
traindataAsk=generate_features(inputdata, forecastVar=3,
                               which.features=genfeature,lagSize=lags) 

trainfeaturesAsk=traindataAsk$features
lmFitAsk=lm(actual~.,trainfeaturesAsk)


#linear regression on Ask data with 2 lag vars
lags=rep(2,ncol(inputdata))
traindataAsk=generate_features(inputdata, forecastVar=3,
                               which.features=genfeature,lagSize=lags) 

trainfeaturesAsk=traindataAsk$features
lmFitAsk2=lm(actual~.,trainfeaturesAsk)

#linear regression on Ask data with 5 lag vars
lags=rep(5,ncol(inputdata))
traindataAsk=generate_features(inputdata, forecastVar=3,
                               which.features=genfeature,lagSize=lags) 

trainfeaturesAsk=traindataAsk$features
lmFitAsk5=lm(actual~.,trainfeaturesAsk)


