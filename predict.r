#first installation of the following packages is required
library(data.table)
library(zoo)
library(rpart)
library(rpart.plot)
library(gbm)
library(randomForest)


rawdata=fread('data4.csv')
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

traindata=generate_features(inputdata, forecastVar=ncol(inputdata),
					which.features=genfeature,lagSize=lags)	

					
#this is a list with two different information:
#features is the data matrix
#days to predict is the corresponding times we do prediction

trainfeatures=traindata$features

#try linear regression
lmFit=lm(actual~., trainfeatures)

#try a linear regression with log-transformed target variable
lmFitLog=lm(log(actual)~., cltemptrain)	
	
#fit a regression tree, result can be interesting
treeFit=rpart(actual~., trainfeatures)
prp(treeFit,type=1,extra=1, digit=0, varlen=0, ni=TRUE,faclen=0)

#all regressions use lag of ask amount, let's drop the lags
#check 
str(trainfeatures)
#lags are between ncol(trainfeatures)-7 and ncol(trainfeatures)-1  
reducedtrainfeatures=trainfeatures[,-c((ncol(trainfeatures)-7):(ncol(trainfeatures)-1))]

#fit a regression tree to reduced set
treeFit=rpart(actual~., reducedtrainfeatures)
prp(treeFit,type=1,extra=1, digit=0, varlen=0, ni=TRUE,faclen=0)

#fit a linear regression
#try linear regression
lmFit=lm(actual~., reducedtrainfeatures)

#try gradient boosting model (gbm)
#parameters of gbm
penalty='gaussian'
int.depth=5
bagf=0.5
noftrees=750
shrink=0.01
gbmfit=gbm(actual~., reducedtrainfeatures,interaction.depth=int.depth,shrinkage=shrink,n.trees=noftrees,
		distribution=penalty, keep.data=F, bag.fraction=bagf,verbose=T)		
summary(gbmfit)

#try random forest	
rfFit=randomForest(reducedtrainfeatures[,-ncol(reducedtrainfeatures)],reducedtrainfeatures$actual,ntree=100)	
#check the result
rfFit	
#check variable importance
varImpPlot(rfFit)

#what if we test
#use daysToPredict to seperate training and test data
#current set has 7000 entries let's use 5000 as training and last 2000 as testing
trainindices=which(traindata$daysToPredict<=5000)
testindices=which(traindata$daysToPredict>5000)

newtrain=reducedtrainfeatures[trainindices,]
rfFit=randomForest(newtrain[,-ncol(reducedtrainfeatures)],newtrain$actual,ntree=100)

newtest=reducedtrainfeatures[testindices,]
predicted=predict(rfFit,newtest[,-ncol(reducedtrainfeatures)])
plot(newtest$actual,predicted)
