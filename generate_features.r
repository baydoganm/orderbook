generate_features <- 
		function(timeSeriesData,
				 forecastVar=NULL,
				 dayInfo=NULL,
				 which.features=NULL,	# matrix of size nfeatures and nfeature types
				 perStart=NULL,
				 perEnd=NULL,
				 lagSize=NULL,	#lag size for each feature in ''timeSeriesData''
				 forecastAhead=1,
				 verbose=FALSE,
				 nPeriods=7,	# aggregation level for aggregated figures
				 nFeatureType=10,
				 maxFeatSize=100) {
	

	if(!is.zoo(timeSeriesData)){
		noZooObject="Data object, timeSeriesData and/or dayInfo, should be of type zoo"
		stop(noZooObject)
	}
	if(!is.zoo(dayInfo)){
		noDayInfo="No information about daily effects is provided"
		warning(noDayInfo)
	}
	
	if(is.null(dim(timeSeriesData))){
		timeSeriesData=as.matrix(timeSeriesData)
	}
	
	nofVariables=ncol(timeSeriesData)
	nofDataPoints=nrow(timeSeriesData)	
	
	if(is.null(forecastVar)){
		noForecastVar="The index of the forecasted variable was not provided. Execution stopped"
		stop(noForecastVar)
	}
	if(is.null(perStart)){
		noPerStart="Start date for feature generation was not provided, setting it to"
		warning(paste(noPerStart,start(timeSeriesData)))
		perStart=start(timeSeriesData)
	}
	if(is.null(perEnd)){
		noPerEnd="End date for feature generation was not provided, setting it to"
		warning(paste(noPerEnd,end(timeSeriesData)))
		perEnd=end(timeSeriesData)
	}				
	if(perEnd<perStart){
		endGreaterStart="End of training period is earlier than the start of training. Execution stopped."
		stop(ndGreaterStart)
	}
	if(is.null(forecastAhead)){
		noForecastAhead="How many periods (i.e. days) to forecast ahead was not provided, setting it to default as 1"
		warning(noForecastAhead)
		noForecastAhead=1
	}
	if(is.null(lagSize)){
		noLagSize="No lag size was provided, setting it to default as 1 period for all variables."
		warning(noLagSize)
		lagSize=array(1,nofVariables)
	} else if (nofVariables>length(lagSize)){
		missingLagSize="Lag size for the following variables is set to 1: "
		for(k in (length(lagSize)+1):(nofVariables-1)){
			missingLagSize=paste0(missingLagSize,names(timeSeriesData)[k],', ') 
		}
		missingLagSize=paste0(missingLagSize,names(timeSeriesData)[k]) 
		warning(missingLagSize)
	}
	if(is.null(which.features) || nofVariables>nrow(which.features)){
		noFeatures="No feature type was provided or missing for some variables, setting it to lag type for all variables."
		warning(noFeatures)
		which.features=matrix(0,nofVariables,nFeatureType)
		which.features[,1]=1
	} 
	
	maxLagSize=max(lagSize)
	if(nofDataPoints<=maxLagSize){
		notEnoughObs="Maximum specified lag size is larger than or equal to number of observations. Cannot create features!"
		return(NULL)
		stop(notEnoughObs)
	}
	
	# of feature types
	nofFeatureType=ncol(which.features)
	# number of instances in the feature matrix
	nofRows=nofDataPoints
	
	# number of features in the feature matrix
	nofCols=maxFeatSize # will be calculated based on the selection of feature types
	
	features=matrix(NA,nofRows,nofCols)
	
	windowSeries=suppressWarnings(window(timeSeriesData, start=perStart, end=perEnd))
	nofVariables=ncol(windowSeries)
	nofRowsFeature=nofDataPoints-max(lagSize)-forecastAhead+1
	nameInfo=NULL
	#print(max(lagSize))
	featureind=0
	for(i in 1:nofVariables){
		for(k in 1:nofFeatureType){
			if(which.features[i,k]>0){
				if(k==1){	# lag tipi featurelar
					if(lagSize[i]>0){
						for(l in 1:lagSize[i]){
							featureind=featureind+1
							st=max(lagSize)+forecastAhead-l
							en=st+nofRowsFeature-1
							lagged=windowSeries[st:en,i]
							features[1:length(lagged),featureind]=lagged
							nameInfo=c(nameInfo,paste0(names(windowSeries)[i],"_lag",l))
						}
					}
				}
				if(k==2){	# average tipi featurelar
					temp=zoo::rollapply(windowSeries[,i],nPeriods,mean)
					for(l in 1:lagSize[i]){
						featureind=featureind+1
						st=max(lagSize)+forecastAhead-l
						en=st+nofRowsFeature-1
						lagged=temp[st:en]
						features[1:length(lagged),featureind]=lagged
						nameInfo=c(nameInfo,paste0(names(windowSeries)[i],"_mean",nPeriods,"_lag",l))
					}
				}
				if(k==3){	# tarih ozelligi tipi featurelar
					featureind=featureind+1
					st=max(lagSize)+forecastAhead
					en=st+nofRowsFeature-1
					lagged=windowSeries[st:en,i]
					features[1:length(lagged),featureind]=lagged
					nameInfo=c(nameInfo,paste0(names(windowSeries)[i],"_lag0"))
				}
				if(k==4){	# tarih lag tipi featurelar
					for(l in 1:lagSize[i]){
						featureind=featureind+1
						st=max(lagSize)+forecastAhead-l
						en=st+nofRowsFeature-1
						lagged=windowSeries[st:en,i]
						features[1:length(lagged),featureind]=lagged
						nameInfo=c(nameInfo,paste0(names(windowSeries)[i],"_lag",l))
					}
				}
			}
		}
	}
	
	featureind=featureind+1
	toForecast=windowSeries[-(1:(nofDataPoints-nofRowsFeature)),forecastVar]
#	toForecast=windowSeries[-(1:(max(lagSize)+forecastAhead-1)),forecastVar]
	features[1:length(toForecast),featureind]=toForecast
	#ind=which(!is.na(features[1,]))
	feat=data.frame(features[1:nofRowsFeature,1:featureind])
	names(feat)[1:(ncol(feat)-1)]=nameInfo
	names(feat)[ncol(feat)]='actual'
	#ind=which(is.na(feat),arr.ind=TRUE)
	#feat=feat[1:(min(ind[,1])-1),]
	return(list(features=feat,daysToPredict=time(toForecast)))
#	NAind=which(is.na(features),arr.ind=TRUE)
	#return(features[1:min(NAind[,1]),])
	#input_list <- list(...)	
	#if(!is.null(input_list)){
	#	lapply(input_list,print)
	#}				
								
}
