generate_pbWaitTimes <- 
  function(tsData) {
    delta=diff(tsData)
    chgTimes=diff(which(delta>0|delta<0))
    testPoints=round(seq(0.1*max(chgTimes),0.9*max(chgTimes),length.out=200))
    logProb=vector(mode='numeric',length=length(testPoints))
    
    for (i in 1:length(testPoints)){
      logProb[i]=log(length(which(chgTimes>testPoints[i]))/length(chgTimes))
    }
    
    return (list(probWait=logProb,tPoints=testPoints))
  }


findNumOfRep<-
  function(tsData){
    data.rle=apply(tsData,2,function(x) rle(x))
   return (list(bidOrders=data.rle$'Best Bid'$lengths,askOrders=data.rle$'Best Ask'$lengths))    
  }