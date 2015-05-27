
import finance,time,tablib,csv
from datetime import datetime


#data=tablib.Dataset()
headers=['Index','Date','Time','Best Bid','Bid Amount','Best Ask','Ask Amount']
#data.headers=['Index','Date','Time','Best Bid','Bid Amount','Best Ask','Ask Amount']
freq=10     #sets the sampling rate
endInd=168*60*(60/freq)  #sets total record time

with open('data4.csv','w') as f:
    writer=csv.writer(f,dialect="excel")
    writer.writerow(headers)

    for ind in range(1,endInd+1):
        entData=finance.get_btc_orderbook()
        locTime1=datetime.now()
        (dt,tm)=(locTime1.strftime('%Y%m%d&%H%M%S')).split('&')
        if (entData[0]==True):
            relData=[ind,dt,tm,float(entData[1]['bids'][0][0]),float(entData[1]['bids'][0][1]),
                       float(entData[1]['asks'][0][0]),float(entData[1]['asks'][0][1])]
        else:
            relData=[ind,dt,tm,'?','?','?','?']
			
        #data.append(relData)
        writer.writerow(relData)
	print "%s\n" % relData
        time.sleep(freq)
		

f.close()



#
# stTime=t1   #record start time
# dataInd=1
# relData=(int(dataInd),dt, tm,float(entData['bids'][0][0]),float(entData['bids'][0][1]),
#                   float(entData['asks'][0][0]),float(entData['asks'][0][1]))
# data.append(relData)
# data.headers=['Index','Date','Time','Best Bid','Bid Amount','Best Ask','Ask Amount']
#
# while True:
#
#     entData=finance.get_btc_orderbook()
#     t2=datetime.fromtimestamp(entData['timestamp'])
#     print t2.second
#     deltat=abs((t2-t1).total_seconds())
#
#     t1=t2   #update old time index
#
#     if(deltat<2 and deltat>0.9):
#          dataInd+=1
#          (dt,tm)=(t2.strftime('%Y%m%d&%H%M%S')).split('&')
#          relData=(int(dataInd),dt, tm, float(entData['bids'][0][0]),float(entData['bids'][0][1]),
#                   float(entData['asks'][0][0]),float(entData['asks'][0][1]))
#          data.append(relData)
#
#     if(abs((t2-stTime).total_seconds())>10):
#         break
#
# print dataInd

# with open('data.csv','wb') as f:
#     f.write(data.csv)





#fatih_portfolio = [["aapl", 100], ["msft", 300], ["intc", 250]]

# print finance.calculate_total_portfolio(fatih_portfolio)
#
#
# ustun_portfolio = [["aapl", 150], ["twtr", 300], ["fb", 250]]
#
# #print calculate_total_portfolio(ustun_portfolio)
#




#print get_stock_price("aapl")
# print get_stock_price("twtr")