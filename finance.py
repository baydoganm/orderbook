__author__ = 'mehmet'

import requests,json
import time
import urllib

def get_stock_price(company_name):
    query = urllib.quote('select Ask from yahoo.finance.quotes where symbol = "%s"' % company_name)
    api_url = "http://query.yahooapis.com/v1/public/yql?q=" + query + '&env=http://datatables.org/alltables.env&format=json'
    response = requests.get(api_url)
    return float(response.json()['query']['results']['quote']['Ask'])


def get_btc_price():
    url = "https://www.btcturk.com/api/ticker"
    response =  requests.get(url)

    return response.json()
        #float(soup.find(id=splitter).text)

def get_btc_orderbook():
    url="https://www.btcturk.com/api/orderbook"
    try:
        response=requests.get(url)
        respJSON=response.json()
        return (True,respJSON)
     #   return response.json()
    except requests.exceptions.ConnectionError:
        return (False,1)
    except ValueError:
        return (False,response.text)



def get_stock_price_old(company_name):
    url = "http://finance.yahoo.com/q?s=" + company_name
    response =  requests.get(url)

    for x in response.content.splitlines():
        if "time_rtq_ticker" in x:
            splitter = 'yfs_l84_%s">' % company_name
            return float(x.split(splitter)[1].split('<')[0])
