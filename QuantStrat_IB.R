############BEGIN###########

#clear workspace (for testing)
rm(list = ls())

#load libraries
library(IBrokers)
library(tawny)
library(quantmod)
library(PerformanceAnalytics)
library(blotter)
library(FinancialInstrument)
try(library(twsInstrument))
try(library(quantstrat))

#Reset TWS

tws<-twsConnect()

#clear portfolio and acct
suppressWarnings(rm("account.stocky","portfolio.stocky",pos=.blotter))
suppressWarnings(rm("order_book.stocky",pos=.strategy))
suppressWarnings(rm(stocky))

#Initialize contract
EURUSD<-getContract("EUR.USD")

############ Get Data ###############
EURUSD<-reqHistoricalData(tws,EURUSD,barSize='15 mins',duration ="20 D", whatToShow="BID")
#close tws
twsDisconnect(tws)
#Set Initial Date and Equity
initDate = start(EURUSD)
initEq = 10000

#Set up currencies
currency("EUR")
currency("USD")
#loop to make exchange rates
symbols =("EURUSD")
for(symbol in symbols) {
exchange_rate(symbol, currency="USD")
}
#Clean up Data from IB
EURUSD<-subset(EURUSD, select= -c(EUR.USD.WAP,EUR.USD.hasGaps,EUR.USD.Count,EUR.USD.Volume))
################## Set up portfolio orders and Acct #################

initPortf(name="stocky",symbols,initPosQty=0,initDate=initDate,currency="USD")
initAcct("stocky",portfolios="stocky",initDate=initDate,initEq=initEq)
initOrders("stocky",symbols,initDate=initDate)

#position limits
addPosLimit("stocky","EURUSD",timestamp=initDate,maxpos=20000, minpos=-20000)

#Set up Strategy
stratstocky<-strategy("stocky")

##############################FUNCTIONS#######################

# PUT YOUR CUSTOM Indicator here
movingavgret<-function(x,n){
step1<-ROC(x)
step2<-SMA(step1,n)
return(step2)
}

########################indicators#############################
stratstocky<-add.indicator(
strategy = stratstocky,
name = "movingavgret",
arguments = list(
x = quote(Cl(mktdata)),
n = 30),
label = "movavg")

################################### Signals ###############

stratstocky<-add.signal(
strategy = stratstocky,
name = "sigThreshold",
arguments = list(
threshold = 0,
column = "movavg",
relationship = "gte",
cross = TRUE),
label = "movavgPOS")

stratstocky<-add.signal(
strategy = stratstocky,
name = "sigThreshold",
arguments = list(
threshold = 0,
column = "movavg",
relationship = "lt",
cross = TRUE),
label = "movavgNEG")
################################## Rules ##################

#Entry Rule
stratstocky<- add.rule(stratstocky,
name = "ruleSignal",
arguments = list(
sigcol = "movavgPOS",
sigval = TRUE,
orderqty = 20000,
ordertype = "market",
orderside = "long",
pricemethod = "market",
replace = FALSE,
TxnFees = -2.50,
osFUN = osMaxPos),
type = "enter",
path.dep = TRUE,
label = "Entry")
#Exit Rules

stratstocky <- add.rule(stratstocky,
name = "ruleSignal",
arguments = list(
sigcol = "movavgNEG",
sigval = TRUE,
orderqty = "all",
ordertype = "market",
orderside = "long",
pricemethod = "market",
replace = FALSE,
TxnFees = -2.50),
type = "exit",
path.dep = TRUE,
label = "Run AWAY!")

############################## Apply Strategy ##########################

out <- applyStrategy(strategy=stratstocky, portfolios="stocky")
updatePortf("stocky")

######################### Portfolio Return Characterics #################
#get portfolio data
portRet <- PortfReturns("stocky")
portRet$Total <- rowSums(portRet, na.rm=TRUE)
charts.PerformanceSummary(portRet$Total)
tradeStats("stocky")[,c("Symbol","Num.Trades","Net.Trading.PL","maxDrawdown")]
chart.Posn("stocky","EURUSD")
