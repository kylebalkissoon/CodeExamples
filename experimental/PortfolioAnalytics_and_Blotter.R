library(quantmod)
library(PerformanceAnalytics)
library(blotter)
library(quantstrat)
library(PortfolioAnalytics)
library(DEoptim)
library(sqldf)

##Point these to the two files
source(file="C:/Users/Kyle/Documents/Backtester/Portfolio_Transactions.R")
source(file="C:/users/kyle/documents/Backtester/Weight_to_quantity.R")


### US ETF List
symbol_list = c('XLF','XLE','XLU','XLK','XLB','XLP','XLY','XLI','XLV','TLT','GLD')

##Get them
getSymbols(symbol_list, from = '1990-01-01')


###Assemble return matrix for PortfolioAnalytics
combined_price_matrix = NULL
for(syms in symbol_list){
  
  combined_price_matrix=merge.xts(combined_price_matrix,Ad(get(syms)))
  
}

###to fix potential issues down the line all times are reindexed to 5pm
combined_price_matrix = xts(combined_price_matrix,order.by=as.POSIXct(paste0(index(combined_price_matrix)," 17:00:00"))) 
combined_return_matrix = ROC(combined_price_matrix,type='discrete')


colnames(combined_return_matrix) = symbol_list
colnames(combined_price_matrix) = symbol_list
##Subset to arbitary future date
combined_return_matrix = combined_return_matrix['2005-01-01/2015-12-31']


###Set Up Financial Instruments and portfolio Equity
currency("USD")
stock(symbol_list,currency='USD')
equity = 1000000
initial_date = as.Date('2005-12-31')

##Make sure objects don't exist (for testing will throw a warning if objects don't exist)
rm("portfolio.stocks",pos=.blotter)
rm("account.GMV_Example",pos=.blotter)
###Initialize accounting framework

initPortf(name="stocks",symbols=symbol_list,initPosQty=0,initDate=initial_date,currency="USD")
initAcct(name = 'GMV_Example',portfolios = c("stocks"),initDate=initial_date,initEq=equity,currency="USD")



###Set up optimization objective
GMV_Portfolio = portfolio.spec(assets=symbol_list)
GMV_Portfolio = add.constraint(portfolio=GMV_Portfolio,type="full_investment")
GMV_Portfolio = add.constraint(portfolio=GMV_Portfolio,type="long_only")
GMV_Portfolio = add.constraint(portfolio=GMV_Portfolio,type='box',length(symbol_list),min=rep(0,length(symbol_list)),max=rep(.15,length(symbol_list)))
GMV_Portfolio = add.objective(portfolio=GMV_Portfolio,type='risk',name='StdDev')

###Get end of month dates
end_of_month=endpoints(combined_return_matrix[,1],on="months")
last_day_in_the_month=index(combined_return_matrix)[end_of_month]

last_day_in_the_month = as.Date(last_day_in_the_month[13:length(last_day_in_the_month)])

zequity = equity
portfolio_weights = xts(matrix(nrow=nrow(combined_return_matrix),ncol=ncol(combined_return_matrix)),order.by=index(combined_return_matrix))
colnames(portfolio_weights) = colnames(combined_return_matrix)



for(dayz in last_day_in_the_month){

  dayz_date = as.Date(dayz)
  
  date_string = paste0(initial_date,"/",as.Date(dayz))
  
  if(!dayz==last_day_in_the_month[1]){
    ##Update portfolio, account and ending equity
    updatePortf("stocks",Dates=date_string)
    updateAcct(name ="GMV_Example",Dates=date_string)
    updateEndEq("GMV_Example",Dates=date_string)
    
    ###Get new equity to toss into the above for rebalancing
    my_account = getAccount("GMV_Example")
    
    zequity = as.numeric(last(my_account$summary$Net.Trading.PL))+equity
    
    
  }
  
  
  ###Pass new returns into optimization framework
  optimal_weights = optimize.portfolio(combined_return_matrix[paste0("'./",dayz_date-2,"'")],portfolio=GMV_Portfolio)
  
  ###For comparison to compute cash drag
  portfolio_weights[paste0(as.Date(dayz_date)),] = as.numeric(optimal_weights$weights)
  ###Calculate shares to buy based on expected price
  Shares_to_buy = Weight_to_quantity(optimal_weights$weights,combined_price_matrix[paste0(as.Date(dayz_date))],as.numeric(zequity),TRUE)
  
  ###Calculate trades to reach shares to buy
  
  ##UpdatePeriod String
  
  date_string_holdings = paste0(initial_date,"/",as.Date(dayz-1))
  
  My_holdings=blotter:::.getBySymbol(getPortfolio("stocks"),'Pos.Qty',Dates=date_string_holdings)
  current_holdings = last(My_holdings)
  ###this is to fix the initial portfolio being empty
  Trades_to_make = Shares_to_buy-ifelse(sum(current_holdings)==0,0,current_holdings)
  
  
  ###Use executed prices (to simulate slippage, as you won't always get hte price you want)
  Portfolio_Transactions(Trades_to_make,combined_price_matrix[paste0(as.Date(dayz))],txn_cost=-1)
  
  
  
  
  ##Update portfolio, account and ending equity
  updatePortf("stocks",Dates=date_string)
  updateAcct(name ="GMV_Example",Dates=date_string)
  updateEndEq("GMV_Example",Dates=date_string)
  
  ###Get new equity to toss into the above for rebalancing
  my_account = getAccount("GMV_Example")
  
  zequity = as.numeric(last(my_account$summary$Net.Trading.PL))+equity
  
  
 # end_date_string = paste0("/",as.Date(dayz))
  
}




