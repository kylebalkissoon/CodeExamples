library(quantmod)
library(PerformanceAnalytics)
library(blotter)
library(quantstrat)
library(PortfolioAnalytics)
library(DEoptim)
library(sqldf)

##Point these to the two files
source(file="https://raw.githubusercontent.com/kylebalkissoon/CodeExamples/master/experimental/Portfolio_Transactions.R")
source(file="https://raw.githubusercontent.com/kylebalkissoon/CodeExamples/master/experimental/Weight_to_quantity.R")
source(file="https://raw.githubusercontent.com/kylebalkissoon/CodeExamples/master/experimental/optimize_portfolio_and_make_transactions.R")
source(file="https://raw.githubusercontent.com/kylebalkissoon/CodeExamples/master/experimental/shareSearch.R")


### US ETF List
symbol_list = c('XLF','XLE','XLU','XLK','XLB','XLP','XLY','XLI','XLV','TLT','GLD')

##Get them
getSymbols(symbol_list, from = '1990-01-01')


###Assemble return matrix for PortfolioAnalytics
combined_price_matrix = NULL
for(syms in symbol_list){
  
  combined_price_matrix=merge.xts(combined_price_matrix,Cl(get(syms)))
  
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
equity = 100000000
initial_date = as.POSIXct('2005-12-31 17:00:00')

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


monthly_returns = combined_price_matrix[last_day_in_the_month,]
monthly_returns = ROC(monthly_returns,type='discrete')


last_day_in_the_month = as.Date(last_day_in_the_month[13:length(last_day_in_the_month)])

account_value = equity
portfolio_weights = xts(matrix(nrow=nrow(combined_return_matrix),ncol=ncol(combined_return_matrix)),order.by=index(combined_return_matrix))
colnames(portfolio_weights) = colnames(combined_return_matrix)



for(i in 1:length(last_day_in_the_month)){
  dayz=last_day_in_the_month[i]
  
  
  if(!dayz==last_day_in_the_month[1]){
    
    
    date_string = paste0(as.Date(last_day_in_the_month[i-1]),"/",as.Date(dayz))
    
    ##Update portfolio, account and ending equity
    updatePortf("stocks",Dates=date_string)
    updateAcct(name ="GMV_Example",Dates=date_string)
    updateEndEq(Account="GMV_Example",date_string)
    
    
  }
  portfolio_weights[paste0(as.Date(dayz)),]= optimize_portfolio_and_make_transactions(R=combined_return_matrix[paste0("'./",dayz-2,"'")],Portfolio.PortA = GMV_Portfolio,Portfolio.Blotter = "stocks",Account.Blotter = "GMV_Example",Expected_Execution_Prices = combined_price_matrix[paste0(as.Date(dayz))],Actual_Execution_Prices = combined_price_matrix[paste0(as.Date(dayz))],allowFractional=FALSE,search_area = 0.005)
}


###update to today
updatePortf("stocks",Dates=)
updateAcct(name ="GMV_Example",Dates=Sys.Date())
updateEndEq(Account="GMV_Example",Sys.Date())



my_account=getAccount("GMV_Example")

###Cash Drag
dollars_invested=my_account$portfolios$stocks$Long.Value

my_equity = my_account$summary$End.Eq/equity
the_weights = portfolio_weights[!is.na(rowSums(portfolio_weights)),]




ret_2_nav = function(x){
  init_nav = rep(100,ncol(x))
  ans_mat = xts(matrix(nrow=nrow(x),ncol=ncol(x)),order.by=index(x))
  
  ans_mat[1,] = as.numeric(init_nav*(1+x[1,]))
  for(i in 2:nrow(ans_mat)){
    ans_mat[i,] = as.numeric(ans_mat[i-1,])*as.numeric(1+x[i,])
    
  }
  colnames(ans_mat) = colnames(x)
  return(ans_mat)
  
}
###Strip out time
daily_acct_equity = to.daily(my_equity)


equity_curve_matrix = merge.xts(Cl(daily_acct_equity))/100)
equity_curve_matrix=equity_curve_matrix[!is.na(equity_curve_matrix[,1]),]
colnames(equity_curve_matrix) = c('Price Space')

##Note returns are end of month and weights are at the exact point in time, pushing the weights to the next month to make them multiply correctly
monthly_portfolio_return = xts(rowSums(monthly_returns['2008/2014']*lag(the_weights['2008/2014'],1)),order.by=index(monthly_returns['2008/2014']))
##Use this date range due to trunctation issues
chart.CumReturns(ROC(equity_curve_matrix,type='discrete')['2009-01-01/2015-01-01'],legend.loc='topleft',main='Price based backtesting')
