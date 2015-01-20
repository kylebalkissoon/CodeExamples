#' Wrapper function to apply a portfolio analytics optimization 
#' to a return series, generate weights, shares, trades and executions.
#'
#' @param R is a matrix of returns
#' @param Portfolio.PortA is the PortfolioAnalytics Portfolio object
#' @param Portfolio.Blotter is a string containing the name of the blotter portfolio for which the trades will be made
#' @param Account.Blotter is a string containing the name of the blotter account in which the portfolio sits
#' @param Expected_Execution_Prices is the expected prices or valuations on which weights will be transformed to shares
#' @param Actual_Execution_Prices is the Prices at which the trades have actually executed it 
#' @param allowFractional is a boolean to allow fractional share units (e.g. an index or simulation)
#' @param TxnCost Flat transaction fee PER TRADE 
#' @param TxnCostPerShare transaction fee per share (Not currently supported)
#' @param ManagementFeeBps Management Fee to be applied (Not currently supported!)
#' @author Kyle Balkissoon {kylebalkissoon@gmail.com}
#' @export


optimize_portfolio_and_make_transactions = function(R,
                                                    Portfolio.PortA,
                                                    Portfolio.Blotter,
                                                    Account.Blotter,
                                                    Expected_Execution_Prices,
                                                    Actual_Execution_Prices,
                                                    max_equity_mult=1,
                                                    allowFractional=TRUE,TxnCost=0,TxnCostPerShare=0,ManagementFeesBps=0){
  
  ###Safety Tests
  if(!is.xts(R)){stop("R must be an XTS object")}
  
  if(!is.portfolio(Portfolio.PortA)){stop("Portfolio.PortA must be a PortfolioAnalytics Portfolio Object")}
  
  if(!is.character(Portfolio.Blotter)){stop("Portfolio.Blotter must be a string that is the name of the blotter portfolio")}
  
  if(!is.character(Account.Blotter)){stop("Account.Blotter must be a string that is the name of the blotter portfolio")}
  
  if(!is.xts(Expected_Execution_Prices)){stop("Expected_Execution_Prices must be an xts object")}
  
  if(!is.xts(Actual_Execution_Prices)){stop("Actual_Execution_Prices must be an xts object")}
  
  
  
  ###Note Potential issue here due to the dates book may need to be updated
  My_holdings=blotter:::.getBySymbol(getPortfolio(Portfolio.Blotter),'Pos.Qty',Dates=index(R))
  
  current_holdings = last(My_holdings)
  
  ###Only for testing
  #if(any(current_holdings<0)){
  #  stop("why is there a short holding")
  #}
  account_value = getEndEq(Account.Blotter,index(R))
  ##If you got no money, you can't invest!
  if(as.numeric(account_value)<=0)stop("BANKRUPT")
  
  #@TODO write this fn
  optimal_weights = optimize.portfolio(R,portfolio=Portfolio.PortA)
  #   portfolio_weights[paste0(as.Date(index(R))),] = as.numeric(optimal_weights$weights)
  

    
  Shares_to_buy = Weight_to_quantity(w = optimal_weights$weights,
                                     expected_price = Expected_Execution_Prices[1,],
                                     equity=as.numeric(account_value),
                                     allow_fractional = allowFractional,
                                     max_equity_mult = max_equity_mult)
  
  
  ###Calculate trades to reach shares to buy
  ###Misordering bug fix
  if(sum(current_holdings)==0){Trades_to_make = Shares_to_buy
  }else{
    current_holdings = current_holdings[,colnames(Shares_to_buy)]
    Trades_to_make = Shares_to_buy-as.numeric(current_holdings)
    
  }
  
  
  ###Use executed prices (to simulate slippage, as you won't always get hte price you want)
  #@TODO Portfolio_Transactions to be updated to take in costs per share
  Portfolio_Transactions(Trades_to_make,Actual_Execution_Prices,txn_cost=TxnCost)
  
  return(optimal_weights$weights)
  
}
