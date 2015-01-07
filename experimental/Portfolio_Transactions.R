#' Generates Portfolio Transactions based on shares and expected price 
#'
#' The \code{PortfolioTransactions} function goes through each symbol and adds the transaction
#'
#' Note that the portfolio will be marked on the time stamp of expected price
#'
#'
#' @param Shares_to_buy is a data.frame or xts matrix with the colnames of the symbols and values of the number of shares to buy
#' @param expected_price is the expected (or actual) execution prices, the time_stamp from that is used to mark the book
#' @author Kyle Balkissoon {kylebalkissoon@gmail.com}
#' @export


Portfolio_Transactions = function(Shares_to_buy,expected_price,txn_cost=0){
  ##Shares to buy is a data frame with colnames of the symbol and numbers of the shares
  ##the price is a data frame with colnames of the symbol and numbers of the price
  
  ###Map to Txn
  
  #set up df to apply txn extra matching is here as a failsafe!
  the_txn = data.frame(Symbol = colnames(Shares_to_buy),TxnQty = as.numeric(Shares_to_buy))
  the_price = data.frame(Symbol=colnames(expected_price),TxnPrice = as.numeric(expected_price))
  ##join prices
  my_txn = sqldf('select a.*,b.TxnPrice from the_txn a left join the_price b on a.Symbol=b.Symbol')
  my_txn$Portfolio = "stocks"
  my_txn$TxnDate = index(expected_price)
  
  for(i in 1:nrow(my_txn)){
    addTxn(Portfolio=my_txn[i,]$Portfolio,Symbol=as.character(my_txn[i,]$Symbol),TxnDate=my_txn[i,]$TxnDate,TxnQty=my_txn[i,]$TxnQty,TxnPrice=my_txn[i,]$TxnPrice,TxnFees=txn_cost)
    
    
  }
  return()
}
