#' Generates Portfolio share quantities based on weights, expected price, portfolio equity and fractional execution or (not)
#'
#' The \code{Weight_to_quantity} function goes through each symbol and adds the transaction
#'
#' 
#'
#'
#' @param w is a vector of weights
#' @param expected_price is the expected (or actual) execution prices
#' @param equity is the portfolio equity
#' @param allow_fractional is to allow fractional shares in the simulation or only whole shares
#' @author Kyle Balkissoon {kylebalkissoon@gmail.com}
#' @export




Weight_to_quantity = function(w,expected_price,equity,allow_fractional=FALSE,max_equity_mult=1,lotSize=100){
  optimal_allocation = (w*equity)/as.numeric(expected_price)
  
  ###Bug fix in the event you have a price of 0 e.g. bankrupt stock 
  if(any(is.infinite(optimal_allocation))){
  optimal_allocation[,is.infinite(optimal_allocation)] = 0}
  if(allow_fractional==TRUE){
    return(optimal_allocation)
  }
  
  ##Find closest whole lot trade
#   ans = shareSearch(optimal_allocation = optimal_allocation,expected_price = expected_price,equity = equity,max_equity_mult = max_equity_mult)
  ans = RoundLotAllocation(optimal_allocation,expected_price = expected_price,equity = equity,lotSize = 100)
  
  return(ans)
  
}
