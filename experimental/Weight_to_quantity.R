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




Weight_to_quantity = function(w,expected_price,equity,allow_fractional=FALSE,search_area=0.05,max_equity_mult=1){
  ##Optimal share lots = 
  optimal_allocation = (w*equity)/expected_price
  if(allow_fractional==TRUE){
    return(optimal_allocation)
  }
  
  ans = shareSearch(optimal_allocation,expected_price,search_area,equity,max_equity_mult)

  
  return(ans)
  
}
