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

round_to_nearest_lot = function(w){
  return(round(w,0))
  
  
}

weight_diff = function(x,y){
  return(sum(abs(x-y)))
}


Weight_to_quantity = function(w,expected_price,equity,allow_fractional=FALSE){
  ##Optimal share lots = 
  optimal_allocation = (w*equity)/expected_price
  if(allow_fractional==TRUE){
    return(optimal_allocation)
  }
  ##Create bounds using rounded 10% of shares
  bound_adjuster = round(0.05*optimal_allocation,0)
  ##@TODO find a faster optimizer than has a fnMap
  temp_ans= DEoptim(weight_diff,lower=optimal_allocation-bound_adjuster,upper=optimal_allocation+bound_adjuster,fnMap=round_to_nearest_lot,y=optimal_allocation,
                    control=DEoptim.control(strategy=6,c=0.7,itermax=2000))
  return(temp_ans$bestmem)
  
}
