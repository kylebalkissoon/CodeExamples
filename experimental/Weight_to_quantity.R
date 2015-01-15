Weight_to_quantity = function(w,expected_price,equity,allow_fractional=FALSE,search_area=0.05,max_equity_mult=1){
  optimal_allocation = (w*equity)/expected_price
  if(allow_fractional==TRUE){
    return(optimal_allocation)
  }
  
  ##Find closest whole lot trade
  ans = round(optimal_allocation,0)
#   ans = shareSearch(optimal_allocation = optimal_allocation,expected_price = expected_price,share_deviation = search_area,equity = equity,max_equity_mult = max_equity_mult)
  
  return(ans)
  
}
