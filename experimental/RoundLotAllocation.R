RoundLotAllocation= function(optimal_allocation,expected_price,equity,lotSize){
  ##Trims it down
  optimal_allocation_hat = floor(optimal_allocation/lotSize)*lotSize
  optimal_allocation_hat = as.data.frame(  t(optimal_allocation_hat))
  value_diff = as.numeric(optimal_allocation-optimal_allocation_hat)*expected_price
  cash_left = equity-sum(as.numeric(optimal_allocation_hat)*expected_price)
  small_lot_cost=lotSize*expected_price
  
  ###Spend all the money!
  ###Find largest round lot
  while(cash_left>min(small_lot_cost)){
    opportunities=value_diff[,small_lot_cost<cash_left]
    optimal_allocation_hat[,colnames(opportunities)[which.max(opportunities)]] =     optimal_allocation_hat[,colnames(opportunities)[which.max(opportunities)]]+100
    
    value_diff = as.numeric(optimal_allocation-optimal_allocation_hat)*expected_price
    
   
    cash_left = equity-sum(as.numeric(optimal_allocation_hat)*expected_price)
    
    
  }
  
  return(optimal_allocation_hat)
}