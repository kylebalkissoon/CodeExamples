#' Searches for closest to theoretical weights within +/-1 one share (Lot) or the rounded optimal
#' @param optimal_allocation is a vector of the optimal weights
#' @param expected_price is the expected (or actual) execution prices, the time_stamp from that is used to mark the book
#' @param equity is the value of the account
#' @param max_equity_mult is the leverage cap of the account (e.g. 2:1 leverage = 2)
#' @author Kyle Balkissoon {kylebalkissoon@gmail.com}
#' @export




shareSearch = function(optimal_allocation,expected_price,equity,max_equity_mult = 1){
  
 
  
  
  ##Create search bounds (+/- 1 lot for each position)
  upper_search_bound =round(optimal_allocation,0)+rep(1,ncol(optimal_allocation))
  lower_search_bound = round(optimal_allocation,0)-rep(1,ncol(optimal_allocation))
  
  
  ###string for expand grid
  grid_string = ""
  for( syms in colnames(optimal_allocation)){
    step1 = paste0(syms,"=",as.numeric(lower_search_bound[,syms]),":",as.numeric(upper_search_bound[,syms]),",")
    grid_string = paste0(grid_string,step1)
    
    
  }
  ###Search space
  my_search_space = eval(parse(text=paste0("expand.grid(",grid_string,"KEEP.OUT.ATTRS=TRUE)")))
  
  ###Remove infeasible combinations
  equity_check = equity_test(my_search_space,expected_price,equity*max_equity_mult)
  my_search_space = my_search_space[equity_check,]

  ###Compute distance
  my_distance= rowSums((my_search_space-matrix(as.numeric(optimal_allocation),nrow=nrow(my_search_space),ncol=ncol(expected_price),byrow=TRUE))^2)
  
  ###Return the best
  ans = my_search_space[which.min(my_distance),]  
 
  return(ans)
 
 
}


equity_test = function(x,expected_price,max_equity){
  ###Returns a boolean on whether or not the parameters exceed max equity
  
  
  return(ifelse(rowSums(as.matrix(x)*matrix(as.numeric(expected_price[1,]),nrow=nrow(x),ncol=ncol(expected_price),byrow=TRUE))<=max_equity,TRUE,FALSE))
  
  
}
