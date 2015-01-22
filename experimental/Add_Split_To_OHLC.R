#' Appends a splitted price to the OHLC to correct for potential valuation issues, will append the splited price in an entry in all columns
#' Care must be taken to calculate indicators using the data before this transformation or excluding this
#'
#'
#' Note that the portfolio will be marked on the time stamp of expected price
#'
#'
#' @param symbol is the symbol of the instrument in string format
#' @param newprice is the new price after the split
#' @datetime is the date and time of the split 
#' @author Kyle Balkissoon {kylebalkissoon@gmail.com}
#' @export


Append_Split_To_OHLC = function(symbol,newprice,datetime){
  
  ###Get old xts object
  prev_object = get(symbol)
  new_entry = xts(data.frame(t(rep(newprice,ncol(prev_object)))),datetime)
  colnames(new_entry) = colnames(prev_object)
  new_object = rbind.xts(prev_object,new_entry)
  assign(symbol,value = new_object,pos = .GlobalEnv)
  return()
}