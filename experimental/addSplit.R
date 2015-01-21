#' Add split transactions to a portfolio.
#' 
#' Splits affect position quantity
#' 
#' @param Portfolio  A portfolio name that points to a portfolio object structured with \code{\link{initPortf}}.
#' @param Symbol An instrument identifier for a symbol included in the portfolio, e.g., IBM.
#' @param TxnDate  Transaction date as ISO 8601, e.g., '2008-09-01' or '2010-01-05 09:54:23.12345'.
#' @param SplitRatio Ratio of which to split, e.g. a 2 for 1 split is 2, a 5 to 1 reverse split is 0.2 (1/5)
#' @param CashInLieuPrice Price at which investor receives cash for partial shares
#' @param \dots Any other passthrough parameters.
#' @param TxnFees Fees associated with the transaction, e.g. commissions. See Details.
#' @param verbose If TRUE (default) the function prints the elements of the transaction in a line to the screen, e.g., "2007-01-08 IBM 50 @@ 77.6". Suppress using FALSE.
#' @param ConMult Contract or instrument multiplier for the Symbol if it is not defined in an instrument specification.
#' @author Kyle Balkissoon based off addTxn by  Peter Carl and Brian G. Peterson
#' @export
#' @note
#' # TODO add TxnTypes to $txn table
#' 
#' # TODO add AsOfDate 
#' 
addSplit <- function(Portfolio, Symbol, TxnDate, SplitRatio,CashInLieuPrice=0, ..., TxnFees=0, ConMult=NULL, verbose=TRUE)
{ # @author Peter Carl
  pname <- Portfolio
  #If there is no table for the symbol then create a new one
  if(is.null(.getPortfolio(pname)$symbols[[Symbol]]))
    addPortfInstr(Portfolio=pname, symbols=Symbol)
  Portfolio <- .getPortfolio(pname)
  
  if(is.null(ConMult) | !hasArg(ConMult)){
    tmp_instr<-try(getInstrument(Symbol), silent=TRUE)
    if(inherits(tmp_instr,"try-error") | !is.instrument(tmp_instr)){
      warning(paste("Instrument",Symbol," not found, using contract multiplier of 1"))
      ConMult<-1
    } else {
      ConMult<-tmp_instr$multiplier
    }
  }
  
  # FUNCTION
  # 
  TxnQty = 0
  TxnPrice = 0
  #TxnType = "Split"
  # TODO add TxnTypes to $txn table
  
  # Get the current position quantity
  PrevPosQty = getPosQty(pname, Symbol, TxnDate)
  ##Check if there is going to be cash in lieu
  if((SplitRatio*PrevPosQty)%%1>0){
    ##Calculate cash in lieu
    PosQty = floor(SplitRatio*PrevPosQty)
    CashInLieu = ((SplitRatio*PrevPosQty)-PosQty)*CashInLieuPrice
    
    
  }else{
  PosQty = PrevPosQty*SplitRatio
  CashInLieu=0}
  # Calculate the value and average cost of the transaction
  # The -1 multiplier allows a positive value to create a
  # positive realized gain
  TxnValue = -CashInLieu # Calc total cash received
  TxnAvgCost = TxnFees
  
  #Update the Positions Average Cost
  PrevPosAvgCost = blotter:::.getPosAvgCost(pname, Symbol, TxnDate)
  PosAvgCost = ((PrevPosAvgCost*PrevPosQty)-CashInLieu)/PosQty # but carry it forward in $txn
  
  # Calculate any realized profit or loss (net of fees) from the transaction, 
  GrossTxnRealizedPL = CashInLieu
  NetTxnRealizedPL =GrossTxnRealizedPL- TxnFees
  
  # Store the transaction and calculations
  NewTxn = xts(t(c(TxnQty, TxnPrice, TxnValue, TxnAvgCost, PosQty, PosAvgCost, GrossTxnRealizedPL, TxnFees, NetTxnRealizedPL, ConMult)), order.by=as.POSIXct(TxnDate))
  #colnames(NewTxns) = c('Txn.Qty', 'Txn.Price', 'Txn.Value', 'Txn.Avg.Cost', 'Pos.Qty', 'Pos.Avg.Cost', 'Gross.Txn.Realized.PL', 'Txn.Fees', 'Net.Txn.Realized.PL', 'Con.Mult')
  Portfolio$symbols[[Symbol]]$txn<-rbind(Portfolio$symbols[[Symbol]]$txn, NewTxn)
  
  if(verbose)
    print(paste(TxnDate, Symbol, "Split", SplitRatio, "on", PrevPosQty, "shares:", PosQty,"Cash In Lieu Received:",CashInLieu, sep=" "))
  #print(Portfolio$symbols[[Symbol]]$txn)
}
