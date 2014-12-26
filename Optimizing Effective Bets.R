library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
 
EffectiveBets = function(R, w) {
  ##By Karl Wichorek
 
  # Perform PCA on the returns
  pca <- prcomp(R, cor = FALSE)
  evec <- pca$rotation[] #eigenvectors
  eigval <- pca$sdev^2 #eigenvalues
 
  # Calculate the factor return streams
  inv.evec <- solve(evec) #inverse of eigenvectors
 
  # Calculate your exposure to each factor
  factor.exposure <- inv.evec %*% w
 
  # Calculate the diversification distribution
  # The numerator is the variance concentration curve
  # The denominator is the total portfolio variance
  div.dist <- (factor.exposure^2)*eigval / sum((factor.exposure^2)*eigval)
 
  # Calculate the effective number of bets
  ENB <- exp(-sum(div.dist*log(div.dist)))
 
  return(ENB)
}
 
 
##Estimate the inv.evec and val first when using optimization w first for easy passing to deoptim
 
EffectiveBets_optim = function(weights,inv.evec,eigval){
  factor.exposure <- inv.evec %*% weights
 
  # Calculate the diversification distribution
  # The numerator is the variance concentration curve
  # The denominator is the total portfolio variance
  div.dist <- (factor.exposure^2)*eigval / sum((factor.exposure^2)*eigval)
 
  # Calculate the effective number of bets
  ENB <- exp(-sum(div.dist*log(div.dist)))
 
  return(1/ENB)
 
}
 
 
 
 
###Canadian ETF LISt
canadian_etfs=c("xiu","xsp","cbo","xsb","XBB","XCB","CPD","XRE","ZHY","CLF","PGL","ZIC")
 
getSymbols(paste0(canadian_etfs,".TO"),from='2000-01-01')
 
etf_name_list = paste0(toupper(canadian_etfs),".TO")
 
###Combine all into a "price matrix"
pricemat=NULL
start_dates = data.frame(etf_name=etf_name_list)
start_dates$date = NA
for(etfs in etf_name_list){
  step1 = Ad(get(paste0(etfs)))
  colnames(step1) = etfs
  pricemat = merge.xts(pricemat,step1)
  start_dates[start_dates$etf_name==etfs,]$date=as.Date(start(step1))
 
 
}
returnmat = ROC(pricemat,type="discrete")
 
 
 
##Fix return data
 
 
 
 
retmat= returnmat['2013-04-01/2015-01-01']
retmat[is.na(retmat)]=0
 
 
 
##Estimate inv.evec
pca <- prcomp(retmat, cor = FALSE)
evec <- pca$rotation[] #eigenvectors
eigval <- pca$sdev^2 #eigenvalues
 
# Calculate the factor return streams
inv.evec <- solve(evec) #inverse of eigenvectors
 
###Use Portfolio Analytics to quickly model the portfolio
moms_portfolio = portfolio.spec(assets=colnames(returnmat))
moms_portfolio = add.constraint(portfolio=moms_portfolio,type="full_investment")
moms_portfolio = add.constraint(portfolio=moms_portfolio,type="long_only")
moms_portfolio = add.objective(portfolio=moms_portfolio,name="EffectiveBets_optim",type="risk",arguments=list(inv.evec=inv.evec,eigval=eigval)) ###Place holder 
 
###Optimize (May want to switch to ROI or different optimizer if dealing with more securities)
 
optimal_portfolio = optimize.portfolio(retmat,moms_portfolio,optimize_method="DEoptim",
                                       search_size=5000, trace=TRUE, traceDE=0)
