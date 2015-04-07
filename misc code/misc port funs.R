Portfolio_rebalancer = function(x,initw)
{
  w= xts(matrix(ncol=ncol(x),nrow=nrow(x)),order.by=index(x))
  colnames(w) = colnames(x)
  w[1,] = as.numeric(t(initw))
  if(nrow(x)==1){}else{
    for (i in 2:nrow(x))
    {
      w[i,] = w[i-1,]*(1+x[i-1,])
      
      w[i,] = w[i,]/sum(as.numeric(w[i,]))
    }}
  return(w)
}


portfolio_ew_rebalancer = function(x,xweek,rebal){
  w= xts(matrix(ncol=ncol(x),nrow=nrow(x)),order.by=index(x))
  colnames(w) = colnames(x)
  start_date = first(index(x))-40
  w[1,] = 1/ncol(xweek)
  if(nrow(x)==1){}else{
    for(i in 2:nrow(x)){
      if((i-1)%%rebal==0){
        start_date = last(first(index(x),i))-40
        w[i,] =  1/ncol(xweek)
      }else{
        w[i,] = (1+as.numeric(x[i-1,]))*as.numeric(w[i-1,])
        w[i,] = w[i,]/sum(w[i,])
        
      }
      
    }}
  return(w)                                                      
}


round_and_eliminate2 = function(x,r){
  if(ncol(x)<=r){step5=x}else{
    step0 = rank(as.numeric(x))
    step2 = data.frame(step0,as.numeric(x))
    step3 = step2[step2[,1]==(ncol(x)-r+1),][,2]
    step4 = ifelse(x<step3,0,x)
    step5 = step4/sum(step4)}
  return(step5)
  
  
}

weight_check = function(x){
  ##Normalize Weights
  return((x/sum(x)))
  
}
