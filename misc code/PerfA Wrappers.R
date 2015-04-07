perfstats = function(x){ isins = colnames(x)
                         
                         volstat = StdDev(x)
                         volstat = (as.numeric(volstat))
                         volranks = as.numeric(rank(volstat))
                         
                         sharpestat = SharpeRatio(x,FUN=c("StdDev"))
                         sharpestat = (as.numeric(sharpestat))
                         sharperanks = as.numeric(rank(sharpestat))
                         
                         esstat = ES(x)
                         esstat = (as.numeric(esstat))
                         esrank = as.numeric(rank(esstat))
                         
                         semistat = SemiDeviation(x)
                         semistat = (as.numeric(semistat))
                         semirank = as.numeric(rank(semistat))
                         
                         cumretstat = Return.cumulative(x)
                         cumretstat = (as.numeric(cumretstat))
                         cumretrank = as.numeric(rank(cumretstat))
                         
                         avgstat = colMeans(x)
                         avgstat = (as.numeric(avgstat))
                         avgstatrank = as.numeric(rank(avgstat))
                         
                         
                         skewstat = skewness(x)
                         skewstat = (as.numeric(skewstat))
                         skewrank = as.numeric(rank(skewstat))
                         
                         kurtstat = kurtosis(x)
                         kurtstat = (as.numeric(kurtstat))
                         kurtrank = as.numeric(rank(kurtstat))
                         
                         ##LPM calc
                         lpm_ans_risk_neutral = last(x)
                         lpm_ans_risk_neutral[1,] = 0
                         lpm_ans_risk_averse = lpm_ans_risk_neutral
                         lpm_ans_risk_seeking = lpm_ans_risk_neutral
                         
                         for(i in 1:ncol(x)){
                           lpm_ans_risk_neutral[,i] = lpm(x[,i],n=1,0,about_mean=TRUE)
                           lpm_ans_risk_averse[,i] = lpm(x[,i],n=3/2,0,about_mean=TRUE)
                           lpm_ans_risk_seeking[,i] = lpm(x[,i],n=1/2,0,about_mean=TRUE)
                         }
                         
                         lpm_ans_risk_neutral = as.numeric(lpm_ans_risk_neutral)
                         lpm_ans_risk_averse = as.numeric(lpm_ans_risk_averse)
                         lpm_ans_risk_seeking = as.numeric(lpm_ans_risk_seeking)
                         
                         lpm_ans_risk_neutral_ranks = rank(as.numeric(lpm_ans_risk_neutral))
                         lpm_ans_risk_averse_ranks = rank(as.numeric(lpm_ans_risk_averse))
                         lpm_ans_risk_seeking_ranks = rank(as.numeric(lpm_ans_risk_seeking))
                         
                         MAD = MeanAbsoluteDeviation(x)
                         MAD = as.numeric(MAD)
                         MAD_rank = rank(as.numeric(MAD))
                         
                         resultrank = data.frame(isins,volranks,sharperanks,esrank,semirank,cumretrank,avgstatrank,skewrank,kurtrank,lpm_ans_risk_neutral_ranks,lpm_ans_risk_averse_ranks,lpm_ans_risk_seeking_ranks,MAD_rank)
                         
                         result = data.frame(isins,volstat,sharpestat,esstat,semistat,cumretstat,avgstat,skewstat,kurtstat,lpm_ans_risk_neutral,lpm_ans_risk_averse,lpm_ans_risk_seeking,MAD)
                         return(resultrank)
}




beta_squared = function(x,y){
  y_fixed = y^2*sign(y)
  x_ans = last(x)
  x_ans[1,] = 0
  for ( i in 1:ncol(x)){
    x_fixed = x[,i]^2*sign(x[,i])  
    reg_step = lm(x_fixed~y_fixed)
    step2 = coef(reg_step)[2]
    x_ans[,i] = as.numeric(step2)    
  }
  
  return(x_ans)
  
  
}

beta_cubed = function(x,y){
  y_fixed = y^3
  x_ans = last(x)
  x_ans[1,] = 0
  for ( i in 1:ncol(x)){
    x_fixed = x[,i]^3  
    reg_step = lm(x_fixed~y_fixed)
    step2 = coef(reg_step)[2]
    x_ans[,i] = as.numeric(step2)    
  }
  
  return(x_ans)
  
  
}