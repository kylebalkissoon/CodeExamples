Calc_Turnover = function(x,y){
  
  x = last(x)
  y = last(y)
  
  x_df = data.frame(colnames(x),as.numeric(x))
  colnames(x_df) = c('ident','weight')
  y_df = data.frame(colnames(y),as.numeric(y))
  colnames(y_df) = c('ident','weight')
  match_df = sqldf('select a.*, b.weight from x_df a left join y_df b on a.ident=b.ident')
  te = match_df[,2] - ifelse(is.na(match_df[,3]),0,match_df[,3])
  sum_te = sum(abs(te))/2
  
  return(sum_te)
}






Calc_rebalancing_turnover_in_year = function(wmat,retmat,rebal){
  ##Calculate differences
  w_prime = end_of_month_weights(wmat,retmat)
  
  turnover = 0
  for(i in 2:nrow(wmat)){
    if((i-1)%%rebal==0){
      tdf = data.frame(weights=as.numeric(wmat[i,]),names=colnames(wmat))
      tdf2 = data.frame(eweights=as.numeric(w_prime[i-1,]),names=colnames(wmat))
      step0 = sqldf('select a.weights,b.eweights, a.names from tdf a left join tdf2 b on a.names=b.names')
      step1 = sum(abs(step0$weights-step0$eweights))
      turnover = turnover+step1
    }else{    }
    
    
  }
  
  return(turnover)
  
  
}

end_of_month_weights = function(w,retmat){
  retmat_subset = retmat[,colnames(retmat)%in%colnames(w)]
  w_prime = w
  for(i in 1:nrow(w)){
    w_prime[i,] = (retmat_subset[i,]+1)*w[i,]
    w_prime[i,] = w_prime[i,]/sum(w_prime[i,])
    
    
  }
  return(w_prime)  
}

make_a_ton_of_charts = function(xts_matrix,cset,bm,folder_name,width_c=1280,height_c=768){
  
  chart_name1 = "chart_cumret.jpg"  
  jpeg(paste0(folder_name,chart_name1),width=width_c,height=height_c)
  plot.new()
  chart.CumReturns(xts_matrix,legend.loc='topleft',
                   wealth.index=T,main='Performance of various strategies',cex.legend=1.5,element.color='black',
                   auto.grid=FALSE,colorset=cset,cex.lab=1.5,cex.axis=1.5,cex.main=2)
  dev.off()  
  
  chart_name2 = "chart_multi.jpg"  
  jpeg(paste0(folder_name,chart_name2),width=width_c,height=height_c)
  
  charts.PerformanceSummary(xts_matrix,legend.loc='topleft',
                            wealth.index=T,main='Performance of various strategies',cex.legend=1.5,element.color='black',
                            auto.grid=FALSE,colorset=cset,cex.lab=1.5,cex.axis=1.5,cex.main=2)
  dev.off()    
  
  chart_name3 = "chart_boxplot.jpg"  
  jpeg(paste0(folder_name,chart_name3),width=width_c,height=height_c)
  plot.new()
  chart.Boxplot(xts_matrix,colorset=cset,cex.lab=2,cex.axis=2,cex.main=2)
  
  dev.off()
  
  chart_name4 = "chart_risk_return.jpg"  
  jpeg(paste0(folder_name,chart_name4),width=width_c,height=height_c)
  plot.new()
  chart.RiskReturnScatter(data_4_plot,colorset=cset,legend.loc='topleft',cex.legend=1.5,cex.lab=1.5,cex.axis=1.5,cex.main=2 )
  dev.off()
  
  chart_name5 = "chart_rolling_perf.jpg"  
  jpeg(paste0(folder_name,chart_name5),width=width_c,height=height_c)
  plot.new()
  charts.RollingPerformance(xts_matrix,legend.loc='topleft',cex.legend=1.5,element.color='black',
                            auto.grid=FALSE,colorset=cset,cex.lab=1.5,cex.axis=1.5,cex.main=2)
  dev.off()
  
  chart_name6 = "charts_rolling_reg.jpg"  
  jpeg(paste0(folder_name,chart_name6),width=width_c,height=height_c)
  plot.new()
  charts.RollingRegression(xts_matrix,bm,legend.loc='topleft',cex.legend=1.3,element.color='black',
                           auto.grid=FALSE,colorset=cset,na.pad=FALSE,cex.lab=1.5,cex.axis=1.5,cex.main=2)
  dev.off()
  
  chart_name7 = "chart_spread_to_bench.jpg" 
  jpeg(paste0(folder_name,chart_name7),width=width_c,height=height_c)
  plot.new()
  chart.RelativePerformance(xts_matrix,bm,legend.loc='topright',cex.legend=1.5,element.color='black',
                            auto.grid=FALSE,colorset=cset,ylim=c(0.8,1.5),cex.lab=1.5,cex.axis=1.5,cex.main=2)
  dev.off()
  
  chart_name8 = "charts_rolling_correlation.jpg" 
  jpeg(paste0(folder_name,chart_name8),width=width_c,height=height_c)
  plot.new()
  chart.RollingCorrelation(xts_matrix,bm,legend.loc='bottomright',cex.legend=1.5,element.color='black',
                           auto.grid=FALSE,colorset=cset,cex.lab=1.5,cex.axis=1.5,cex.main=2)
  dev.off()
  
  # da_plots = list(chart_cumret,chart_multi,chart_da_box,chart_risk_return,chart_rolling_perf,charts_rolling_reg,
  #                 chart_spread_to_bench,charts_rolling_correlation)
  
  
  # names(da_plots) = c('chart_cumret',' chart_multi','chart_da_box','chart_risk_return','chart_rolling_perf',
  #                     'charts_rolling_reg','chart_spread_to_bench','charts_rolling_correlation')
  
}

make_the_stats = function(Ra,Rb){
  
  return_table = PerformanceAnalytics:::table.Returns(Ra)
  return_table_bench = PerformanceAnalytics:::table.Returns(Rb)
  CAPM_stats = table.CAPM(Ra,Rb)
  
  volstats = table.Variability(merge.xts(Ra,Rb))
  
  sharpe_table = SharpeRatio.annualized(merge.xts(Ra,Rb))
  
  es_info = ES(merge.xts(Ra,Rb))
  
  maxdd = maxDrawdown(merge.xts(Ra,Rb))
  
  detailed_stats_table = rbind(c(round(CAPM_stats[6,],3),0),
                               c(round(CAPM_stats[2,],2),1),
                               c(round(CAPM_stats[11,],2),0),
                               c(round(sharpe_table[1],2),round(sharpe_table[2],2)),
                               c(round(es_info[1],3),round(es_info[2],3)),
                               c(round(maxdd[1]*-1,3),round(maxdd[2]*-1,3)),
                               c(round(volstats[3,1],3),round(volstats[3,2],3)),
                               c(TrackingError(Ra,Rb),0))
  rownames(detailed_stats_table) = c('alpha',
                                     'beta',
                                     'Informationratio',
                                     'sharperatio',
                                     'es',
                                     'maxdd',
                                     'vol',
                                     'TrackingError')
  
  colnames(detailed_stats_table) = c('CKC','Benchmark')
  
  Year_stats = data.frame(return_table[,13],return_table_bench[,13])
  rownames(Year_stats) = rownames(return_table)
  
  ans = list(detailed_stats_table,Year_stats)
  return(ans)
}

ret_2_nav = function(x){
  init_nav = rep(100,ncol(x))
  ans_mat = xts(matrix(nrow=nrow(x),ncol=ncol(x)),order.by=index(x))
  
  ans_mat[1,] = as.numeric(init_nav*(1+x[1,]))
  for(i in 2:nrow(ans_mat)){
    ans_mat[i,] = as.numeric(ans_mat[i-1,])*as.numeric(1+x[i,])
    
  }
  colnames(ans_mat) = colnames(x)
  return(ans_mat)
  
}

recent_ret_table = function(x){
  if(periodicity(x)$scale!='monthly'){stop("Need a monthly time series")}
  cnames = c('1 month','3 months','6 months','1 year','3 years','5 years')
  if(nrow(x)>=60){
    m_1 = as.numeric(last(x))
    m_3 = as.numeric(Return.cumulative(xts::last(x,3)))
    m_6 = as.numeric(Return.cumulative(xts::last(x,6)))
    y_1 = as.numeric(Return.annualized(xts::last(x,12)))
    y_3 = as.numeric(Return.annualized(xts::last(x,36)))
    y_5 = as.numeric(Return.annualized(xts::last(x,60)))
    inception = as.numeric(Return.annualized(x))
    recent_rets = data.frame(m_1,m_3,m_6,y_1,y_3,y_5,inception)
    
    colnames(recent_rets) = c('1 month','3 months','6 months','1 year','3 years','5 years','From Inception')
    
    
    
  }else{
    recent_rets = data.frame(as.numeric(last(x)))
    colnames(recent_rets) = cnames[1]
    test_vec=c(3,6,12,36,60)
    for(i in 1:5){
      if(nrow(x)>=test_vec[i]){
        if(test_vec[i]<12){
          s1=  data.frame(as.numeric(Return.cumulative(xts::last(x,test_vec[i]))))
          #         colnames(s1) = cnames[i+1]
          
        }else{
          s1= data.frame(as.numeric(Return.annualized(xts::last(x,test_vec[i]))))
          #        colnames(s1) = cnames[i+1]
          #         
          #         recent_rets = data.frame(recent_rets,s1)
          # colnames(recent_rets) = cnames[1:(i+1)]
        }
        recent_rets = data.frame(recent_rets,s1)
        colnames(recent_rets)=cnames[1:(i+1)]
        
        
        
      }
      
    }
    inception = as.numeric(Return.annualized(x))
    
    recent_rets = data.frame(recent_rets,inception)
    colnames(recent_rets) = c(cnames[1:(i+1)],'From Inception')
    
  }
  
  
  
  return(recent_rets)
}

