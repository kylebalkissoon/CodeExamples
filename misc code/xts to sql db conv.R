xts_to_df = function(x){
  ##Extract Date
  date_index = as.character(index(x))
  
  ##Extract Data
  main_data = coredata(x)
  
  ##Join
  output_data_frame = data.frame(date_index,main_data)
  
  ##Map Column Names
  #replace space with _ in colnames
  fixed_col_names = sub(".","_",colnames(x),fixed=TRUE)
  fixed_col_names = sub(".","_",fixed_col_names,fixed=TRUE)
  fixed_col_names = sub(".","_",fixed_col_names,fixed=TRUE)
  fixed_col_names = sub(".","_",fixed_col_names,fixed=TRUE)
  fixed_col_names = sub(".","_",fixed_col_names,fixed=TRUE)
  
  colnames(output_data_frame) = c('date',fixed_col_names)
  
  return(output_data_frame)  
}



df_to_xts = function(x){
  #Get Date
  dates = x$date
  
  #Get Data
  main_data = x[,2:ncol(x)]
  
  ##Make all numeric
  main_data = data.matrix(main_data)
  
  #Make XTS
  xts_obj = xts(main_data,order.by=as.Date(dates))
  
  #Assign Column Names
  colnames(xts_obj) = colnames(x)[2:ncol(x)]
  return(xts_obj)
}
