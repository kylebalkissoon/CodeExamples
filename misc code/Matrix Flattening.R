upper.diag<-function(x){
  #Maps horizontal vector to upper.tri of matrix filling by column
  m<-(-1+sqrt(1+8*length(x)))/2
  X<-upper.tri(matrix(NA,m,m),diag=TRUE)
  X[X==TRUE]<-x
  X
}
lower.diag<-function(x){
  #Maps horizontal vector to upper.tri of matrix filling by column
  m<-(-1+sqrt(1+8*length(x)))/2
  X<-upper.tri(matrix(NA,m,m),diag=TRUE)
  X[X==TRUE]<-x
  t(X)
}

recreate_matrix = function(x,ret_mat){
  # Recreates covariance matrix from time series vectorized form
  # Reconstruction upper and lower tri's by column (to reverse deconstruction in same manner)
  # Reverses vectorization of covariance matrix
  # x = Vectorized Covariance Matrices in Time Series Format (Upper Tri + Diagonal must be Vectorized by Column)
  step1 = upper.diag(x) + lower.diag(x)
  diag(step1) = diag(step1)/2
  colnames(step1) = colnames(ret_mat)
  rownames(step1) = colnames(ret_mat)
  return(step1)
}
