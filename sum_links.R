sum_links = function(x){
  df = data.frame()
  for (i in 1:nrow(x)){
    tmp = 0; y=0
    tmp = x[x$Party != x$Party[i],]
    y = colSums(as.matrix(tmp[,i]))
    df = rbind(df, y)
  }
  rownames(df) = rownames(x)
  return(df)
}