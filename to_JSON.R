JSON = vector('list', nrow(CongressAdj))
names(JSON) = rownames(CongressAdj)
for(i in 1:ncol(CongressAdj)){
  JSON[[i]] = sapply(CongressAdj[,i], function(x) ifelse(x > 0, x, NA))
}
JSON = lapply(JSON, function(x) x[!is.na(x)])
dataframe = as.data.frame(source = rep(names(JSON[1]), length(JSON[[1]])), targets = names(JSON[[1]]), weights = JSON[[1]])
head(dataframe)
