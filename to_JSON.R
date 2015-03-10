#encode list of connections
JSON = vector('list', nrow(CongressAdj))
names(JSON) = rownames(CongressAdj)
for(i in 1:ncol(CongressAdj)){
  JSON[[i]] = sapply(CongressAdj[,i], function(x) ifelse(x > 0, x, NA))
}
JSON = lapply(JSON, function(x) x[!is.na(x)])

#encode JSON links for d3 force layout
JSON2 = vector('list', length(JSON))
for(i in 1:length(JSON2)){
  JSON2[[i]] = cbind(source = i, value = JSON[[i]], target = sapply(names(JSON[[i]]), grep, names(JSON)))
  rownames(JSON2[[i]]) = NULL
}
JSONlinks = do.call(rbind, JSON2)

#encode JSON nodes for d3 force layout
JSONnodes = cbind(HouseCent, names = rownames(HouseCent), group = Party)
rownames(JSONnodes) = NULL
