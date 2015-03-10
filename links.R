DSenate = filter(Senate, Party == 'D')
RSenate = filter(Senate, Party == 'R')
rownames(RSenate) = RSenate$Senator.Rep
rownames(DSenate) = DSenate$Senator.Rep
DSenate = DSenate[,-(1:2)]
RSenate = RSenate[,-(1:2)]
DemsWithin = rowSums(as.matrix(DSenate) %*%t(as.matrix(DSenate)))
RepsWithin = rowSums(as.matrix(RSenate) %*%t(as.matrix(RSenate)))
Coopertivity = rowSums(as.matrix(DSenate) %*%t(as.matrix(RSenate)))
Coopertivity2 = rowSums(as.matrix(RSenate) %*%t(as.matrix(DSenate)))
WithinParty = c(RepsWithin, DemsWithin)
AcrossAisle = c(Coopertivity, Coopertivity2)
merge(WithinParty,AcrossAisle,by=0)