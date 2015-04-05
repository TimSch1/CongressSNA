Congress = read.csv('113thCongress.csv', header = TRUE)
Congress = Congress[,-c(13,14,18,20,23,24,26,33,51,60,62,77)] #remove excluded bills
Congress[is.na(Congress)] = 0 #replace NAs with 0s

library(stringr)
Sens = str_detect(Congress$Senator.Rep, 'Sen') #Separate Senate from House of Reps
Senate = Congress[Sens,]
Congress = Congress[-Sens,]
Senate$Senator.Rep = gsub('Sen', '', Senate$Senator.Rep) #Strip title
Senate$Senator.Rep = str_trim(Senate$Senator.Rep, side='both') #Remove Whitespace
which(str_detect(Senate$Senator.Rep, 'Rep')) #One representative had 'Sen' in last name, erroneously included
Senate = Senate[-67,]
#Senate2 = Senate[-(85:86),] #Removes Oren Hatch and Mike Lee for 2nd Pass


DSenate = filter(Senate, Party == 'D')
RSenate = filter(Senate, Party == 'R')
rownames(RSenate) = RSenate$Senator.Rep
rownames(DSenate) = DSenate$Senator.Rep
DSenate = DSenate[,-(1:2)] #Creates Adj Matrix for within/across links
RSenate = RSenate[,-(1:2)]
DemsWithin = rowSums(as.matrix(DSenate) %*%t(as.matrix(DSenate)))
RepsWithin = rowSums(as.matrix(RSenate) %*%t(as.matrix(RSenate)))
Coopertivity = rowSums(as.matrix(DSenate) %*%t(as.matrix(RSenate)))
Coopertivity2 = rowSums(as.matrix(RSenate) %*%t(as.matrix(DSenate)))
WithinParty = c(RepsWithin, DemsWithin)
AcrossAisle = c(Coopertivity, Coopertivity2)
Links = merge(WithinParty,AcrossAisle,by=0)
colnames(Links) = c('Name','Within','Coopertivity')

rownames(Senate) = Senate$Senator.Rep #Simplify DF to matrix
Party = Senate$Party
Senate = Senate[,-(1:2)]

SenateAdj = as.matrix(Senate) %*% t(as.matrix(Senate)) #Create adjacency matrix

library(igraph)
g = graph.adjacency(SenateAdj, mode = 'undirected', weighted = TRUE) #Create plot object
g= simplify(g) #gets rid of loops within vertices - code to show weight in Edges!!
V(g)$color = ifelse(Party == 'D', 'blue', 'red')
V(g)$label = V(g)$name #get rid of title, resize smaller name
V(g)$label.cex = 0.5
SenateCent = data.frame(bet = betweenness(g), eig = evcent(g)$vector)
layout1 = layout.fruchterman.reingold(g, niter=500)
set.seed(200)
plot(g, layout1, vertex.label.color='black', vertex.size = SenateCent$bet, edge.width=E(g)$weight)

detach('package:sna', unload=T)
SenateCent = data.frame(bet = betweenness(g), eig = evcent(g)$vector) #regress Eigenvector on Betweenness
res = lm(eig~bet, data = SenateCent)$residuals
SenateCent = transform(SenateCent, res=lm(eig~bet, data= SenateCent)$residuals)
library(ggplot2)
SenateActors = ggplot(SenateCent, (aes(x=bet, y=eig, label = rownames(SenateCent),color=Party, size = abs(res))))+xlab('Betweenness Centrality')+ylab('Eigenvector Centrality')
SenateActors+scale_color_manual(values=c('blue','red'))+geom_text()+labs(title = 'Eigenvector and Betweenness Centrality') #Add title and make geom text

#following analysis not useful
layout1 = layout.fruchterman.reingold(g, niter=500)
nodes = as.vector(V(g))
nodes[which(abs(res)<.3)] = NA
nodes = ifelse(nodes =='NA', 'NA', rownames(SenateAdj))
plot(g, layout = layout1, vertex.label=nodes, vertex.size = abs(res)*50, vertex.label.dist = 0.25, edge.width = E(g)$weight, vertex.label.color = 'black')

cores = graph.coreness(g)
g2 = induced.subgraph(g, as.vector(which(cores>50)))
nodes = as.vector(V(g2))
nodes = V(g2)$name
#nodes[which(abs(res)<.3)] = NA
layout2 = layout.fruchterman.reingold(g2, niter=500)
plot(g2, layout = layout2, vertex.label=nodes, vertex.size = abs(res)*50, vertex.label.dist = 0.25, vertex.label.color = 'black')

library(sna)
library(ggdendro)
set.seed(200)
SenateClust = equiv.clust(SenateAdj, method='hamming', cluster.method='ward.D')
den = as.dendrogram(SenateClust$cluster)
ddata = dendro_data(den, type='rectangle')
ddata$labels[,3] = rownames(SenateAdj)
ddata$labels = cbind(ddata$labels, Party)
ggplot() +
       geom_segment(data=segment(ddata), aes(x=x, y=y, xend=xend, yend=yend)) +
       geom_text(data = label(ddata), aes(x=x, y=y, label=label, colour = Party, hjust=0), size=3) +
       geom_point(data = label(ddata), aes(x=x, y=y), size=3, shape = 21) +
       coord_flip() +
       scale_y_reverse(expand=c(0.2, 0)) +
       scale_colour_manual(values=c('blue','red')) + 
       theme_dendro()

hClustAssignments = cutree(SenateClust$cluster, k=4)

set.seed(200)
wc = walktrap.community(g) #seeCommunities
plot(wc, g)
walktrapComm = wc$membership

Coopertivity = data.frame(Name = rownames(SenateAdj), Party = Party, hClustAssign = hClustAssignments, wtComm = walktrapComm)
Coopertivity$hClustAssign = ifelse(Coopertivity$hClustAssign == 4, 'R', 'D')
Coopertivity$wtComm = ifelse(Coopertivity$wtComm == 3, 'D', 'R')
Coopertivity = merge(Coopertivity, Links, by.x='Name', by.y='Name')

