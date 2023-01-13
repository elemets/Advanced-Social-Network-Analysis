library('statnet')
library('igraph')
library('RColorBrewer')

WTN <- read.csv('./Datasets/WTN.csv',sep=',',header = TRUE, stringsAsFactors = FALSE)
wtnNetwork <- as.network(WTN[,2:65], directed=TRUE)

WTNmatrix <- as.matrix(WTN[,2:65])

colnames(WTNmatrix) <- WTN[,1]
rownames(WTNmatrix) <- WTN[,1]
wtnGraph = graph.adjacency(WTNmatrix,mode='directed')

## Reading continents in from file 
conts <- read.csv('./Datasets/Conts_Nums.csv',sep=',',header = TRUE)
continent <- conts[2:65]

vertex_attr(wtnGraph) <- list(continent = conts[2:65])
vertex_attr(wtnGraph, "label") <- colnames(WTNmatrix)

## plotting graph with colours and adding the legend

cont_list <- c("Africa", "South America", "Oceania", "Europe", "Asia", "North America")

pal <- brewer.pal(length(cont_list), "Set1")
colors <- pal[as.numeric(as.factor(vertex_attr(wtnGraph, "continent")))]

V(wtnGraph)$continent

plot(wtnGraph, vertex.size=10, edge.width=.5 ,edge.arrow.size =.3, vertex.label.family='sans',vertex.label.cex=.75, vertex.color= pal[as.numeric(as.factor(vertex_attr(wtnGraph, "continent")))])

legend(1, 1, legend=cont_list, col=unique(pal[as.numeric(as.factor(vertex_attr(wtnGraph, "continent")))]), pch=15,cex=0.75)



## hetrophilous and homophilous ties
as_ties_homo =sum(WTNmatrix[continent==1, continent==1])
as_ties_hetero =sum(WTNmatrix[continent==1, continent!=1])
eu_ties_homo =sum(WTNmatrix[continent==2, continent==2])
eu_ties_hetero =sum(WTNmatrix[continent==2, continent!=2])
oc_ties_homo =sum(WTNmatrix[continent==3, continent==3])
oc_ties_hetero =sum(WTNmatrix[continent==3, continent!=3])
sa_ties_homo =sum(WTNmatrix[continent==4, continent==4])
sa_ties_hetero =sum(WTNmatrix[continent==4, continent!=4])
na_ties_homo =sum(WTNmatrix[continent==5, continent==5])
na_ties_hetero =sum(WTNmatrix[continent==5, continent!=5])
af_ties_homo =sum(WTNmatrix[continent==6, continent==6])
af_ties_hetero =sum(WTNmatrix[continent==6, continent!=6])

total = sum(as_ties_homo, as_ties_hetero,na_ties_homo,na_ties_hetero,
sa_ties_homo,sa_ties_hetero,af_ties_hetero,af_ties_homo,oc_ties_homo, oc_ties_hetero,eu_ties_homo,eu_ties_hetero)

WTNmatrix[continent==6, continent==1]
continent <- V(wtnGraph)$continent 
summary(wtnNetwork~edges+nodecov('continent'))

set.vertex.attribute(wtnNetwork, "continent", continent)
wtnNetwork%v%"continent" <- continent

sum(outdegree_input)

## general node match using idegree
idegree_list <- c(0,1)
ergm_edge_dist_homo <- ergm(wtnNetwork~edges+nodeifactor('continent',levels=c(2,3,4,5), nodematch('continent'), odegree(5), idegree(idegree_list)))
summary(ergm_edge_dist)
ergm_edge_dist_gof_homo.gof <- gof(ergm_edge_dist_homo~distance+idegree+esp+dsp+odegree)
par(mfrow=c(3,2))
plot(ergm_edge_dist_gof_homo.gof)

## node match for Europe
idegree_list <- c(0,1)
ergm_edge_dist_eu <- ergm(wtnNetwork~edges+nodeifactor('continent',levels=c(2), nodematch('continent', levels=c(2)), odegree(5), idegree(idegree_list)))
summary(ergm_edge_dist)
ergm_edge_dist_gof_eu.gof <- gof(ergm_edge_dist_eu~distance+idegree+esp+dsp+odegree)
par(mfrow=c(3,2))
plot(ergm_edge_dist_gof_eu.gof)

## transitivity

triad_census(wtnGraph)
 ergm(wtnNetwork~edges+triadcensus(levels=c(8,12))+gwesp(0.35))
summary(ergm_edge_dist_trans)
ergm_edge_dist_trans.sim <- simulate(ergm_edge_dist_trans, nsim=20)
ergm_edge_dist_gof_trans.gof <- gof(ergm_edge_dist_trans~distance+esp+odegree+idegree)
plot(ergm_edge_dist_gof_trans.gof)

## balance
triad_census(wtnGraph)
ergm_edge_dist_bal <- ergm(wtnNetwork~edges+balance+odegree(5)+idegree(1)+idegree(0)+)
summary(ergm_edge_dist_bal)
ergm_edge_dist_bal.sim <- simulate(ergm_edge_dist_bal, nsim=20)
ergm_edge_dist_gof_bal.gof <- gof(ergm_edge_dist_bal~distance+esp+triadcensus+idegree)
plot(ergm_edge_dist_gof_bal.gof)
