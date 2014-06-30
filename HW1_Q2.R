library("igraph")
g <- barabasi.game(1000,directed=FALSE)
#####PLOTTING DEGREE DISTRIBUTION
hist(degree(g),breaks=50, main="Degree Distribution")
plot(degree.distribution(g),main="Degree Distribution")

#### CHECKING CONNECTIVITY
is.connected(g)
dia=diameter(g, directed = FALSE, unconnected = FALSE, weights = NULL)
path=get.diameter(g, directed = FALSE, unconnected = FALSE, weights = NULL)

##### FINDING GIANT CONNECTED COMPONENT
cl <- clusters(g)
gccIndex = which.max(cl$csize)
nonGccNodes <- (1:vcount(g))[cl$membership != gccIndex]
gcc <- delete.vertices(g, nonGccNodes)
fg <- fastgreedy.community(gcc)

##### PLOTTING THE GCC
plot(gcc)
plot(g)

#fastgreedy.community(gcc)
cmsize <- sizes(fg)
hist(fg$membership,breaks=50)
hist(fg$modularity,breaks=50)
ctm = community.to.membership(g, fg$merges, step=which.max(fg$modularity)-1)
barplot(table(ctm$csize),main="community size")
hist(fg$modularity,breaks=50)
cmsize <- as.vector(sizes(fg))
gccNodes <- (1:vcount(g))[cl$membership == gccIndex]
cm1Nodes <- gccNodes[fg$membership == 1]


###### DOING FOR 10000 NODES
g1 <- barabasi.game(10000,directed=FALSE)
cl1 <- clusters(g1)
gccIndex1 = which.max(cl1$csize)
nonGccNodes1 <- (1:vcount(g1))[cl1$membership != gccIndex1]
gcc1 <- delete.vertices(g1, nonGccNodes1)
fg1 <- fastgreedy.community(gcc1)
hist(fg1$modularity,breaks=50)
plot(fg1$modularity)


#### PART (D)

i = floor(runif(1)*1000)
nbr_size <- neighborhood.size(g, 1, i)
nbr <- data.frame(neighborhood(g, 1, i))
j = ceiling(runif(1)*nbr_size)
if(j == 1) {
  j == 2
}

node_num = nbr[j,]
g_nbr <- graph.neighborhood(g, 1, node_num) 
degreeDist <- degree(g_nbr[[1]])
h2 <- hist(degreeDist, breaks=seq(-0.5, by=1 , length.out=max(degreeDist)+2))       
pl2 <- data.frame(x=h2$mids, y=h2$density)
plot(pl2, type="o")
