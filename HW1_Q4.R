library("igraph")
g <- forest.fire.game(1000, directed=TRUE, fw.prob=0.37, bw.factor=0.32/0.37)
ddin <- degree.distribution(g, mode="in")
#PLOTTING THE DEGREE DISTRIBUTION
ddout<- degree.distribution(g, mode="out")
if (interactive()) {
  plot(seq(along=ddin)-1, ddin, log="xy")
  points(seq(along=ddout)-1, ddout, col=2, pch=2)
}

#CHECKING THE CONNECTIVITY
if(is.connected(g))
{
  print("Graph g with FOREST FIRE MODEL is connected.")
} else 
  print("Graph g with FOREST FIRE MODEL is NOT connected.")

#CALCULATING THE DIAMETER
dia=diameter(g, directed = TRUE, weights = NULL)
path1=get.diameter(g, directed = TRUE, weights = NULL)
prop<-edge.betweenness.community (g,directed = TRUE, edge.betweenness = TRUE, merges = TRUE,
                            bridges = TRUE, modularity = TRUE, membership = TRUE)

#PLOTTING THE MODULARITY
plot(prop$membership,main="Membership Plot")
plot(prop$modularity,main="Modularity Plot")
ctm = community.to.membership(g, prop$merges, step=which.max(prop$modularity)-1)
barplot(table(ctm$csize),main="Community Size Distribution")
