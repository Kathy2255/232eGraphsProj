library("igraph")

#Part 1 
file = scan("/Users/shivinkapur/Documents/Shivin College/UCLA/UCLA Classes/Spring 14/EE 232e/HW3/sorted_directed_net.txt", what=list(0,0,0))
fromlink <- file[[1]] + 1
tolink <- file[[2]] + 1
edgelist = cbind(fromlink, tolink)

g <- graph.edgelist(el=edgelist, directed=TRUE); 
E(g)$weight <- file[[3]];

is.connected(g)
is.directed(g)
is.weighted(g)

gcc_strong <- clusters(g, mode="strong")
gccIndex_strong = which.max(gcc_strong$csize)
gcc_node_strong <- c()
for(i in 1:length(V(g))){
  if(gcc_strong$membership[i] == gccIndex_strong)
    gcc_node_strong <- append(gcc_node_strong, i)
}

#Part 2
out_dd_strong <- degree(g,v=gcc_node_strong, mode="out")
in_dd_strong <- degree(g,v=gcc_node_strong,mode="in")

plot(gcc_node_strong,out_dd_strong,pch=20,type="line",xlab="Nodes",ylab="Out Degree",main="Strongly Connected Component")
plot(gcc_node_strong,in_dd_strong,pch=20,type="line",xlab="Nodes",ylab="In Degree",main="Strongly Connected Component")

#Part 3
g_undirected_1 <- as.undirected(g,mode="each")

community_1 <- label.propagation.community(g_undirected_1)
#community_1$membership
table(community_1$membership)
names(community_1)
community_1$modularity
community_1$vcount
community_1$algorithm
#plot(community_1,g_undirected_1)

g_undirected_2 <- as.undirected(g,mode="collapse", edge.attr.comb=list(weight="prod"))
E(g_undirected_2)$weight <- sqrt(E(g_undirected_2)$weight)

community_2 <- fastgreedy.community(g_undirected_2)
#community_2$membership
table(community_2$membership)
community_2$modularity
mean(community_2$modularity)
community_2$vcount
community_2$algorithm 
#plot(community_2,g_undirected_2)

#Part 4 
table1 <- table(community_2$membership)
largest_community_2 = which.max(table1)[[1]]
vertices <- c()
for(i in V(g_undirected_2))
  if(community_2$membership[i] == largest_community_2)
    vertices <- append(vertices, i)

#vertices

sub_graph <- induced.subgraph(g_undirected_2, vids=vertices)

community_2_1 <- fastgreedy.community(sub_graph)
community_2_1$membership
table(community_2_1$membership)
community_2_1$modularity
mean(community_2_1$modularity)
community_2_1$vcount
community_2_1$algorithm 

#Part 5
index_greater_than_100 <- which(table(community_2$membership) > 100, arr.ind=TRUE)
sub_graph <- c()
community <- c()
indexes <- paste("A",1:length(index_greater_than_100),sep="")

for(k in index_greater_than_100)
{
  vertices <- c()
  for(i in V(g_undirected_2))
    if(community_2$membership[i] == k)
      vertices <- append(vertices, i)
    
  sub_graph[[indexes[[k]]]] <- induced.subgraph(g_undirected_2, vids=vertices)
  
  community[[indexes[[k]]]] <- fastgreedy.community(sub_graph[[indexes[[k]]]])
}

for(k in index_greater_than_100)
{
  hist(community[[indexes[[k]]]]$membership, xlab="Community Membership Number", ylab="Frequency", main=paste("Histogram of Community Structure",k))
}
community
length(community)
  

#Part 6

library("netrw")
#install.packages("hash")
library("hash")

rw <- netrw(graph=g, walker.num=length(V(g)), start.node=sample(1:(vcount(g))), damping=0.85, T=length(V(g)), output.walk.path=TRUE, output.visit.prob=TRUE) 

master_hash <- hash(V(g), rep(0,length(V(g))))
node_multi_comm <- c()

for(i in 1:length(V(g)))
{
  h <- hash(1:15, rep(0,15))
  
  c1 <- 1:length(V(g))
  c2 <- rw$visit.prob[,i]
  mat <- cbind(c1,c2)
  mat <- mat[order(mat[,2], decreasing=TRUE), ]
  for(j in 1:30)
  {
    m <- community_2$membership[mat[j,][1]]
    v <- mat[j,][2]
    curr_value <- values(h, keys=m)
    curr_value <- curr_value + (m*v) 
    .set(h, keys=m, values=curr_value)
  }
  for (j in 1:15) 
  {
    val <- values(h, keys=j)
    if (val < 0.01)
    {
      .set(h, keys=j, values=0)
    }
  }
  .set(master_hash, keys=i,values=h)
  if(length(which(values(values(master_hash, keys = i)[[1]]) > 0)) > 1)
    node_multi_comm <- append(node_multi_comm,i)
}

#display 3 nodes belonging to different communities
length(node_multi_comm)
node_values = sample(node_multi_comm,3)
node_values

for (i in node_values) {
  print (values(master_hash, keys=i))
}

