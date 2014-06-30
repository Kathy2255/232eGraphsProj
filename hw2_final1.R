library(igraph)
library(netrw)
myfunction <- function(graph,nodes,damping_factor)
{
  avg<-rep(NA,nodes)
  stdev<-rep(NA,nodes)
  for(i in (1:nodes))
  {
    r1<- netrw(graph, walker.num=nodes,start.node=1:vcount(graph), damping=damping_factor, T=i, output.walk.path=TRUE)
    sp <- rep(NA,nodes)
    for(j in (1:nodes))
    {
      a <-get.shortest.paths(graph, from=r1$walk.path[1,j], to=r1$walk.path[i,j])
      sp[j]<-length(a$vpath[[1]])-1
    }
    avg[i] = mean(sp)
    stdev[i] = sd(sp) 
  }
  print(avg)
  print(stdev)
  
  #plots
  plot(1:nodes,avg,type="line",xlab="Number of Steps",ylab="Average")
  plot(1:nodes,stdev,type="line",xlab="Number of Steps",ylab="Standard Deviation")
  
  #degreedistribution
  #part e
  hist(degree(graph),xlab="Degree",ylab="Frequency",main="Degree Distribution of the graph")
  degdist <- rep(NA,nodes)
  for(i in (1:nodes))
    degdist[i] = degree(graph,r1$walk.path[nodes,i])
  hist(degdist,xlab="Degree",ylab="Frequency",main="Degree distribution of the nodes reached at the end of the random walk")
  
}
#part 1.a)
g1_1000 <- random.graph.game(1000, 0.01, directed=FALSE)

#part 1.b)
myfunction(g1_1000,1000,1) #1000 nodes

#part 1.d)
g1_100 <- random.graph.game(100, 0.01, directed=FALSE)
myfunction(g1_100,100,1) #100 nodes

g1_10000<- random.graph.game(10000, 0.01, directed=FALSE)
myfunction(g1_10000,10000,1) #10000 nodes


#print Diameters for q1
print(diameter(g1_100))
print(diameter(g1_1000))

#part 1.e) is being calculated in the function

#part 2.a)
g2_1000 = barabasi.game(1000,directed=FALSE)

#part 2.b)
myfunction(g2_1000,1000,1)

#part 2.d)
g2_100 = barabasi.game(100,directed=FALSE)
myfunction(g2_100,100,1)

#print Diameters for q2
print(diameter(g2_1000))
print(diameter(g2_100))

#part 2.e) is being calculated in the function

#part 3.a)
myfunction2 <- function(graph,nodes,damping_factor)
{
  r1<- netrw(graph, walker.num=nodes,start.node=1:vcount(graph), damping=damping_factor, T=1000, output.walk.path=TRUE)
  plot(r1$ave.visit.prob,xlab="Nodes",ylab="Visit Probablity",pch=1)
  
  degprob<- c()
  visitprob <- c()
  for(i in r1$walk.path[,1])
  {
    degprob<-append(degprob,degree(graph,i))
    visitprob<-append(visitprob,r1$ave.visit.prob[i])
  }
  #print(length(degprob))
  #print(length(visitprob))
  
  final_measure<- cbind(degprob,visitprob)
  final_measure<- final_measure[order(final_measure[,1]),]
  #print(final_measure)
  
  plot(final_measure[,1],final_measure[,2],type="line",xlab="Degree",ylab="Average Visit Probablity")
}

g3_1000 <- random.graph.game(1000, 0.01, directed=FALSE);
myfunction2(g3_1000,1000,1)

#part 3.b)
g3_d1000 <- random.graph.game(1000, 0.01, directed=TRUE);
myfunction2(g3_d1000,1000,1)

#part 3.c)
myfunction2(g3_1000,1000,0.85)


myfunction4 <- function(graph,nodes,damping_factor,vector)
{
  r1<-netrw(graph, walker.num=nodes,start.node=1:vcount(graph),damping=damping_factor,T=1000, output.walk.path=TRUE,teleport.prob=vector)
  #plot(r1$ave.visit.prob,xlab="Nodes",ylab="Visit Probablity",pch=1)
  
  degprob<- c()
  visitprob <- c()
  for(i in r1$walk.path[,1])
  {
    degprob<-append(degprob,degree(graph,i))
    visitprob<-append(visitprob,r1$ave.visit.prob[i])
  }
  final_measure<- cbind(degprob,visitprob)
  
  #plot(final_measure[,1],final_measure[,2],type="line",xlab="Degree",ylab="Average Visit Probablity")
  plot(1:1000,final_measure[,2],xlab="Number of nodes",ylab="Average Visit Probablity")
}


#part 4.a)
g4_1000 <- random.graph.game(1000, 0.01, directed=TRUE)
myfunction4(g4_1000,1000,0.85,NULL)
pagerank1<-page.rank(g4_1000,directed=TRUE,damping=0.85)


#part 4.b)
g4_1000tele <- random.graph.game(1000, 0.01, directed=TRUE)
myfunction4(g4_1000tele,1000,0.85,pagerank1$vector)

myfunctionCombine4<- function(graph1,graph2,nodes,damping_factor)
{
  r1<-netrw(graph1, walker.num=nodes,start.node=1:vcount(graph1),damping=damping_factor,T=1000, output.walk.path=TRUE)
  pagerank1<-page.rank(graph1,directed=TRUE,damping=0.85)
  r2<-netrw(graph2, walker.num=nodes,start.node=1:vcount(graph2),damping=damping_factor,T=1000, output.walk.path=TRUE,teleport.prob=pagerank1$vector)
  #plot(r1$ave.visit.prob,xlab="Nodes",ylab="Visit Probablity",pch=1)
  
  degprob<- c()
  visitprob <- c()
  final_measure<-c()
  for(i in r1$walk.path[,1])
  {
    degprob<-append(degprob,degree(graph1,i))
    visitprob<-append(visitprob,r1$ave.visit.prob[i])
  }
  final_measure<- cbind(degprob,visitprob)
  
  #plot(final_measure[,1],final_measure[,2],type="line",xlab="Degree",ylab="Average Visit Probablity")
  plot(1:1000,final_measure[,2],xlab="Number of nodes",ylab="Average Visit Probablity")

  degprob1<- c()
  visitprob1 <- c()
  final_measure1<-c()
  for(i in r2$walk.path[,1])
  {
    degprob1<-append(degprob1,degree(graph2,i))
    visitprob1<-append(visitprob1,r2$ave.visit.prob[i])
  }
  final_measure1<- cbind(degprob1,visitprob1)
  
  points(1:1000,final_measure1[,2],col="red")
}

myfunctionCombine4(g4_1000,g4_1000tele,1000,0.85)
