######################################################
# Name: Utkarsha Verma
# UID: 204357330
# EE232E Project
######################################################

library("igraph")
library("fit.models")
library("ggplot2")
library("hash")

edgelistFile <- "/Volumes/Utkarsha/Study_Material/UCLA/Quarter_3/EE232/Project/facebook_combined.txt"

g <- read.graph(edgelistFile , format = "ncol" , directed=FALSE)

g = graph.edgelist(el=edges, directed=FALSE); 

############   QUESTION_1     ################


is.connected(g)
is.directed(g)

diameter(g, directed = FALSE)
degree.distribution(g)
degreeDist = degree(g)
h1 = hist(degreeDist, breaks=seq(0.0, by=1 , length.out=max(degreeDist)+2))       
df1 = data.frame(x=h1$mids, y=h1$density)
plot(df1,main="Degree Distribution of Facebook Graph", xlab="Nodes", ylab="Degree Distribution",type="o")
barplot(degree.distribution(g),main="Bar Plot of Degree Distribution", xlab="Nodes", ylab="Frequency of Distribution")

models <- list(
               nls(y ~ I(1/x*a) + b*x, data = df1, start = list(a = 1, b = 1)), 
               nls(y ~ (a + b*log(x)), data=df1, start = setNames(coef(lm(y ~ log(x), data=df1)), c("a", "b"))),
               nls(y ~ I(exp(1)^(a + b * x)), data=df1, start = list(a=-3.59,b=-0.02)),
               nls(y ~ I(1/x*a)+b, data=df1, start = list(a=1,b=1)),
               lm(y~x, data = df1), 
               lm(y~I(1/x), data=df1),
               lm(y ~ log(x), data = df1))


ggplot(df1, aes(x, y)) + geom_point(size = 5) +
  stat_smooth(method = "nls", formula = as.formula(models[[1]]), data=df1, start = list(a=0,b=0), size = 1, se = FALSE, colour = "blue") + 
  stat_smooth(method = "nls", formula = as.formula(models[[2]]), data=df1, start = setNames(coef(lm(y ~ log(x), data=df1)), c("a", "b")), size = 1, se = FALSE, colour = "yellow") +
  stat_smooth(method = "nls", formula = as.formula(models[[3]]), data=df1, start = list(a=0,b=0), size = 1, se = FALSE, colour = "purple") +
  stat_smooth(method = "nls", formula = as.formula(models[[4]]), data=df1, start = list(a=0,b=0), size = 1, se = FALSE, colour = "red")+
  stat_smooth(method = "lm", formula = as.formula(models[[5]]), size = 1, se = FALSE, colour = "white") + 
  stat_smooth(method = "lm", formula = as.formula(models[[6]]), size = 1, se = FALSE, colour = "green") + 
  stat_smooth(method = "lm", formula = as.formula(models[[7]]), size = 1, se = FALSE, colour = "pink") +
  ggtitle("Fitted models for Degree Distribution of Facebook Graph")+ xlab("Nodes") +ylab("Degree Distribution")

ggplot(df1, aes(x, y)) + geom_point(size = 5) + stat_smooth(method = "nls", formula = as.formula(models[[3]]), data=df1, start = list(a=0,b=0), size = 1, se = FALSE, colour = "yellow")+ggtitle("Best Fitted model for Degree Distribution of Facebook Graph")+ xlab("Nodes") +ylab("Degree Distribution")


summary(nls(y ~ I(exp(1)^(a + b * x)), data=df1, start = list(a=-3.59,b=-0.02)))

mean(degree(g))


(RSS.p=sum(residuals(models[6])^2))
(TSS = sum((h1$density - mean(h1$density))^2))


1 - sum(((h1$mids)^3 - (h1$density))^2)/TSS
#1 - sum((x^3 - y)^2)/TSS
############  END OF QUESTION_1   #############


############   QUESTION_2     ################

subGraphNodes <- neighborhood(g , 1)
length(subGraphNodes)
subGraphNodes <- subGraphNodes[[1]]
length(subGraphNodes)
nonSubGraphNodes <- which( !( (1:vcount(g)) %in% subGraphNodes)  )
subGraph <- delete.vertices(g , nonSubGraphNodes)
no_of_edges = length(E(subGraph))
no_of_nodes = length(V(subGraph))


############  END OF QUESTION_2   #############


############   QUESTION_3     ###########

core_nodes = {}
core_nodes<-which(neighborhood.size(g, 1 , nodes=V(g)) > 200)
noOfcore_nodes = length(core_nodes)
avg_degree=mean(degreeDist[core_nodes])

core <- core_nodes[7]
subGraph_core_nodes <- neighborhood(g , 1 , nodes=core)
length(subGraph_core_nodes)
subGraph_core_nodes <- subGraph_core_nodes[[1]]
length(subGraph_core_nodes)
nonSubGraph_core_nodes <- which( !( (1:vcount(g)) %in% subGraph_core_nodes)  )
subGraph_core <- delete.vertices(g , nonSubGraph_core_nodes)
plot(subGraph_core,main="Sub Network of the core node 7")
comm1 = fastgreedy.community(subGraph_core)
plot(comm1,subGraph_core,main="Communities for Core Graph's Node 7, FAST-GREEDY",vertex.size=5,vertex.label=NA)
comm2 <- edge.betweenness.community(subGraph_core)
plot(comm2, subGraph_core,main="Communities for Core Graph's Node 7, EDGE-BETWEENNESS",,vertex.size=5,vertex.label=NA)
comm3 <- infomap.community(subGraph_core)
plot(comm3, subGraph_core,main="Communities for Core Graph's Node 7, INFOMAP",,vertex.size=5,vertex.label=NA)
hist(comm1$membership,col="dark red",main="Community Distribution of the Fast Greedy Algorithm",xlab="Community Number",ylab="No. of Nodes in a Community")
hist(comm2$membership,col="dark blue",main="Community Distribution of the Edge-Betweenness Algorithm",xlab="Community Number",ylab="No. of Nodes in a Community")
hist(comm3$membership,col="dark green",main="Community Distribution of the Infomap Algorithm",xlab="Community Number",ylab="No. of Nodes in a Community")


#plot(subGraph_core,vertex.color="black",edge.color="gray",vertex.size=4,vertex.label=NA,edge.arrow.size=0.01)


############  END OF QUESTION_3   #############


############   QUESTION_4     ###########

subGraph_core_nodes_1 <- subGraph_core_nodes[-1]
nonSubGraph_core_nodes_1 <- which( !( (1:vcount(g)) %in% subGraph_core_nodes_1)  )
subGraph_core_1 <- delete.vertices(g , nonSubGraph_core_nodes_1)
plot(subGraph_core_1)
comm1_1 = fastgreedy.community(subGraph_core_1)
plot(comm1_1,subGraph_core_1,main="Communities for Core Graph without core node 7, FAST-GREEDY")
comm2_1 <- edge.betweenness.community(subGraph_core_1)
plot(comm2_1, subGraph_core_1,main="Core Node 9's Personal Network Structure Without Node 9, EDGE-BETWEENNESS")
comm3_1 <- infomap.community(subGraph_core_1)
plot(comm3_1, subGraph_core_1,main="Communities for Core Graph without core node 7, INFOMAP")
hist(comm1_1$membership,col="dark red",main="Community Distribution of the Fast Greedy Algorithm",xlab="Community Number",ylab="No. of Nodes in a Community")
hist(comm2_1$membership,col="dark blue",main="Community Distribution of the Edge-Betweenness Algorithm",xlab="Community Number",ylab="No. of Nodes in a Community")
hist(comm3_1$membership,col="dark green",main="Community Distribution of the Infomap Algorithm",xlab="Community Number",ylab="No. of Nodes in a Community")
############  END OF QUESTION_4   #############


############   QUESTION_5     ###########

# defining initial functions
# ===========================
commNeib_find <- function(u,v,g)
  {
  neighborsU <- neighborhood(g,1,u)[[1]][-1]
  neighborsV <- neighborhood(g,1,v)[[1]][-1]
  intersect(neighborsU, neighborsV)
  }

# embeddedness calculations
# ===========================
embd_find <- function (u,v,g)
  {
  emdb = length(commNeib_find(u,v,g))
  emdb
  }

perNet_find <- function(u, g)
  {
  pnNodes <- neighborhood(g , 1 , nodes=u)[[1]]
  nonPNNodes <- which( !( (1:vcount(g)) %in% pnNodes)  )
  perNetw <- delete.vertices(g , nonPNNodes)
  perNetw$name =  sort(pnNodes)
  perNetw
  }

# node ids
# ========
nodeID_find <- function(g, vertex)
  {
  temp <- which(g$name == vertex)
  temp
  }

# dispersion
# ==========
disp_find <- function(u,v,g) 
  {
  disp <- 0
  commonUV <- commNeib_find(u, v, g)
  gNoUV <- delete.vertices(g, c(u, v))
  
  for(s in commonUV) 
    {
    for(t in commonUV) 
      {
      if(s != t) 
        {
        if(!is.na(match(s,V(gNoUV))) && !is.na(match(t,V(gNoUV))) && !are.connected(gNoUV,s,t) && length(commNeib_find(s,t,gNoUV)) == 0) {
          disp = disp + 1
          }
        }
      }
    }
  disp
}

# ratio: emb, disp
# =====
dispEmb_find <- function(g,coreNode){
  
  highDp = 0;
  dpNode = 0;
  Rt_high = 0;
  rtN = 0;
  embd_high = 0;
  embNode = 0;
  
  pnOfU <- perNet_find(coreNode,g)
  u <- nodeID_find(pnOfU, coreNode)
  print(u %in% V(pnOfU))
  
  nodes <- V(g)
  for(v in nodes){
    if(v == u)
      next
    
    dip = disp_find(u,v,g)
    emdb = embd_find(u,v,g)
    
    if (emdb > 0)
    {
      rt = dip/emdb
      if (rt > Rt_high)
      {
        rtN = v;
      }
    }
    
    if (dip > highDp)
    {
      dpNode = v
    }
    if (emdb > embd_high)
    {
      embNode = v
    }
    
  }
  
  if (dpNode > 0)
  {
    colVet = rep(1, length(V(pnOfU)));
    colVet[dpNode] = 2;
    colEd = rep(8, length(E(pnOfU)));
    colEd[which(get.edgelist(pnOfU,name=F)[,1] == dpNode | get.edgelist(pnOfU,name=F)[,2] == dpNode)] = 3;
    E(pnOfU)$color = colEd;
    dev.new ();
    plot(pnOfU, vertex.size=3, vertex.label=NA, asp=9/16, vertex.color=colVet, main=paste(c("Max dip", order, "Personal Network", "of node", toString(V(pnOfU)[dpNode]$Number)), collapse=" "));
  }
  
  else
  {
    print (paste(c("No high Disp node", toString(coreNode)), collapse=" "));
  }
  
  if (emNode > 0)
  {
    colVet = rep(1, length(V(pnOfU)));
    colVet[emNode] = 7;
    colEd = rep(8, length(E(pnOfU)));
    colEd[which(get.edgelist(pnOfU,name=F)[,1] == emNode | get.edgelist(pnOfU,name=F)[,2] == emNode)] = 3;
    E(pnOfU)$color = colEd;
    dev.new ();
    plot(pnOfU, vertex.size=3, vertex.label=NA, asp=9/16, vertex.color=colVet, main=paste(c("Max emdb", order, "Personal Network", "of node", toString(V(pnOfU)[emNode]$Number)), collapse=" "));
  }
  else
  {
    print (paste(c("No high Emb node", toString(coreNode)), collapse=" "));
  }
  
  if (rtN > 0)
  {
    colVet = rep(1, length(V(pnOfU)));
    colVet[rtN] = 6;
    colEd = rep(8, length(E(pnOfU)));
    colEd[which(get.edgelist(pnOfU,name=F)[,1] == rtN | get.edgelist(pnOfU,name=F)[,2] == rtN)] = 3;
    E(pnOfU)$color = colEd;
    dev.new ();
    plot(pnOfU, vertex.size=3, vertex.label=NA, asp=9/16, vertex.color=colVet, main=paste(c("Max Disp/Emb", order, "Personal Network", "of node", toString(V(pnOfU)[rtN]$Number)), collapse=" "));
  }
  else
  {
    print (paste(c("No high Disp node", toString(coreNode)), collapse=" "));
  }
  
}

# =======================
dispVec <- c();
emb_vector <- c();


for(coreNode in core_nodes)
{
  pnOfU <- perNet_find(coreNode,g)
  u <- nodeID_find(pnOfU, coreNode)
  print(u %in% V(pnOfU))
  
  nodes <- V(g)
  for(v in nodes){
    if(v == u)
      next
    
    emdb = embd_find(u,v,g)
    dip = disp_find(u,v,g)
    emb_vector <- c(emb_vector, emdb);
    dispVec <- c(dispVec, dip);
    
  }
}


dev.new()
hist (emb_vector, breaks=seq (-0.5, by=1, length.out=max(emb_vector) +2), main ="emdb Distribution", xlab="emdb");
dev.new ();
hist (dispVec, breaks=seq (-0.5, by=1, length.out=max(dispVec) +2), main="dip Distribution", xlab="dip");

dispEmb_find(fbGraph,coreNodes[1])
dispEmb_find(fbGraph,coreNodes[9])
dispEmb_find(fbGraph,coreNodes[10])


# subGraphNodes <- neighborhood(g , 1)
# length(subGraphNodes)
# subGraphNodes <- subGraphNodes[[1]]
# length(subGraphNodes)
# nonSubGraphNodes <- which( !( (1:vcount(g)) %in% subGraphNodes)  )
# subGraph <- delete.vertices(g , nonSubGraphNodes)
# no_of_edges = length(E(subGraph))
# no_of_nodes = length(V(subGraph))

# 
# find_Gu <- function(u, g){
#   subGraphNodes <- neighborhood(g , 1 , nodes=u)
#   subGraphNodes <- subGraphNodes[[1]]
#   nonSubGraphNodes <- which( !( (1:vcount(g)) %in% subGraphNodes)  )
#   subGraph <- delete.vertices(g , nonSubGraphNodes)
#   subGraph$name =  sort(subGraphNodes)
#   subGraph
# }
# 
# 
# for(nd in keys(h)){
#   disp_vals <- values(h,keys=nd)
#   nodes <- V(find_Gu(nd,g))
#   
#   idx <- which(disp_vals == max(disp_vals))
#   cat(nd," ", idx[1], "\n")
#   print(nodes[idx[1]])
#   print("----------------------")
# }
# 
# for(nd in keys(h_e)){
#   disp_vals <- values(h_e,keys=nd)
#   nodes <- V(find_Gu(nd,g))
#   
#   idx <- which(disp_vals == max(disp_vals))
#   cat(nd," ", idx[1], "\n")
#   print(nodes[idx[1]])
#   print("----------------------")
# }
# 
# for(nd in keys(h_u)){
#   disp_vals <- values(h_u,keys=nd)
#   nodes <- V(find_Gu(nd,g))
#   
#   idx <- which(disp_vals == max(disp_vals))
#   cat(nd," ", idx[1], "\n")
#   print(nodes[idx[1]])
#   print("----------------------")
# }
# 
# 
# nd <- 353
# g_temp <- find_Gu(nd,g)
# 
# neighborhood_core_infomap <- infomap.community(g_temp)
# neighborhood_core_fastgreedy <- fastgreedy.community(g_temp)
# #neighborhood_core_edgebetweenness <- edge.betweenness.community(g_temp)
# 
# V(g_temp)$color = colors()[membership(neighborhood_core_infomap)[V(g_temp)]]
# V(g_temp)[147]$color = "black"
# plot(g_temp, layout=layout.fruchterman.reingold, vertex.label=NA, vertex.size=6)
# 
# nd <- 349
# g_temp <- find_Gu(nd,g)
# 
# neighborhood_core_infomap <- infomap.community(g_temp)
# neighborhood_core_fastgreedy <- fastgreedy.community(g_temp)
# #neighborhood_core_edgebetweenness <- edge.betweenness.community(g_temp)
# 
# V(g_temp)$color = colors()[membership(neighborhood_core_infomap)[V(g_temp)]]
# V(g_temp)[10]$color = "red"
# plot(g_temp, layout=layout.fruchterman.reingold, vertex.label=NA, vertex.size=6)

###dispersion
#353,2154
#352, 260
#1822 41

###embeddedness
#349 10
#353 101
#108 1022


### dispersion/embeddedness
#1491 2
#1822 41
#349 10


############  END OF QUESTION_5   #############


############   QUESTION_6     ###########

h_main <- hash(keys=1,values=1)
for(node in core_nodes)
{
  sub_graph <- get_Gu(node, g)
  neighborhood_fastgreedy <- fastgreedy.community(sub_graph)
  membership <- neighborhood_fastgreedy$membership
  h_temp <- hash(keys=1,values=1)
  for(idx in 1:10)
  {
    members <- which(membership == idx)
    
    if(length(members) > 10)
    {
      members <- members[members != node]
      g_temp <- induced.subgraph(sub_graph, vids=members)
      Cuv <- V(g_temp)
      disp <- 0
      for(p in Cuv) 
        {
        for(q in Cuv) 
          {
          if(p != q) 
            {
            if(check_node(g_temp,p) && check_node(g_temp,q) && !are.connected(g_temp,p,q) && length(common_nbrs(p,q,g_temp)) == 0) 
              {
              disp = disp + 1   
              }
            }
          }
        }
      hash:::.set(h_temp,keys=idx,values=disp/2)
    }
  }
  print(h_temp)
  hash:::.set(h_main,keys=node,values=h_temp)
}

for(key in keys(h_main))
{
  cat("Node = ", key, "\n")
  
  for(k in keys(values(h_main,keys=key)[[1]]))
  {
    val <- values(values(h_main,keys=key)[[1]],keys=k)
    if(k == 1 && val == 1)
      next
    if(val == 0)
      type=0
    else
      type=1
    cat("membership = ", k , " Dispersion= " , val , " Community type = ",  type ,"\n")
  }
}

for(node in core_nodes)
{
  sub_graph <- get_Gu(node, g)
  neighborhood_fastgreedy <- fastgreedy.community(sub_graph)
  membership <- neighborhood_fastgreedy$membership
  print(table(membership))
}

############  END OF QUESTION_6   #############



############ Question 7##############


filesPath1 <- "/Volumes/Utkarsha/Study_Material/UCLA/Quarter_3/EE232/Project/gplus/"

edgesFiles = list.files(path="./gplus/",pattern="edges")
g2Raw<-{}
g2<-{}
g2u<-{}
egoNodes={}
circlesRaw={{}}
#circlesMap = hash(keys=1,values=1)
count=1
commu1 <- {{}}
commu2 <- {{}}
for(j in 1:132)
  {
  nodeId = sub("\\..*","",edgesFiles[j])
  circlesFile = paste(filesPath1,nodeId,".circles",sep="")
  fileConnection <- file(circlesFile, open="r")
  if(length(fileConnection)>0)
    {
    lines <- readLines(fileConnection)
    if(length(lines>0))
      {
      circles <- list()
      print(length(lines))
      for (i in 1:length(lines)) 
        {
        sp <- strsplit(lines[i],"\t")
        circles[[i]] <- sp[[1]][-1]
        }
      #close(fileConnection)
      if(length(circles)>2)
        {
        print("Found one!")
        edgelistFile = paste(filesPath1,edgesFiles[j],sep="")
        #g2Raw <- c(g2Raw,read.graph(edgelistFile, format = "ncol" , directed=TRUE))
        g2Raw[[count]] = read.graph(edgelistFile,format="ncol",directed=TRUE)
        circlesRaw[[count]] <-circles
        
        #hash:::.set(circlesMap,keys=nodeId,values=circles)
        nonEgoNodes = V(g2Raw[[count]])
        egoNodes[count]=nodeId
        g2[[count]] <- add.vertices(g2Raw[[count]],1,name=nodeId)
        egoNodeIndex <- which(V(g2[[count]])$name==nodeId) 
        edgeAppendList <- c()
        for (nodeIndex in 1:(vcount(g2[[count]])-1)) 
          {
          edgeAppendList <- c(edgeAppendList , c(vcount(g2[[count]]),nodeIndex))
          }
        g2[[count]] <- add.edges(g2[[count]],edgeAppendList)
        g2u[[count]]<- as.undirected(g2[[count]])
        commu1[[count]] <- walktrap.community(g2U, steps = 4, merges = TRUE, modularity = TRUE, membership = TRUE)
        commu2[[count]] <- infomap.community (g2U, e.weights = NULL, v.weights = NULL, nb.trials = 10, modularity = TRUE)
        
        count=count+1
        print(count)
      }
    } 
  }
  close(fileConnection)
}
##Sample Community Plot
gcomm_1<-walktrap.community(g2[[1]])
plot(gcomm_1,g2[[1]],vertex.label=NA,vertex.size=7,edge.arrow.size=0.2,main="Community Structure for ego node 1 - WALK TRAP")
gcomm_2<-infomap.community(g2[[1]])
plot(gcomm_2,g2[[1]],vertex.label=NA,vertex.size=7,edge.arrow.size=0.2,main="Community Structure for ego node 1 - INFOMAP")

plot(commu1[[1]],g2u[[1]],,vertex.label=NA,vertex.size=7,edge.arrow.size=0.2,main="Community Structure for ego node 1 - WALK TRAP")
plot(commu2[[1]],g2u[[1]],,vertex.label=NA,vertex.size=7,edge.arrow.size=0.2,main="Community Structure for ego node 1 - WALK TRAP")


match_index <- list()
match_index1 <- list()
match_index2<-list()
array1 <- list()
array2 <- list()
z <- 1
circles<-{{}}
{
  match_index1 <- list()
  for(i in 1:(length(list_no)-1)){
    if(length(commu1[[i]]) != 0){
      match_index1[[i]] <- list()
      for(c1 in 1:length(commu1[[i]])){
        match_index1[[i]][[c1]] <- list()
        array1 <- (commu1[[i]])$names[which(commu1[[i]]$membership==c1)]
        for(j in 1:length(circles[[i]])){
          array2 <- circles[[i]][[j]] 
          match_index1[[i]][[c1]][[j]] <- 2*length(intersect(array1,array2))/(length(array1)+length(array2))
          match_index1[[i]] <- 2*length(intersect(array1,array2))/(length(array1)+length(array2))
          z <- z + 1
          write(paste(c, ": communities ", i, "|", list_no[i], ".", c1, " ", j, "|", list_no[j], ".", " match_index - ", match_index1[[i]][[c1]][[j]]), file="/Volumes/Utkarsha/Study_Material/UCLA/Quarter_3/EE232/Project/out2.txt", append=TRUE)
        }
      }
    }
  }
}
############  END OF QUESTION_7   #############
######################## END OF PROJECT  ######################## 