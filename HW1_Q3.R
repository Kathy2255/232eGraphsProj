library("igraph")
#####MAKING THE NETWORK
g1 <- aging.prefatt.game(1000, pa.exp=1, aging.exp=0, aging.bin=1000)
g2 <- aging.prefatt.game(1000, pa.exp=1, aging.exp=-1,   aging.bin=1000)
g3 <- aging.prefatt.game(1000, pa.exp=1, aging.exp=-3,   aging.bin=1000)

##### DEGREE DISTRIBUTION
barplot(degree.distribution(g1),main="Degree Distribution Histogram - G1(Aging.exp=0)")
plot(degree.distribution(g1),main="Degree Distribution Plot - G1(Aging.exp=0)")
barplot(degree.distribution(g2),main="Degree Distribution Histogram - G2(Aging.exp=-1)")
plot(degree.distribution(g2),main="Degree Distribution Plot - G2(Aging.exp=-1)")
barplot(degree.distribution(g3),main="Degree Distribution Histogram - G3(Aging.exp=-3)")
plot(degree.distribution(g3),main="Degree Distribution Plot - G3(Aging.exp=-3)")


#########APPLYYING FAST GREEDY COMMUNITY DETECTION ALGORITHM
ug1<- as.undirected(g1)
fg1<-fastgreedy.community(ug1)
hist(fg1$membership,breaks=50)
hist(fg1$modularity,breaks=50)
ug2<- as.undirected(g2)
fg2<-fastgreedy.community(ug2)
hist(fg2$membership,breaks=50)
hist(fg2$modularity,breaks=50)
ug3<- as.undirected(g3)
fg3<-fastgreedy.community(ug3)
hist(fg3$membership,breaks=50)
hist(fg3$modularity,breaks=50)

###### PLOTTING TH COMMUNITY SIZE DISTRIBUTION
ctm1 = community.to.membership(g1, fg1$merges, step=which.max(fg1$modularity)-1)
barplot(table(ctm1$csize),main="Community Size - G1(Aging.exp=0)")
ctm2 = community.to.membership(g2, fg2$merges, step=which.max(fg2$modularity)-1)
barplot(table(ctm2$csize),main="Community Size - G2(Aging.exp=-1)")
ctm3 = community.to.membership(g3, fg3$merges, step=which.max(fg3$modularity)-1)
barplot(table(ctm3$csize),main="Community Size - G3(Aging.exp=-3)")
