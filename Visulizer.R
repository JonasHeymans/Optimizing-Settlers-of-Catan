#install.packages("igraph", dependencies = TRUE)

rm(list=ls(all=TRUE))       #Clear Environment
setwd("C:/Users/Callie Ann/Documents/Github/Settlers-of-Catan")     #Set the working directory

nodes <- read.csv("Dataset-NODES.csv", header = T, as.is = T)
links <- read.csv("Dataset-EDGES.csv", header = T, as.is = T)
probability <- read.table("RollProbabilities.txt", header = T)


head(nodes)
head(links)
head(probability)

nrow(nodes); length(unique(nodes$id))
nrow(unique(nodes[c("name.type")]))
nrow(links); nrow(unique(links[,c("from","to")]))

links <- links[order(links$from, links$to),]

##################################
#Initial Graph

library(igraph)

#Building the inital Graph
net <- graph_from_data_frame(d=links, vertices = nodes, directed = F)

colrs <- c("burlywood", "chocolate", "gold", "darkgreen", "dimgrey", "darkolivegreen1", "aliceblue")
V(net)$color <- colrs[V(net)$name.type]

shp <- c("square","square","square","square","square","square","circle")
V(net)$shape <- shp[V(net)$name.type]

l <- layout.auto(net)

plot(net, vertex.label=V(net)$name.roll, edge.arrow.size=.2, layout = l)

##################################
#Calculate Probability

#install.packages("tidyverse", dependencies = TRUE)
#install.packages("dplyr", dependencies = TRUE)

library(dplyr)

#First we extract the roll values for each tile

name.roll = vertex_attr(net,"name.roll", index = V(net)[1:19])
name = vertex_attr(net,"name", index = V(net)[1:19])
roll <- data.frame(name, name.roll)

#Probability is assigned for the roll value
roll <- left_join(roll, probability, by = c("name.roll" = "roll"), match = "first")

#Added as an attribute for each node
V(net)$roll.prob <- NA

net <- set_vertex_attr(net, "roll.prob", index= V(net)[1:19], roll$probability)
  
vertex_attr(net, "roll.prob", index = V(net))

#vertex_attr(net, index = V(net))

#######################
#Finding the expected value for the intersections by Finding the Sum of Probabilites
#from the neighbors
V(net)$value.prob <- NA

#calulations
al <- as_adj_list(net, mode = c("all"))
prob <- V(net)$roll.prob
V(net)$value.prob <- sapply(al, function(x) sum(prob[x]))
V(net)$value.prob

#Assigning Size and Color to the intersections
V(net)$size <- (V(net)$value.prob)*36

V(net)$size[1:19] <- 20

heat <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                              "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

col <- heat(max(V(net)$size[20:73])+1)
col <- col[V(net)$size[20:73]+1]

V(net)$color[20:73] <- col

plot(net, vertex.label=V(net)$name.roll, layout = l)

#########################################
#Calculating the Resource Scarsity and using it as a weight

vertex_attr_names(net)

#adding the information about the type of tile
name.type = vertex_attr(net, "name.type", index = V(net)[1:19])
roll <- data.frame(roll, name.type)


#We are now calculating the propotion or weight each tile has
desert = 0
brick= 0
grain = 0
lumber = 0
ore = 0
wool = 0

for(x in 1:19){
  if(roll[,"name.type"][x] == 2){
    brick = brick + roll[,"probability"][x]
  }else if(roll[,"name.type"][x] == 3){
    grain = grain + roll[,"probability"][x]
  }else if(roll[,"name.type"][x] == 4){
    lumber = lumber + roll[,"probability"][x]
  }else if(roll[,"name.type"][x] == 5){
    ore = ore + roll[,"probability"][x]
  }else if(roll[,"name.type"][x] == 6){
    wool = wool + roll[,"probability"][x]
  }else{
    desert = 0
  }
}

roll.scar <- vector()

for(y in 1:19){
  if(roll[,"name.type"][y] == 2){
    roll.scar[y] <- roll[,"probability"][y]/brick
  }else if(roll[,"name.type"][y] == 3){
    roll.scar[y] <- roll[,"probability"][y]/grain
  }else if(roll[,"name.type"][y] == 4){
    roll.scar[y] <- roll[,"probability"][y]/lumber
  }else if(roll[,"name.type"][y] == 5){
    roll.scar[y] <- roll[,"probability"][y]/ore
  }else if(roll[,"name.type"][y] == 6){
    roll.scar[y] <- roll[,"probability"][y]/wool
  }else{
    roll.scar[y] <- 0
  }
}

roll <- data.frame(roll, roll.scar)

#Add to atribute list for each tile
V(net)$roll.scar <- NA

net <- set_vertex_attr(net, "roll.scar", index= V(net)[1:19], roll$roll.scar)

vertex_attr(net, "roll.scar", index = V(net))

#########################################################################
#Calulating the sum of the proportions:

V(net)$value.scar <- NA

al <- as_adj_list(net, mode = c("in"))
scar <- V(net)$roll.scar
V(net)$value.scar <- sapply(al, function(x) sum(scar[x]))
V(net)$value.scar

#Assigning Size and Color to Graph:
V(net)$size <- (V(net)$value.scar)*9

V(net)$size[1:19] <- 20

heat <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                           "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

col <- heat(max(V(net)$size[20:73])+1)
col <- col[V(net)$size[20:73]+1]

V(net)$color[20:73] <- col

plot(net, vertex.label=V(net)$name.roll, layout = l)

###############################################################################
#Looking at the Distance to Ports and the Threat-Point of the Goods

nodes2 <- read.csv("Pathway-NODES.csv", header = T, as.is = T)
links2 <- read.csv("Pathway-EDGES.csv", header = T, as.is = T)

head(nodes2)
head(links2)

nrow(nodes2); length(unique(nodes2$id))
nrow(unique(nodes2[c("trade.type")]))
nrow(links2); nrow(unique(links2[,c("from","to")]))

links2 <- links2[order(links2$from, links2$to),]

pathway <- graph_from_data_frame(d=links2, vertices = nodes2, directed = F)

colrs <- c("blue", "chocolate", "gold", "darkgreen", "dimgrey", "darkolivegreen1", "aliceblue")

V(pathway)$color <- colrs[V(pathway)$trade.type]

shp <- c("square","square","square","square","square","square","circle")
V(pathway)$shape <- shp[V(pathway)$trade.type]

t <- layout.auto(pathway)

plot(pathway, vertex.label=V(pathway)$trade.type, layout = t)

####################################################
#Calculate the distance from Brick and everything

vertex_attr_names(pathway)

dist.from.brick <- distances(pathway, v=V(pathway)[trade.label == "Brick"], 
                             to=V(pathway), mode=c("all"), weights = NA)

oranges <- colorRampPalette(c("dark red", "yellow"))

brk <- oranges(max(dist.from.brick)+1)
brk <- brk[dist.from.brick+1]

V(pathway)$color[10:63] <- brk[10:63]

plot(pathway, vertex.label=V(pathway)$trade.type, layout = t)

V(pathway)$port.brk[10:63] <- dist.from.brick[10:63]

#Calculate the distance from Grain

dist.from.grain <- distances(pathway, v=V(pathway)[trade.label == "Grain"], 
                             to=V(pathway), mode=c("all"), weights = NA)

grn <- oranges(max(dist.from.grain)+1)
grn <- grn[dist.from.grain+1]

V(pathway)$color[10:63] <- grn[10:63]

plot(pathway, vertex.label=V(pathway)$trade.type, layout = t)

V(pathway)$port.grn[10:63] <- dist.from.grain[10:63]

#Distance from Lumber
dist.from.lumber <- distances(pathway, v=V(pathway)[trade.label == "Lumber"], 
                             to=V(pathway), mode=c("all"), weights = NA)

lmbr <- oranges(max(dist.from.lumber)+1)
lmbr <- lmbr[dist.from.lumber+1]

V(pathway)$color[10:63] <- lmbr[10:63]

plot(pathway, vertex.label=V(pathway)$trade.type, layout = t)

V(pathway)$port.lmbr[10:63] <- dist.from.lumber[10:63]

#Distance from Ore
dist.from.ore <- distances(pathway, v=V(pathway)[trade.label == "Ore"], 
                              to=V(pathway), mode=c("all"), weights = NA)

ore1 <- oranges(max(dist.from.ore)+1)
ore1 <- ore1[dist.from.ore+1]

V(pathway)$color[10:63] <- ore1[10:63]

plot(pathway, vertex.label=V(pathway)$trade.type, layout = t)

V(pathway)$port.ore1[10:63] <- dist.from.ore[10:63]

#Distance from Wool
dist.from.wool <- distances(pathway, v=V(pathway)[trade.label == "Wool"], 
                           to=V(pathway), mode=c("all"), weights = NA)

wl <- oranges(max(dist.from.wool)+1)
wl <- wl[dist.from.wool+1]

V(pathway)$color[10:63] <- wl[10:63]

plot(pathway, vertex.label=V(pathway)$trade.type, layout = t)

V(pathway)$port.wl[10:63] <- dist.from.wool[10:63]

#Distance from Everything Ports
dist.from.everything <- distances(pathway, v=V(pathway)[trade.type == "1"], 
                            to=V(pathway), mode=c("all"), weights = NA)


dist.from.every.min <- apply(dist.from.everything, 2, min)

evrthn <- oranges(max(dist.from.every.min)+1)
evrthn <- evrthn[dist.from.every.min+1]

V(pathway)$color[10:63] <- evrthn[10:63]

plot(pathway, vertex.label=V(pathway)$trade.type, layout = t)

V(pathway)$port.evrthn[10:63] <- dist.from.every.min[10:63]

#################################################################################
#Combine the two Graphs

V(net)$color <- NA
V(pathway)$color <- NA

final_net_node <- as.data.frame(get.vertex.attribute(net))
final_net_edge <- as.data.frame(get.edgelist(net))

final_pathway_node <- as.data.frame(get.vertex.attribute(pathway))
final_pathway_edge <- as.data.frame(get.edgelist(pathway))

withports_node <- full_join(final_net_node, final_pathway_node, by = NULL)
withports_edge <- rbind(final_net_edge, final_pathway_edge)

withports_edge <- withports_edge[order(withports_edge$V1),]

withports <- graph_from_data_frame(d=withports_edge,vertices = withports_node,  directed = F)

v <- layout.auto(withports)

plot(withports, vertex.label= NA, layout = v)

vertex_attr_names(withports)








