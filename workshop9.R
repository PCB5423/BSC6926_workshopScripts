# """ Workshop 9: Food webs: trophic networks
#     author: BSC 6926 B53
#     date: 11/15/2022"""

# This workshop covers food web analysis using trophic networks.\


## Trophic networks
# Trophic networks represent the connection between species in a food web. In R they take advantage of tools and packages related to network analysis. For these analyses we need information about the species in the food web and the trophic linkages between them. We will use the `igraph` package for these analyses. 
library(tidyverse)
library(igraph)

# load trophic groups
tg = read_csv("data/foodweb_example.csv")

tg

#Trophic links
tl = read_csv("data/trophic_links.csv")

tl

## Setting up data for igraph
#We will use the trophic linkages dataset to define the food web network. `vcount()` displays the number of nodes (or in this case species) in the food web. `ecount()` is the number of trophic linkages in the food web. We can also add properties to the network.
#define a adjacency matrix
tl.net = graph.data.frame(tl, directed = TRUE)

tl.net

#Get info igraph structure
summary(tl.net)
str(tl.net)

# number of nodes
vcount(tl.net)

# number of trophic linkages
ecount(tl.net)

#Change or add new descriptors
V(tl.net)$Species = V(tl.net)$name

# add group names to network
nodes = as.data.frame(V(tl.net)$name)
colnames(nodes) = c("Taxon")
nodes = left_join(nodes, tg, by = "Taxon")

V(tl.net)$FG = nodes$Group
V(tl.net)$TL = nodes$TL

# add weight to the network 
is.weighted(tl.net)
E(tl.net)$weight = E(tl.net)$Weight
is.weighted(tl.net)

# Plotting the network
#Random
plot(tl.net, vertex.label.color="black", vertex.label.dist=1, 
     edge.arrow.size = .3, vertex.label.cex = 0.8)

#Circular
plot(tl.net, vertex.label.color="black", vertex.label.dist=1, 
     edge.arrow.size = .3, vertex.label.cex = 0.8, layout = layout_in_circle)

#Kamada Kawai
plot(tl.net, vertex.label.color="black", vertex.label.dist=1, 
     edge.arrow.size = .3, vertex.label.cex = 0.8, layout = layout_with_kk)

#Tree
plot(tl.net, vertex.label.color="black", vertex.label.dist=1, 
     edge.arrow.size = .3, vertex.label.cex = 0.8, layout = layout_as_tree)

# with trophic level as y
lay = as.matrix(bind_cols(x = sample(1:vcount(tl.net)), y = nodes$TL))

plot(tl.net, vertex.label.color="black", vertex.label.dist=1, 
     edge.arrow.size = .3, vertex.label.cex = 0.5, layout = lay)

# add color to plots
library(RColorBrewer)
display.brewer.pal(5, "Dark2")

my_pal = tibble(col = brewer.pal(7, "Dark2"),
                Group = unique(tg$Group))

nodes = left_join(nodes, my_pal, by = 'Group')

plot(tl.net, vertex.label.color="black", vertex.label.dist=1, 
     edge.arrow.size = .3, vertex.label.cex = 0.5, layout = lay,
     vertex.color = nodes$col, displaylabels = TRUE)

plot(tl.net, vertex.label.color="black",
     edge.arrow.size = .3, layout = lay,
     vertex.color = nodes$col, vertex.label = NA)

# add weight of line to food web
plot(tl.net, vertex.label.color="black",
     edge.arrow.size = .3, layout = lay,
     vertex.color = nodes$col, vertex.label = NA,
     edge.width = E(tl.net)$Weight*8)

# add legend
legend("bottomleft", legend = my_pal$Group, pch = 21, col = "black",
       pt.bg = my_pal$col, pt.cex = 2, cex = .8, bty = "n", ncol = 1)
### Custom functions for plotting
#[Kortsch et al.](https://rfrelat.github.io/BalticFoodWeb.html) created custom functions to take trophic linkages netwrok and automatically plot based on a calculated trophic level that averages based on all of the trophic linkages of the node. 

## functions
trophiclevels = function(net){
  #Get adjacency matrix 
  mat <- get.adjacency(net, sparse=F)
  
  #Detect basal node
  basal <- rownames(subset(mat, apply(mat, 2, sum)==0) & apply(mat, 1, sum)!= 0)
  #Compute the shortest path to basal node
  paths_prey <- suppressWarnings(shortest.paths(graph = net, v= V(net), to = V(net)[basal], 
                                                mode = "in", weights = NULL, algorithm = "unweighted"))
  
  paths_prey[is.infinite(paths_prey)] <- NA
  shortest_paths <- suppressWarnings(as.matrix(apply(paths_prey, 1, min, na.rm=TRUE)))
  #for species with no prey apart of them
  shortest_paths[is.infinite(shortest_paths)] <- NA
  
  # Shortest TL
  sTL <- 1 + shortest_paths
  
  # Compute average prey trophic level
  # inspired from cheddar package calculation
  W <- t(mat)
  rs <- rowSums(W)
  W <- W/matrix(rs, ncol = ncol(W), nrow = nrow(W))
  W[0 == rs, ] <- 0
  I <- diag(ncol(W))
  tl0<-rowSums(I - W)
  result <- tryCatch(solve(I - W), error = function(e) e)
  if ("error" %in% class(result)) {
    avtl <- rep(NA, ncol(pm))
    names(avtl) <- colnames(pm)
  }
  else {
    avtl <- rowSums(result)
  }
  
  # Short-weighted TL is the average between 
  # Shortest TL and average prey TL
  SWTL <- (sTL + avtl)/2
  
  return(SWTL)
}


plotfw = function(net, col=NULL, lab=NULL, size=NULL,
                  nylevel=7, maxsize=10, labcex=0.01,
                  ynum=6, ylab= "Trophic Level", ...){
  n <- vcount(net)
  if (!is.null(col)){
    V(net)$color <- col
  } else{
    V(net)$color <- rainbow(vcount(net))
  }
  if (!is.null(lab)){
    V(net)$name <- lab
  }
  if (!is.null(size)){
    V(net)$size <- size
  } else {
    V(net)$size <- maxsize/2
  }
  
  tl <- trophiclevels(net)
  dgpred <- tl
  
  bks <- c(0.9, seq(1.9, max(tl), length.out = nylevel))
  ynod <- cut(tl, breaks = bks, include.lowest = TRUE, 
              labels = 1:(length(bks)-1))
  
  maxx <- max(table(ynod))
  xnod <- rep(0,n)
  for (i in 1:nylevel){
    l <- sum(ynod==i)
    
    ltr <- (l/maxx)**(1/2)*maxx
    if (l>1) {
      xnod[ynod==i] <- seq(-ltr,ltr,length.out = l)
    } else {
      xnod[ynod==i] <- 0
    }
  }
  
  coo <- cbind(xnod,tl)
  
  #y axis with 1 and continuous axis from 2 to max TL.
  yax <- c(1, seq(2, max(tl), length.out = ynum-1))
  labax <- round(yax, 1)
  #rescale xax between -1 and 1
  laby <- (yax-min(yax))/(max(yax)-min(yax))*2 - 1
  
  plot(net, layout= coo, vertex.label.color="black", 
       vertex.label.cex=labcex, ...)
  axis(2, at = laby, labels = labax)
  mtext(ylab, side = 2, line=2)
  res <- data.frame(coo, "size"= V(net)$size, "color"= V(net)$color)
  names(res) <- c("x", "y", "size", "color")
  row.names(res) <- V(net)$name
  invisible(res)
}

### Plot
plotfw(tl.net, col=nodes$col, size = 10,
       edge.width=0.3, edge.arrow.size=0.3)

## Food web characteristics
#We can calculate different topological metrics that can tell us about the food web

### Species richness
#Species richness ($S$) is the same as the number of nodes. \
#*Note - nodes can also be functional groups and not just species*

S = vcount(tl.net)
S

### Connectance 
#Connectance ($C$) is the proportion of trophic links ($L$) realized out of the maximum number of possible links. Because self-loops (cannibalistic links) were removed, the number of possible links is not $S^2$ as with cannibalism included, but $S∗(S−1)$, therefore
#  C = \frac{L}{S*(S-1)}

C = ecount(tl.net)/(S*(S-1))
C
edge_density(tl.net, loops=F) 

### Generality
#Generality is the number of resources used per taxon (i.e., number of linkages pointing to a node)
# Generality
# Identify predator nodes, i.e. taxa with at least one prey
pred = degree(tl.net, mode="in")>0
# Compute mean generality of the food web, i.e. mean number of prey per predators
G = sum(degree(tl.net, mode="in")[pred])/sum(pred)
G
### Vulnerability

prey = degree(tl.net, mode="out")>0
# Compute the mean vulnerability, i.e. mean number of predators per prey
V = sum(degree(tl.net, mode="out")[prey])/sum(prey)
V
## Seagrass loss
#Let's see how the food web changes once we remove seagrass as a source, similar to large scale seagrass loss. 

tl_noSG = tl %>% 
  filter(Diet != 'Seagrass detritus') 

noSG.net = graph.data.frame(tl_noSG, directed = TRUE)

# add group names to network
nodes_noSG = as.data.frame(V(noSG.net)$name)
colnames(nodes_noSG) = c("Taxon")
nodes_noSG = left_join(nodes_noSG, tg, by = "Taxon") %>% 
  left_join(my_pal, by = 'Group')

V(noSG.net)$FG = nodes_noSG$Group

noSG_pal = brewer.pal(7, "Dark2")
noSG_col = as.factor(get.vertex.attribute(noSG.net, "FG"))

plotfw(tl.net, col=nodes$col, size = 10,
       edge.width=0.3, edge.arrow.size=0.3)

plotfw(noSG.net, col=nodes_noSG$col, size = 10,
       edge.width=0.3, edge.arrow.size=0.3)


# connectance 
edge_density(tl.net, loops=F) 
edge_density(noSG.net, loops = F)

# Generality
# Identify predator nodes, i.e. taxa with at least one prey
pred = degree(tl.net, mode="in")>0
pred_noSG = degree(noSG.net, mode = 'in')>0
# Compute mean generality of the food web, i.e. mean number of prey per predators
sum(degree(tl.net, mode="in")[pred])/sum(pred)
sum(degree(tl.net, mode="in")[pred_noSG])/sum(pred_noSG)

# Vulnerability
prey = degree(tl.net, mode="out")>0
prey_noSG = degree(noSG.net, mode = 'out')>0
# Compute the mean vulnerability, i.e. mean number of predators per prey
sum(degree(tl.net, mode="out")[prey])/sum(prey)
sum(degree(noSG.net, mode="out")[prey_noSG])/sum(prey_noSG)

