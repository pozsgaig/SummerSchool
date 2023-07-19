# install.packages("rstudioapi", dependencies = TRUE)
# install.packages("shiny", dependencies = TRUE) # ! important
# install.packages("igraph", dependencies = TRUE) # ! important
# install.packages("gplots", dependencies = TRUE) # ! important
# install.packages("ggraph", dependencies = TRUE)
# install.packages("ggforce", dependencies = TRUE)
# install.packages("graphlayouts", dependencies = TRUE)
# install.packages("visNetwork", dependencies = TRUE) # ! important
# 
# if (!requireNamespace("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")
# 
# BiocManager::install("RedeR")
# 
# install.packages("snahelper", dependencies = TRUE)

# install.packages(
#     "https://github.com/vosonlab/VOSONDash/releases/download/v0.5.11/VOSONDash-0.5.11.tar.gz",
#     repo = NULL, type = "source")






library(rstudioapi)  # alternatively, set your working directory here  


library(shiny)
library(igraph)
library(gplots)
library(ggraph)
library(ggforce)
library(graphlayouts)
library(visNetwork)
library(RedeR)
library(snahelper)
library(VOSONDash)

# Setting up working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)

head(nodes)
head(links)

net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
net
E(net)       # The edges of the "net" object
V(net)       # The vertices of the "net" object
E(net)$type  # Edge attribute "type"
V(net)$media # Vertex attribute "media"

# Find nodes and edges by attribute:
# (that returns oblects of type vertex sequence/edge sequence)
V(net)[media=="BBC"]
E(net)[type=="mention"]

# You can also access the network matrix directly:
net[1,]
net[5,7]

# Get an edge list or a matrix:
as_edgelist(net, names=T)
as_adjacency_matrix(net, attr="weight")

# Or data frames describing nodes and edges:
as_data_frame(net, what="edges")
as_data_frame(net, what="vertices")
windows()
plot(net) # not a pretty picture!
net <- simplify(net, remove.multiple = F, remove.loops = T) 
plot(net, edge.arrow.size=.4,vertex.label=NA)


nodes2 <- read.csv("Dataset2-Media-User-Example-NODES.csv", header=T, as.is=T)
links2 <- read.csv("Dataset2-Media-User-Example-EDGES.csv", header=T, row.names=1)
links2 <- as.matrix(links2)
dim(links2)
dim(nodes2)
head(nodes2)
head(links2)

net2 <- graph_from_incidence_matrix(links2)
table(V(net2)$type)

# distances between nodes
dist_hist<-distance_table(net2)
barplot(dist_hist$res, names.arg = seq_along(dist_hist$res))

# degrees
degrees <- degree(net2)
barplot(degrees)



# Plot with curved edges (edge.curved=.1) and reduce arrow size:

# Note that using curved edges will allow you to see multiple links
# between two nodes (e.g. links going in either direction, or multiplex links)
plot(net, edge.arrow.size=.4, edge.curved=.6)

# Set edge color to light gray, the node & border color to orange 
# Replace the vertex label with the node names stored in "media"
plot(net, edge.arrow.size=.2, edge.color="light grey",
     vertex.color="maroon", vertex.frame.color="#ffffff",
     vertex.label=V(net)$media, vertex.label.color="black") 

# Generate colors based on media type:
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]

# Compute node degrees (#links) and use that to set node size:
deg <- degree(net, mode="all")
V(net)$size <- deg*3
betweenness(net)


# We could also use the audience size value:
V(net)$size <- V(net)$audience.size*0.6

# The labels are currently node IDs.
# Setting them to NA will render no labels:
V(net)$label <- NA

# Set edge width based on weight:
E(net)$width <- E(net)$weight/6

#change arrow size and edge color:
E(net)$arrow.size <- .2
E(net)$color <- "green"

# We can even set the network layout:
graph_attr(net, "layout") <- layout_with_lgl
plot(net) 

legend("topright", c("Newspaper","Television", "Online News"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)


### layouts ####
net.bg <- sample_pa(100) 
V(net.bg)$size <- 8
V(net.bg)$frame.color <- "white"
V(net.bg)$color <- "orange"
V(net.bg)$label <- "" 
E(net.bg)$arrow.mode <- 0

plot(net.bg)

plot(net.bg, layout=layout_randomly)

l <- layout_in_circle(net.bg)
plot(net.bg, layout=l)

layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 
# Remove layouts that do not apply to our graph.
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

par(mfrow=c(3,3), mar=c(1,1,1,1), ask=T)
for (layout in layouts) {
  print(layout)
  l <- do.call(layout, list(net)) 
  plot(net, edge.arrow.mode=0, layout=l, main=layout) }


par(mfrow=c(1,1), ask=F)

# Community detection (by optimizing modularity over partitions):
clp <- cluster_optimal(net)
class(clp)

# Community detection returns an object of class "communities" 
# which igraph knows how to plot: 
plot(clp, net)

windows()

# alternatively 

dendrogram <- cluster_edge_betweenness(net2)
plot_dendrogram(dendrogram, direction = "downwards")

# We can also plot the communities without relying on their built-in plot:
V(net)$community <- clp$membership
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
plot(net, vertex.color=colrs[V(net)$community])



### VisNetwork ####
require(visNetwork, quietly = TRUE)
nodes <- data.frame(id = 1:10, 
                    label = paste("Node", 1:10),                                 # add labels on nodes
                    group = c("GrA", "GrB"),                                     # add groups on nodes 
                    value = 1:10,                                                # size adding value
                    shape = c("square", "triangle", "box", "circle", "dot", "star",
                              "ellipse", "database", "text", "diamond"),                   # control shape of nodes
                    title = paste0("<p><b>", 1:10,"</b><br>Node !</p>"),         # tooltip (html or character)
                    color = c("darkred", "grey", "orange", "darkblue", "purple"),# color
                    shadow = c(FALSE, TRUE, FALSE, TRUE, TRUE))                  # shadow

head(nodes)


edges <- data.frame(from = sample(1:10, 8), to = sample(1:10, 8),
                    label = paste("Edge", 1:8),                                 # add labels on edges
                    length = c(100,500),                                        # length
                    arrows = c("to", "from", "middle", "middle;to"),            # arrows
                    dashes = c(TRUE, FALSE),                                    # dashes
                    title = paste("Edge", 1:8),                                 # tooltip (html or character)
                    smooth = c(FALSE, TRUE),                                    # smooth
                    shadow = c(FALSE, TRUE, FALSE, TRUE))                       # shadow
head(edges)

visNetwork(nodes, edges, width = "100%")


# using an other package

rdp <- RedPort()
calld(rdp)
addGraph(rdp, net)
updateGraph(rdp)


# x <- c(-70.2792, 14.8678, 62.0995, -25.187, -124.0367, -223.0483, 344.2958, 298.087, 130.1318, 189.9644, 238.3059, 43.9885,
#        74.8526, 301.0085, -260.5673, -402.8285, -160.0039)
# y <- c(-1149.8008, -998.1259, -1213.4644, -1285.206, -930.6548, -1314.292, -1220.4395, -1052.6934, -816.8599, -1008.6258,
#        -1367.7944, -1439.1412, -1640.7412, -1513.2818, -1100.6766, -1443.9313, -1496.6237)
# 
# ggraph(net, layout = "manual", x = x, y = y) + 
#   geom_edge_link0(edge_colour = "#A8A8A8", edge_width = 0.8, edge_alpha = 1,
#                   arrow = arrow(angle = 30, length = unit(0.15, "inches"), ends = "last", type = "closed")) + 
#   geom_node_point(aes(fill = type.label,
#                       size = audience.size), colour = "#000000", shape = 21, stroke = 0.3) + 
#   scale_fill_brewer(palette = "Set1", na.value = "gray53") + 
#   scale_size(range = c(3, 8)) + 
#   geom_node_text(aes(label = media), colour = "#000000", size = 6, family = "serif") + 
#   theme_graph() + 
#   theme(legend.position = "bottom")





runVOSONDash()
