
#Loading from dataframe
egraph<-graph_from_data_frame(emailsDF,directed=FALSE)
#Simplify graph
egraph <- igraph::simplify(
  egraph,
  remove.multiple = TRUE,
  remove.loops = TRUE,
  edge.attr.comb = list(Subject="first")
)
#Delete vertices with degree less than mean to simplify graph
cut.off <- mean(degree(egraph))*4
egraph<-delete_vertices(egraph,V(egraph)[degree(egraph)<cut.off])
extractName <- function(x) strsplit(x, "@")[[c(1, 1)]]
egraph<- set.vertex.attribute(egraph, "name", value= sapply(V(egraph)$name,extractName))


#Edges
edges=E(egraph)
#Vertices
vertice=V(egraph)
#Edge density
e_density <- edge_density(egraph, loops = FALSE)
#Centrality
egraph.cent <- igraph::centr_betw(egraph)
hist(egraph.cent$res,main="The betweenness of vertices")
#Diamter of graph
egraph.diamter <-igraph::diameter(egraph)

deg <- degree(egraph, mode="all")
hist(deg, breaks=100, main="Histogram of node degree")

################################################
################################################
#########Other functions from igraph############



## Add edges to graph
egraph <- add_edges(egraph, c("iris.mack","clint.dean"))

##Add vertice to graph
egraph <- add_vertices(egraph,2, name=c("yusif.aghalarli","sadiq.akhund"))
##Delete edges from graph
egraph<-delete_edges(egraph,seq(1, 9, by = 2))

##Delete vertice from graph
egraph<-delete_vertices(egraph,"yusif.aghalarli")

## Checks if graph has multiple edges 
egraph <- add_edges(egraph, c("iris.mack","clint.dean"))
egraph <- add_edges(egraph, c("iris.mack","clint.dean"))
any_multiple(egraph)
## Get number of multiple eges
count_multiple(egraph)
## Check if two vertices are connecetd
are.connected(egraph,"iris.mack","clint.dean")
##Calculate transitivity
##global - ratio of triangles (direction disregarded) to connected triples.
##local - ratio of triangles to connected triples each vertex is part of.
transitivity(egraph, type="global") 
transitivity(egraph, type="local")
##Average path length: the mean of the shortest distance between each pair
mean_distance(egraph, directed=F)

#Neighbors of a vertex
neig <-neighbors(egraph, V(egraph)[name=="technology.enron"], mode="out")
# From the given start vertex, take the given number of steps,
#choosing an edge from the actual vertex uniformly randomly. 
random_walk(egraph, start = 1, steps = 6)

#Central nodes(s) in the graph
#There are two types of centrality measure: local and global
#Local centrality does not take into acccount all network

#The simplest measure of centrality is degree centrality. 
#It counts how many edges each node
V(egraph)$degree <-degree (egraph) 
centralLocal <- V(egraph)[order(V(egraph)$degree,decreasing=TRUE)[1:5]] 

#Betweenness centrality captures which nodes are important in the flow of the network. 
#It makes use of the shortest paths in the network
V(egraph)$betweenness <- betweenness(egraph, directed = F)
centralGlobal <- V(egraph)[order(V(egraph)$betweenness , decreasing=TRUE)[1:5]] 

#Longest path(s)
egraph$longest_path<-get_diameter(egraph)
egraph$longest_distance <- diameter(egraph)

#Power centrality
most_powerfull_10 = sort(round(power_centrality(egraph, exp=0.3), 2),decreasing = TRUE)[1:10]

#Find community
communities = cluster_edge_betweenness(egraph)
largest_community=sort(sizes(communities)[1])

plot(cluster_walktrap(egraph),egraph,vertex.label.cex=0.001)



#Clique
egraph.sym <- as.undirected(egraph, mode= "collapse",
                         
                         edge.attr.comb=list(weight="sum", "ignore"))
cliques(egraph.sym) # list of cliques       

sapply(cliques(egraph.sym), length) # clique sizes

largest_cliques(egraph.sym) # cliques with max number of nodes



#Ego graphs, plot largest one
egos <-make_ego_graph(egraph)
ego <- egos[[order(ego_size(egraph),decreasing=TRUE)[3]]]
plot(ego, 
     edge.width = 2, vertex.color="green",vertex.size=15)


