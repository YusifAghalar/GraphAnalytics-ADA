egraph<-graph_from_data_frame(emailsDF)

#simplify graph
egraph <- igraph::simplify(
  egraph,
  remove.multiple = TRUE,
  remove.loops = TRUE,
)

#Edges
edges=E(egraph)
#Vertices
vertice=V(egraph)

#Edge density
e_density <- edge_density(egraph, loops = FALSE)

#Centrality
egraph.cent <- igraph::centr_betw(egraph)

#Diamter of graph
egraph.diamter <-igraph::diameter(egraph)


#########Other functions from igraph
##Delete edges from graph
egraph<-delete_edges(egraph,seq(1, 9, by = 2))

##Delete vertice from graph
############ NOTE NOTE NOTE NOTE NOTE #######################
# Error in as.igraph.vs(graph, v) : Invalid vertex names
egraph<-delete_vertices(egraph,"yusif.aghalarli@gmail.com")

## Add edges to graph
egraph <- add_edges(egraph, c("lindy.donoho@enron.com","david.delainey@enron.com"))
##Add vertice to graph
egraph <- add_vertices(egraph,2, name=c("yusif.aghalarli@gmail.com","sadiq.akhund@gmail.com"))

## Checks if graph has multiple edges 
egraph <- add_edges(egraph, c("lindy.donoho@enron.com","david.delainey@enron.com"))
egraph <- add_edges(egraph, c("lindy.donoho@enron.com","david.delainey@enron.com"))
any_multiple(egraph)
count_multiple(egraph)



#Central nodes(s) in the graph
#There are two types of centrality measure: local and global
#Local centrality does not take into acccount all network

#The simplest measure of centrality is degree centrality. 
#It counts how many edges each node
V(egraph)$degree <-degree (egraph) 
centralLocal <- V(egraph)[order(V(egraph)$degree,decreasing=TRUE)[1]] 
#Betweenness centrality captures which nodes are important in the flow of the network. 
#It makes use of the shortest paths in the network
V(egraph)$betweenness <- betweenness(egraph, directed = F)
centralGlobal <- V(egraph)[order(V(egraph)$betweenness , decreasing=TRUE)[1]] 

#Longest path(s)
egraph$longest_path<-get_diameter(egraph)
egraph$longest_distnace <- diameter(egraph)
#Largest clique
egraph$largest_clique <- largest_cliques(egraph)