source("./header.R")

mainDir <- "./maildir"
allFiles <- fetchFiles(mainDir)
contents <- readAllFiles(allFiles)
# 3. ... use 10.000 of email/files randomly
contents <- sample(x = contents, size = 10000)

# Create DataFrame
emailsDF <- data.frame(matrix(ncol = 3, nrow = 0))

# Populate DataFrame
#### TODO: Make this as function/Tip: create df inside func. then return
for(i in 1:length(contents)){
  # get and clean each email
  lines <- contents[i]
  lines <- str_replace_all(lines, "[\r\n\t]" , " ")
  # Fetch 'From' field:
  From = matchBetween(lines, "From:", "To:")
  From = trimws(From)
  # Fetch 'To' field:
  To = matchBetween(lines, "To:", "Subject:")
  To = strsplit(To, split = ",")
  To = lapply(To, trimws)
  # Fetch 'Subject' field:
  Subject = matchBetween(lines, "Subject:", "Cc:")
  if(nchar(Subject) < 2 || grepl("From", Subject, fixed = T)){
    Subject = matchBetween(lines, "Subject:", "Mime-Version:")
  }
  Subject = trimws(Subject)
  
  ## Anomalies:
  # Condition 1: some emails do not match the standard structure, so skip
  # Condition 2: some emails have x-From instead of From, thus we get clutter
  if((is_empty(To) || is_empty(From)) || nchar(From) > 100){
    next
  }
  
  print(paste("Email:", i))  
  
  for(t in To){
    df <- data.frame(From, t, Subject)
    emailsDF <- rbind(emailsDF, df)
  }   
}

# Rename Columns
colnames(emailsDF) <- c("From", "To", "Subject")

#write.csv(x = emailsDF, "./emails.csv", row.names = F)

egraph<-graph_from_data_frame(emailsDF)

#simplify graph
egraph <- igraph::simplify(
  egraph,
  remove.multiple = TRUE,
  remove.loops = TRUE,
)
egraph<-delete_vertices(egraph,V(egraph)[degree(egraph)<5])
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
egraph$longest_distance <- diameter(egraph)
#Largest clique
egraph$largest_clique <- largest_cliques(egraph)
#Power centrality
most_powerfull_10 = sort(round(power_centrality(egraph, exp=0.5), 2),decreasing = TRUE)[1:10]


