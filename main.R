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

g=graph_from_data_frame(emailsDF)

#Edges
edges=E(g)
#Vertices
vertice=V(g)

#Edge density
e_density = edge_density(g, loops = FALSE)

#Degree of each node
igraph::degree(g)

#Centrality
igraph::centr_betw(g)
