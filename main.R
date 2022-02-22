mainDir <- "./maildir"

# takes folder as an input and returns list of files
fetchFiles <- function(repo, breadth = 0) {
  if(is.na(repo))
    return(NULL)
  
  files <- list.files(path = repo, all.files = F, full.names = T)
  subdirs <- list.dirs(path = repo, full.names = T, recursive = F)
  breadth <- if(breadth == 0) length(subdirs) else breadth
  subdirs <- subdirs[1: breadth]
  
  
  retval <- c()
  # process Files
  for(file in files){
    if(!dir.exists(file)){
      retval <- append(retval, file)
    }
  }
  # process Directories
  for(subdir in subdirs){
    retval <- append(retval, fetchFiles(subdir, 8))
  }
  return(retval)
}

allFiles <- fetchFiles(mainDir)
contents <- c()
## FYI: This part is resource-intensive
for(fileName in allFiles){
  content <- readChar(fileName, file.info(fileName)$size)
  contents <- append(contents, content)
}

## Retrieve "From", "To", "Subject" fields
contacts <- data.frame(row.names =  c("From", "To", "Subject"))

library("stringr")

matchBetween <- function(text, pat1, pat2){
  regex <- regexpr(pattern = paste(pat1, "(.*?)", pat2), text = text, perl = T)
  s <- attr(regex, "capture.start")
  e <- s + attr(regex, "capture.length") - 1
  substr(text,  s, e)
}


for(i in 1:64){
  lines <- contents[i]
  lines <- str_replace_all(lines, "[\r\n\t]" , " ")
  
  From = matchBetween(lines, "From:", "To:")
  To = matchBetween(lines, "To:", "Subject:")
  Subject = matchBetween(lines, "Subject:", "Cc:")
  if(length(Subject) < 2)
    Subject = matchBetween(lines, "Subject:", "Mime-Version:")

  print(paste("---->From: ", From))
  print(paste("-------To:", To))
  print(paste("--Subject:", Subject))
}

