# rm(list=ls())
library("stringr")
library("igraph")
library("sjmisc")
# takes folder as an input and returns list of files
fetchFiles <- function(repo, breadth = 0) {
  if(is.na(repo))
    return(NULL)
  
  files <- list.files(path = repo, all.files = F, full.names = T)
  subdirs <- list.dirs(path = repo, full.names = T, recursive = F)
  # if parameter not given then use length as breadth of recursion
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
    # 2. ..limit sub-folders with first 8
    retval <- append(retval, fetchFiles(subdir, 8))
  }
  return(retval)
}

matchBetween <- function(text, pat1, pat2){
  regex <- regexpr(pattern = paste(pat1, "(.*?)", pat2), text = text, perl = T)
  s <- attr(regex, "capture.start")
  e <- s + attr(regex, "capture.length") - 1
  substr(text,  s, e)
}

readAllFiles <- function(files){
  retval <- c()
  ## FYI: This part is resource-intensive
  for(fileName in files){
    content <- readChar(fileName, file.info(fileName)$size)
    retval <- append(retval, content)
  }
  return(retval)
}