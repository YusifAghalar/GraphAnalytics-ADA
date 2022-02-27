# Clear environment before running
rm(list=ls())

# load all the necessary libraries
library("stringr")
library("sjmisc")
library("igraph")
library("dplyr")
library("tcltk")

# load the header file with user-defined functions
source("./src/header.R")

# start processing the emails
source("./src/process.R") # takes time...

## write to .csv file 
write.csv(x = emailsDF, "./res/emails.csv", row.names = F)

# start analyzing the network
source("./src/analyze.R")

# visualize results
source("./src/visualize.R")













