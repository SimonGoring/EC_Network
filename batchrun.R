args <- commandArgs(trailingOnly = TRUE)

library(dplyr)
library(purrr)
library(multidplyr)
library(RNeo4j)

read_award <- function(file) {
  input <- xml2::read_xml(file)
  input <- xml2::as_list(input)
  
  input
}

source('R/mergeNode.R')
source('R/mergeRel.R')
source('R/collapse_list.R')
source('R/parse_award.R')

nsf_files <- read.csv(paste0('data/input/files', args[1], '.csv'), stringsAsFactors = FALSE)

for(i in 1:nrow(nsf_files)) {
  parse_award(read_award(nsf_files[i,1]))
}
