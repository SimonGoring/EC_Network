library(dplyr)
library(purrr)
library(multidplyr)
library(RNeo4j)

file <- 'data/input/awards/9496153.xml'

nsf_files      <- data.frame(files = list.files('data/input/awards',
                                                full.names = TRUE, pattern = '.xml'),
                             stringsAsFactors = FALSE)

nsf_files$group <- (1:length(nsf_files) %% 4 + 1)
                   
read_award <- function(file) {
  input <- xml2::read_xml(file)
  input <- xml2::as_list(input)
  
  input
}

source('R/mergeNode.R')
source('R/mergeRel.R')
source('R/collapse_list.R')
source('R/parse_award.R')

cluster <- multidplyr::create_cluster(3)

by_group <- nsf_files %>% sample_n(nrow(nsf_files)) %>%
  partition(group, cluster = cluster)

cluster %>% cluster_library(packages = "dplyr") %>% 
  cluster_library(packages = "purrr")  %>%
  cluster_library(packages = "xml2")   %>%
  cluster_library(packages = "RNeo4j") %>%
  cluster_assign_value("read_award", read_award) %>% 
  cluster_assign_value("mergeRel",   mergeRel)   %>% 
  cluster_assign_value("mergeNode",  mergeNode)  %>%
  cluster_assign_value("parse_award",  parse_award)  %>%
  cluster_assign_value("collapse_list",  collapse_list)

files_in_parallel <- by_group %>% # Use by_group party_df
  mutate(
    lists = map(.x = files, 
                       ~ try(parse_award(read_award(.x)))
    )
  ) %>%
  collect() # Special collect() function to recombine partitions

# The failed nodes:
failed <- unlist(build_nodes)
for(i in 1:length(failed)) {
  cat(i)
  test <- read_award(failed[i]) %>% parse_award
  
}

input <- read_award(failed[1])
input
