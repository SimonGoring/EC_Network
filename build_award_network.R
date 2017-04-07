library(dplyr)
library(purrr)
library(multidplyr)
library(RNeo4j)
library(lubridate)

file <- 'data/input/awards/9496153.xml'

nsf_files      <- data.frame(files = list.files('data/input/awards',
                                                full.names = TRUE, pattern = '.xml'),
                             stringsAsFactors = FALSE)

nsf_files$group <- (1:nrow(nsf_files) %% 10) + 1
     
read_award <- function(file) {
  input <- xml2::read_xml(file) %>% xml2::as_list()
  
  input
}

text_output <- function(x){
  x <- unlist(x)
  date <- try(read_award(x[1])$Award$AwardEffectiveDate %>% unlist %>% dmy)
  
  if(class(date) == 'try-error') {
    return(data.frame(file = x[1], group = x[2], year = NA, parsed = NA, 
                      stringsAsFactors = FALSE))  
  }
  
  return(data.frame(file = x[1], group = x[2], year = date, parsed = NA, 
                    stringsAsFactors = FALSE))
}

end_table <- nsf_files %>% 
  by_row(function(x){
    tester <- try(text_output(x))
    if('data.frame' %in% class(tester)) return(tester)
    return(data.frame(file = x[1], group = x[2], 
                      year = NA, parsed = NA, 
                      stringsAsFactors = FALSE))},
    .labels = FALSE)

end_table <- end_table[[1]] %>% bind_rows()

write.csv(end_table, file = 'data/output/end_table.csv', row.names = TRUE)

source('R/mergeNode.R')
source('R/mergeRel.R')
source('R/collapse_list.R')
source('R/parse_award.R')

#####################################################
# Parallel way:
# cluster <- multidplyr::create_cluster(3)
# 
# by_group <- nsf_files %>% sample_n(nrow(nsf_files)) %>%
#   partition(group, cluster = cluster)
# 
# cluster %>% cluster_library(packages = "dplyr") %>% 
#   cluster_library(packages = "purrr")  %>%
#   cluster_library(packages = "xml2")   %>%
#   cluster_library(packages = "RNeo4j") %>%
#   cluster_assign_value("read_award", read_award) %>% 
#   cluster_assign_value("mergeRel",   mergeRel)   %>% 
#   cluster_assign_value("mergeNode",  mergeNode)  %>%
#   cluster_assign_value("parse_award",  parse_award)  %>%
#   cluster_assign_value("collapse_list",  collapse_list)
# 
# files_in_parallel <- by_group %>% # Use by_group party_df
#   mutate(
#     lists = map(.x = files, 
#                        ~ try(parse_award(read_award(.x)))
#     )
#   ) %>%
#   collect() # Special collect() function to recombine partitions
# 
#################################################
#  Serial Way:

for(i in unique(end_table$group)) {
  #write.csv(end_table %>% filter(group == i),
  #          paste0('data/input/end_table_', i, '.csv'),
  #          row.names = FALSE)
  system(command = paste0('Rscript batchrun.R ', i), wait = FALSE)
}

nsf_files <- nsf_files[sample(nrow(nsf_files)), ]

for(i in (i-1):nrow(nsf_files)) {
  parse_award(read_award(nsf_files[i,1]))
}

# The failed nodes:
failed <- unlist(build_nodes)
for(i in 1:length(failed)) {
  cat(i)
  test <- read_award(failed[i]) %>% parse_award
  
}

input <- read_award(failed[1])
input
