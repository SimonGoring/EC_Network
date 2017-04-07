args <- commandArgs(trailingOnly = TRUE)

sink(file = paste0('data/output/logger_', args[1], '.log'))

library(dplyr)
library(purrr)
library(multidplyr)
library(RNeo4j)
library(lubridate)

nsf_files <- read.csv(paste0('data/input/end_table_', args[1], '.csv'),
                      stringsAsFactors = FALSE) %>% 
  mutate(year = ymd(year)) %>% 
  filter(group == args[1])

read_award <- function(file) {
  input <- xml2::read_xml(file)
  input <- xml2::as_list(input)
  
  input
}

source('R/mergeNode.R')
source('R/mergeRel.R')
source('R/collapse_list.R')
source('R/parse_award.R')

for(i in 1:nrow(nsf_files)) {
  cat(nsf_files$file[i], '\n')
  
  if(is.na(nsf_files$parsed[i]) & (!is.na(nsf_files$year[i]) & nsf_files$year[i] > as_date("1999-12-31"))) {
    runner <- try(parse_award(read_award(nsf_files$file[i])))
    
    sys_start <- proc.time()
    
    while(class(runner) == 'try-error' & (proc.time() - sys_start)[3] < 10) {
      runner <- try(parse_award(read_award(nsf_files$files[i])))
      
      if(class(runner) == 'try-error') { cat(runner) }
      
    }
    
    if(class(runner) == 'try-error') { 
      nsf_files$parsed <- NA
    } else {
      nsf_files$parsed[i] <- 1
    }
    
  }
  
  if(i %% 20 == 0) {
    write.csv(nsf_files, 
              paste0('data/input/end_table_', args[1], '.csv'),
              row.names = FALSE)
  }

}

sink(file = NULL)