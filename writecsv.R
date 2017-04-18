
# x <- 'data/input/awards/0000541.xml'

bind_to_file <- function(x, award_file) {
  require(dplyr)
  
  is.NullOb <- function(x) (is.null(x) | length(x) == 0) | all(sapply(x, is.null))
  
  ## Recursively step down into list, removing all such objects 
  rmNullObs <- function(x) {
    # Replace missing elements with 'None', this is to work with Neo4j more effectively.
    if (is.NullOb(x)) {
      return('None')
    }
    out <- lapply(x, function(x) {
      if (is.list(x)) {
        z <- rmNullObs(x) 
      } else { z <- x }
      names(z) <- names(x) 
      return(unlist(z))})
    
    return(out)
  }
  
  
  file <- xml2::read_xml(x) %>% 
    xml2::as_list() %>% 
    magrittr::extract2('Award') %>% 
    rmNullObs %>% purrr::map(function(z)as.data.frame(rbind(z)))
  
  newRows <-list()
  
  for (i in 1:length(unique(names(file)))) {
    newRows[[i]] <- file[names(file) %in% unique(names(file))[i]] %>% data.table::rbindlist()
    if ("V1" %in% colnames(newRows[[i]])) {
      colnames(newRows[[i]])[which(colnames(newRows[[i]]) == "V1")] <- unique(names(file))[i]
    }
  }
  
  output <- do.call("cbind", newRows)

  tester <- data.frame(AwardTitle = NA,
                       AwardEffectiveDate = NA,
                       AwardExpirationDate = NA,
                       AwardAmount = NA,
                       Value = NA,
                       Code = NA,
                       Directorate.LongName = NA,
                       Division.LongName = NA,
                       SignBlockName = NA,
                       AbstractNarration = NA,
                       MinAmdLetterDate = NA,
                       MaxAmdLetterDate = NA,
                       ARRAAmount = NA,
                       AwardID = NA,
                       Name = NA,
                       CityName = NA,
                       ZipCode = NA,
                       PhoneNumber = NA,
                       StreetAddress = NA,
                       CountryName = NA,
                       StateName = NA,
                       StateCode = NA,
                       Code = NA,
                       Name = NA,
                       Code = NA,
                       Text = NA,
                       Code = NA,
                       Text = NA)
  
  full_output <- bind_rows(tester, output)
  
  if(any(colnames(full_output) %in% tester)) {
    warning(paste0('File ', x, ' had extra column names.'))
    full_output <- full_output[, colnames(tester)]
  }
  
  if(!award_file %in% list.files('data/output')){
    readr::write_csv(x = full_output[-1,], path = paste0('data/output/', award_file))
  } else {
    readr::write_csv(x = full_output[-1,], path = paste0('data/output/', award_file), append = TRUE)
  }
  
  return(NULL)
}

files <- list.files('data/input/awards/', full.names = TRUE)

z <- length(files)

for(i in 1:length(files)) {
  bind_to_file(files[i], 'award_file.csv')
  if ((i %% 200) == 0) {
    flush.console()
    cat("Run ", i, " of ", z, " files.")
  }
}
