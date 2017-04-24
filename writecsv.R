
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
      z <- unlist(z)
      names(z) <- names(x)
      return(z)})
    
    return(out)
  }
  
  clean_file <- try(xml2::read_xml(x) %>% 
    xml2::as_list() %>% 
    magrittr::extract2('Award'), silent = TRUE)
  
  if('try-error' %in% class(clean_file)) {return(NULL) }
  
  file <- clean_file %>% 
    rmNullObs %>% 
    purrr::map(function(z)as.data.frame(rbind(z), stringsAsFactors = FALSE))
    
  names(file) <- names(clean_file)
  
  
  newRows <-list()
  
  for (i in 1:length(names(file))) {
    newRows[[i]] <- file[[i]] %>% data.frame()
    colnames(newRows[[i]]) <- paste0(names(file)[i], '.', colnames(newRows[[i]]))
    
    if (length(grep("V1", colnames(newRows[[i]]))>0)) {
      colnames(newRows[[i]]) <- names(file)[i]
    }
  }
  
  # These column names are right:
  output <- newRows %>% 
    Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, all = TRUE), .) %>% 
    purrr::map(function(x){x[is.na(x)] <- x[!is.na(x)][1]; return(x)}) %>% 
    as.data.frame()
  
  tester <- data.frame(AwardTitle = NA,
                       AwardEffectiveDate = NA,
                       AwardExpirationDate = NA,
                       AwardAmount = NA,
                       AwardInstrument.Value = NA,
                       Organization.Code = NA,
                       Organization.Directorate = NA,
                       Organization.Division = NA,
                       ProgramOfficer.SignBlockName = NA,
                       AbstractNarration = NA,
                       MinAmdLetterDate = NA,
                       MaxAmdLetterDate = NA,
                       ARRAAmount = NA,
                       AwardID = NA,
                       Investigator.EmailAddress = NA,
                       Investigator.FirstName = NA,
                       Investigator.LastName = NA,
                       Investigator.StartDate = NA,
                       Investigator.EndDate = NA,
                       Investigator.RoleCode = NA,
                       Institution.Name = NA,
                       Institution.CityName = NA,
                       Institution.ZipCode = NA,
                       Institution.PhoneNumber = NA,
                       Institution.StreetAddress = NA,
                       Institution.CountryName = NA,
                       Institution.StateName = NA,
                       Institution.StateCode = NA,
                       ProgramElement.Code = NA,
                       ProgramElement.Text = NA,
                       ProgramReference.Code = NA,
                       ProgramReference.Text = NA,
                       FoaInformation.Name = NA,
                       FoaInformation.Code = NA,
                       FoaInformation.Text = NA, 
                       stringsAsFactors = FALSE)
  
  full_output <- bind_rows(tester, output)
  
  if(any(colnames(full_output) %in% tester)) {
    warning(paste0('File ', x, ' had extra column names.'))
    full_output <- full_output[, colnames(tester)]
  }

  full_output <- apply(full_output, 2, function(x) gsub('"', "'", x)) %>% 
    as.data.frame
  
  if(!award_file %in% list.files('data/output')){
    write.table(x = full_output[-1,], 
      file = paste0('data/output/', award_file), 
      quote = TRUE, sep = ",", row.names=FALSE)
  } else {

    write.table(x = full_output[-1,], 
      file = paste0('data/output/', award_file), 
      quote = TRUE, append = TRUE,
      sep = ",", col.names = FALSE, row.names=FALSE)
  }
  
  return(NULL)
}

files <- list.files('data/input/awards/', full.names = TRUE, pattern = '.xml')

file_length <- length(files)

start <- proc.time()

j <- 1

for(i in 1:file_length) {
  try(bind_to_file(files[i], paste0('award_file_', j,'.csv')))
  if ((i %% 200) == 0) {
    elapsed <- proc.time()[3] - start[3]
    tpf <-  elapsed / i

    end_time <- round((tpf * (file_length - i)) / 60, 0)

    message("Run ", i, " of ", file_length, " files.  ", round(tpf, 2), 
            "s per file. ",end_time,"m remaining. \r", appendLF=FALSE)
    flush.console()
  }
  if(i %% 1000) {
    j <- j + 1
  }
}
