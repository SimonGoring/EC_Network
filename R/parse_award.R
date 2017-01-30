
parse_award <- function(input) {
  
  ec_graph <- startGraph("http://localhost:7474/db/data", username = "neo4j", password = "c@mpf1re")
  
  # The award number is going to be the key identifier for all relationships
  # and for the award itself:
  award_no <- unlist(input$Award$AwardID)
  
  # It looks like Organization only ever has one unit:
  
  # We're going to bail on Fellowships, which are assigned to only a single person:
  if('Fellowship' %in% unlist(input$Award$AwardInstrument)) {
    return(data.frame(award = award_no, success = 0, message = 'Fellowship'))
  }
  
  org_parse <- function(x) {
    
    if('Organization' %in% names(x)) {
      x <- x$Organization
    }
    
    org <- list(code        = unlist(x$Code),
                directorate = unlist(x$Directorate),
                division    = unlist(x$Division))
    
    mergeNode(org, graph = ec_graph, type = "organization")
  }
  
  in_orgs <- input$Award[names(input$Award) == 'Organization']
  
  # I used ifelse, but it wasn't returning the right object:
  if (length(in_orgs) == 1 ) {
    org_node <- org_parse(in_orgs)
  } else if (length(in_orgs) > 1) {
    org_node <- lapply(in_orgs, org_parse)
  } else {
    org_node <- NA
  }
  
  # Program ###
  
  prog_parse <- function(x) {
    
    if('ProgramElement' %in% names(x) | 'ProgramReference' %in% names(x)) {
      x <- x[[1]]
    }
    
    if (!(length(x$Code) == 0 & length(x$Text) == 0)) {
      
      prog <- list(code     = ifelse(length(x$Code) > 0, unlist(x$Code), unlist(x$Text)),
                   program  = unlist(x$Text))
      
      return(mergeNode(prog, graph = ec_graph, type = "program"))
    }
  }
  
  in_prog <- input$Award[names(input$Award) %in% c('ProgramReference', 'ProgramElement')]
  
  if (length(in_prog) == 1) {
    prog_node <- prog_parse(in_prog)
  } else if (length(in_prog) > 1) {
    prog_node <- lapply(in_prog, prog_parse)
  } else {
    prog_node <- NA
  }
  
  # Now, combine the Program and Organization elements
  if (!all(NA %in% prog_node)) {
    na <-   mergeRel(x     = list(type = 'program', object = prog_node),
                     y     = list(type = 'organization', object = org_node),
                     type  = list(type = 'program_of', data = list(award = award_no)),
                     graph = ec_graph)
  } 
  
  # Award :
  if (!length(input$Award$AwardID) == 0) {
    
    if (is.null(unlist(input$Award$AwardID))) {
      # We can't use NULL IDs, so we pass the title. . .
      input$Award$AwardID <- gsub(' ', '', unlist(input$Award$AwardTitle))
    }
    
    award <- list(id             = unlist(input$Award$AwardID),
                  title          = unlist(input$Award$AwardTitle),
                  amount         = unlist(input$Award$AwardAmount),
                  effectiveDate  = unlist(input$Award$AwardEffectiveDate),
                  expirationDate = unlist(input$Award$AwardExpirationDate),
                  instrument     = unlist(input$Award$AwardInstrument),
                  abstract       = unlist(input$Award$AbstractNarration))
    
    awd_node <- mergeNode(award, graph = ec_graph, type = "award")
    
    if (!all(NA %in% prog_node)) {
      mergeRel(x     = list(type = 'program', object = prog_node),
               y     = list(type = 'award', object = awd_node),
               type  = list(type = 'funded_by', data = list(award = award_no)),
               graph = ec_graph)
    } else if (!all(NA %in% org_node)) {
      na <-   mergeRel(x     = list(type = 'organization', object = org_node),
                       y     = list(type = 'award', object = awd_node),
                       type  = list(type = 'funded_by', data = list(award = award_no)),
                       graph = ec_graph)
    }  
  }
  
  #  Person
  
  in_pers <- input$Award[names(input$Award) %in% 'Investigator']
  
  pers_parse <- function(x) {
    
    if ('Investigator' %in% names(x)) {
      x <- x$Investigator
    }
    
    if (length(x$FirstName) == 0 & length(x$LastName) == 0 | "DATA NOT AVAILABLE" %in% x$LastName) {
      return(NULL)
    }
    
    pers <- list(id           = paste0(unlist(x$FirstName),unlist(x$LastName)),
                 firstname    = unlist(x$FirstName),
                 lastname     = unlist(x$LastName),
                 email        = unlist(x$EmailAddress))
    
    mergeNode(pers, graph = ec_graph, type = "person")
    
  }
  
  if (length(in_pers) == 1 ) {
    pers_node <- pers_parse(in_pers)
  } else if (length(in_pers) > 1) {
    pers_node <- lapply(in_pers, pers_parse)
  } else {
    pers_node <- NA
  }
  
  if(!(is.null(pers_node))) {
    if(! all(is.na(unlist(pers_node, recursive = TRUE)))) {
      # This means that awards with no listed personnel get assigned to the
      # institutution, but not to a person.  So they're still in the graph.
      mergeRel(x     = list(type = 'person', object = pers_node),
               y     = list(type = 'award', object = awd_node),
               type  = list(type = 'awarded_to', data = list(award = award_no)),
               graph = ec_graph)
    }
  }  
  
  # Institution
  
  in_inst <- input$Award[names(input$Award) %in% 'Institution']
  
  inst_parse <- function(x) {
    
    x <- x$Institution
    
    inst <- list(id        = paste0(unlist(x$Name),unlist(x$ZipCode)),
                 name      = unlist(x$Name),
                 address   = unlist(x$StreetAddress),
                 city      = unlist(x$CityName),
                 state     = unlist(x$StateName),
                 statecode = unlist(x$StateCode),
                 country   = unlist(x$CountryName),
                 phone     = unlist(x$PhoneNumber))
    
    mergeNode(inst, graph = ec_graph, type = "institution")
    
  }
  
  if (length(in_inst) == 1) {
    inst_node <- inst_parse(in_inst)
  } else if (length(in_inst) > 1) {
    inst_node <- lapply(in_inst, inst_parse)
  } else {
    inst_node <- NA
  }
  
  if(!(is.null(pers_node))) {
    if(! all(is.na(unlist(pers_node, recursive = TRUE)))) {
      mergeRel(x = list(type     = 'person', object = pers_node),
               y     = list(type = 'institution', object = inst_node),
               type  = list(type = 'employed_by', data = list(award = award_no)),
               graph = ec_graph)
    }
  }
  
  mergeRel(x = list(type     = 'award', object = awd_node),
           y     = list(type = 'institution', object = inst_node),
           type  = list(type = 'administered_by', data = list(award = award_no)),
           graph = ec_graph)
  
  return(data.frame(award = award_no, success = 0, message = "You're the best!"))
  
}
