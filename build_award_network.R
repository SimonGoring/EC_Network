library(dplyr)
library(purrr)

file <- 'data/input/awards/9496153.xml'

nsf_files      <- list.files('data/input/awards', full.names = TRUE)


read_award <- function(file) {
  input <- xml2::read_xml(file)
  input <- xml2::as_list(input)
  
  input
  
}

award_elements <- nsf_files %>% 
  sample(5000) %>% 
  map(read_award) %>% 
  map(function(x)names(x$Award)) %>% 
  unlist %>% 
  table

award_parse <- nsf_files %>% 
  sample(500) %>% 
  map(read_award)

library(RNeo4j)

ec_graph <- startGraph("http://localhost:7474/db/data", username = "neo4j", password = "c@mpf1re")

collapse_list <- function(x){
  paste0(names(x), ":'", unlist(x), "'", collapse = ', ')
}

mergeNode <- function(x, type, graph) {
  query = "MERGE (n"
  if (length(type) > 0) {
    for (i in 1:length(type)) {
      query = paste0(query, ":", type[i])
    }
  }
  
  props <- x
  
  query <- ifelse(length(props) > 0, paste0(query, " {", collapse_list(x), "}) "), 
                 query)
  
  query <- paste0(query, "ON CREATE SET n =  {", collapse_list(x), "} RETURN n")
  node  <- cypherToList(graph, query, props = x)[[1]]$n
  
  return(node)
}

mergeRel <- function(x, y, type, graph) {
  # x needs to be a two element vector, as does y.  
  # Type is the relationship type, and should be a list with two elements
  # the relationship type, and attributes for the relationship.

  # x and y should be lists of IDs:
  
  attrs <- type$data
  
  if(length(attrs) == 0) {
    
    query <-  paste0('MATCH (n:', x$type, ' {', collapse_list(x$object), 
                     '}), (n2:', y$type, '  {', collapse_list(y$object), 
                     '}) MERGE (n)-[r:', type$type, ']-(n2) RETURN (n)-[r]-(n2)')
    
    new_rel <- cypherToList(graph, query)
    
  } else {
    
    query <-  paste0('MATCH (n:', x$type, ' {', collapse_list(x$object), 
                     '}), (n2:', y$type, '  {', collapse_list(y$object), 
                     '}) MERGE (n)-[r:', type$type, ' {', 
                     collapse_list(attrs), '}]-(n2)  RETURN (n)-[r]-(n2)')
    
    new_rel <- cypherToList(graph, query)
    
  }
  
  return(new_rel)
  
}

parse_award <- function(input) {
  
  # The award number is going to be the key identifier for all relationships
  #  and for the award itself:
  award_no <- unlist(input$Award$AwardID)
  
  # It looks like Organization only ever has one unit:
  
  org_parse <- function(x) {
    
    x <- x$Organization
    
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
    
    if (!(length(x$Code) == 0 & length(x$Text) == 0)) {
      
      prog <- list(code     = ifelse(length(x$Code) > 0, unlist(x$Code), unlist(x$Text)),
                   program  = unlist(x$Text))
      
      return(mergeNode(prog, graph = ec_graph, type = "program"))
    }
  }
  
  in_prog <- input$Award[names(input$Award) %in% c('ProgramReference', 'ProgramElement')]
  
  if (length(in_prog) == 1 ) {
    prog_node <- prog_parse(in_orgs)
  } else if (length(in_prog) > 1) {
    prog_node <- lapply(in_prog, prog_parse)
  } else {
    prog_node <- NA
  }
  
  # Now, combine the Program and Organization elements
  if (!'list' %in% c(class(prog_node), class(org_node))) {
  
    mergeRel(x     = list(type = 'program', object = prog_node),
             y     = list(type = 'organization', object = org_node),
             type  = list(type = 'program_of', data = list(award = award_no)),
             graph = ec_graph)
    
  } else {
    if ('list' %in% class(prog_node)) {
      for (i in 1:length(prog_node)) {
        if ('list' %in% class(org_node)) {
          for (j in 1:length(org_node)) {
            mergeRel(x     = list(type = 'program', object = prog_node[[i]]),
                     y     = list(type = 'organization', object = org_node[[j]]),
                     type  = list(type = 'program_of', data = list(award = award_no)),
                     graph = ec_graph)
            
          }
        } else {
          mergeRel(x     = list(type = 'program', object = prog_node[[i]]),
                   y     = list(type = 'organization', object = org_node),
                   type  = list(type = 'program_of', data = list(award = award_no)),
                   graph = ec_graph)
        }
      }
    } else {
      for (j in 1:length(org_node)) {
        mergeRel(x     = list(type = 'program', object = prog_node),
                 y     = list(type = 'organization', object = org_node[[j]]),
                 type  = list(type = 'program_of', data = NULL),
                 graph = ec_graph)
        
      }
    }
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
  }
  
  mergeRels(x = )
  
  #  I need to do this pairwise and use the award number as an attribute in the links.
  #  Person
  
  in_pers <- input$Award[names(input$Award) %in% 'Investigator']
  
  pers_parse <- function(x) {
    
    pers <- list(id           = paste0(unlist(x$FirstName),unlist(x$LastName)),
                 firstname    = unlist(x$FirstName),
                 lastname     = unlist(x$LastName),
                 email        = unlist(x$EmailAddress))
    
    mergeNode(pers, graph = ec_graph, type = "person")
    
  }
  
  pers_node <- ifelse(length(in_pers)  > 1, 
                      lapply(in_pers, pers_parse),
                      ifelse(length(in_pers) == 1, pers_parse(in_pers$Investigator), NA))
  
  # Institution
  
  in_inst <- input$Award[names(input$Award) %in% 'Institution']
  
  inst_parse <- function(x) {
    
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
  
  inst_node <- ifelse(length(in_inst)  > 1, 
                      lapply(in_inst, inst_parse),
                      ifelse(length(in_inst) == 1, inst_parse(in_inst$Institution), NA))
  
  # Once those elements are all created, we need to bring it all together:
  
  # Relationships:
  #  People "WORKS AT" Institutions.  We want to modify the early & late dates for their employment at those insitutions based
  #  on award dates (going by the date awarded, rather than end dates).
  
  #  Awards are GRANTED THROUGH organizations.
  
  mergeRel(x = list(type = 'award', 
                    id = input$Award$AwardID),
           y = list(type = 'institution', 
                    id = paste0(unlist(input$Award$Institution$Name),unlist(input$Award$Institution$ZipCode))),
           type = list(type = 'GRANTED_TO', data = data.frame()),
           graph = ec_graph)
           
  
}

build_nodes <- nsf_files %>% sample(500) %>% map(function(x) {test <- try(parse_award(read_award(x))); ifelse('try-error' %in% class(test), x, test)})
