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
  sample(500) %>% 
  map(read_award) %>% 
  map(function(x)length(x$Award$Investigator$FirstName)) %>% 
  unlist %>% 
  table

award_parse <- nsf_files %>% 
  sample(500) %>% 
  map(read_award)

library(RNeo4j)

ec_graph <- startGraph("http://localhost:7474/db/data", username = "neo4j", password = "c@mpf1re")

collapse_list <- function(x){
  
  # Escape characters:
  # Remove double escapes within the document.
  x <- lapply(x, function(x) gsub('\\', '', x, fixed = TRUE))
  x <- lapply(x, function(x) gsub('(\'|\")', '\\\\\\1', x))
            
  paste0(names(x), ":'", unlist(x), "'", collapse = ', ')
}

mergeNode <- function(x, type, graph) {

  #' Merge nodes using `MERGE` rather than CREATE.
  #' 
  #' @param x A list of node elements
  #' @param type The node type.
  #' @param graph The neo4j graph.
  #' 
  #' @author Simon Goring
  
  query = "MERGE (n"
  
  if (length(type) > 0) {
    for (i in 1:length(type)) {
      query = paste0(query, ":", type[i])
    }
  }
  
  if (all(sapply(x, length) == 0)) { return(NULL) }
  
  query <- ifelse(length(x) > 0, paste0(query, " {", collapse_list(x), "}) "), 
                 query)
  
  query <- paste0(query, "ON CREATE SET n =  {", collapse_list(x), "} RETURN n")
  node  <- cypherToList(graph, query)[[1]]$n
  
  return(node)
}

mergeRel <- function(x, y, type, graph) {
  
  #' Generate and merge new relationships between nodes.
  #' @param x A list with two elements, \code{type} and \code{object}, a \code{node} object.
  #' @param as A list with two elements, \code{type} and \code{object}, a \code{node} object.  
  #' @param type A list with two elements, the new relationship \code{type} and \code{data}, any information to be passed in.

  # Match nodes:
  render_obj <- function(x, type, prepend) {
    
    if ('list' %in% class(x)) {
      
      x <- x[!sapply(x, is.null)]
      
      out <- lapply(1:length(x), function(index) {
        paste0('(', prepend, index, ':',type,' {', collapse_list(x[[index]]), '})') })
      elements <- paste0(prepend, 1:length(x))
    } else {
      out <- paste0('(', prepend, '1:', type, ' {', collapse_list(x), '})')
      elements <- paste0(prepend, '1')
    }
    return(list(string = out, elements = elements))
  }

  x_objs <- render_obj(x$object, x$type, 'x')
  y_objs <- render_obj(y$object, y$type, 'y')
  
  
  
  matches <- paste0(paste(x_objs$string, collapse = ', '), ', ',
                    paste(y_objs$string, collapse = ', '))
  
  relations <- expand.grid(to = paste0('(', x_objs$elements, ')'), 
                           from = paste0('(', y_objs$elements, ')'))
  
  if (length(type$data) == 0) {
    relation_string <- paste0(relations$to, '-[r:', type$type, ']-', relations$from)
  } else {
    rs <- paste0('r', 1:nrow(relations))
    
    relation_string <- paste(paste0(relations$to, '-[', rs, ':', type$type, 
                              ' {', collapse_list(type$data), '}]-', relations$from), 
                             collapse = ' MERGE ')
  }
    
  query <-  paste0('MATCH ', matches, 
                   ' MERGE ', relation_string)
    
  new_rel <- cypherToList(graph, query)
  
  return(new_rel)
  
}

parse_award <- function(input) {
  
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
  
  if(!(is.null(in_pers))) {
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
  
  if(!(is.null(in_pers))) {
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

build_nodes <- nsf_files %>% sample(500) %>% map(function(x) {test <- try(parse_award(read_award(x))); ifelse('try-error' %in% class(test), x, test)})

# The failed nodes:
failed <- unlist(build_nodes)
for(i in 1:length(failed)) {
  test <- read_award(failed[i]) %>% parse_award
}

input <- read_award(failed[3])
input
