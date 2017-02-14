#' Merge nodes using `MERGE` rather than CREATE.
#' 
#' @param x A list of node elements
#' @param type The node type.
#' @param graph The neo4j graph.
#' 
#' @author Simon Goring

mergeNode <- function(x, type, graph) {
  
  query = "MERGE (n"
  
  if (length(type) > 0) {
    for (i in 1:length(type)) {
      query = paste0(query, ":", type[i])
    }
  }
  
  if (all(sapply(x, length) == 0)) { return(NULL) }
  
  query <- ifelse(length(x) > 0 & !(":''" %in% collapse_list(x)), 
                  paste0(query, " {", collapse_list(x), "}) "), 
                  query)
    
  x$createTime <- gsub(':', '/:', Sys.time())
  
  query <- paste0(query, "ON CREATE SET n =  {", collapse_list(x), "} RETURN n")
  node  <- cypherToList(graph, query)[[1]]$n
  
  return(node)
}

