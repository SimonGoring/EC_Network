#' @title  Merge Relationships
#' Generate and merge new relationships between nodes.  Either append or create new relationships based on properties.
#' @param x A list with two elements, \code{type} and \code{object}, a \code{node} object.
#' @param as A list with two elements, \code{type} and \code{object}, a \code{node} object.  
#' @param type A list with two elements, the new relationship \code{type} and \code{data}, any information to be passed in.
#' @param match Should matched values be apended to the existing parameter?  Default \code{TRUE}.

mergeRel <- function(x, y, type, graph, match = TRUE) {

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
    relation_string <- paste0(relations$to, '<-[r:', type$type, ']-', relations$from)
    
  } else {
    rs <- paste0('r', 1:nrow(relations))
    
    relation_string <- paste(paste0(relations$to, '<-[', rs, ':', type$type, 
                                    ']-', relations$from), 
                             collapse = ' MERGE ')
    
    if (match == TRUE) {
      # We're going to add elements to the match:
      on_match <- paste0(' ON CREATE SET ', rs, '.', names(type$data), '= [\'',type$data, '\']',
                        '  ON MATCH SET ', rs, '.', names(type$data), '=', rs, '.', names(type$data), ' + \'', type$data, '\'')
    }
  }
  
  query <-  paste0('MATCH ', matches, 
                   ' MERGE ', relation_string)
  
  
  if (match == TRUE) {
    query <- paste0(query, paste(on_match, collapse = ' '))
  }
  
  new_rel <- cypherToList(graph, query)
  
  return(new_rel)
  
}