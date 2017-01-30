
collapse_list <- function(x){
  
  # Escape characters:
  # Remove double escapes within the document.
  x <- lapply(x, function(x) gsub('\\', '', x, fixed = TRUE))
  x <- lapply(x, function(x) gsub('(\'|\")', '\\\\\\1', x))
  
  if('award' %in% names(x)) {
    out <- paste0(names(x), ":['", unlist(x), "']", collapse = ', ')
  } else {
    out <- paste0(names(x), ":'", unlist(x), "'", collapse = ', ')
  }
  
  return(out)
  
}
