
#' @export
removeShortFeatures <- function(tokens, min=3, ...){
  types <- unique(unlist(tokens, use.names = FALSE))
  types_short <- types[stringi::stri_length(types) < min]
  return(quanteda::selectFeatures2(tokens, types_short, selection='remove',
                                   valueType='fixed', case_insensitive=FALSE, ...))
}

#' @export
removePadding <- function(tokens){
  return(quanteda::selectFeatures2(tokens, '', padding=FALSE,
                                   valueType='fixed', case_insensitive=FALSE))
}

#' @export
tokenize <- function(...){
  quanteda::tokenize(...)
}
