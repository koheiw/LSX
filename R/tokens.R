#' @export
removeTokens <- function(tokens, types_remove, spacer=FALSE){
  tokens2 <- select_tokens_cppl(tokens, types_remove, TRUE, spacer)
  attributes(tokens2) <- attributes(tokens)
  return(tokens2)
}

#' @export
removeShortTokens <- function(tokens, min=3, spacer=FALSE){
  types <- unique(unlist(tokens, use.names = FALSE))
  types_remove <- types[stringi::stri_length(types) < min]
  return(removeTokens(tokens, types_remove, spacer))
}

#' @export
removeSpacers <- function(tokens){
  return(removeTokens(tokens, '', FALSE))
}

#' @export
tokenize <- function(...){
  quanteda::tokenize(...)
}
