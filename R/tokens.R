
#' @examples
#' docs <- readLines('/home/kohei/projects/immigration/data/uk_img/2009-2010.txt')
#' sents <- tokenize(docs, what='sentence', simplify = TRUE)
#' tokens <- tokenize(sents, removePunct=TRUE, removeNumbers=TRUE)
#' tokens2 <- removeTokens(tokens, c('are', 'is', 'be'), spacer=TRUE)
#'
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
