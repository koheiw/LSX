#' @examples
#' lapply(list(LETTERS, letters), flag_collocates, c('C', 'J', 'o' ,'w'), FALSE, 2)
flag_collocates <- function(tokens, targets, flag_targets=FALSE, window=10){
  len <- length(tokens)
  index <- which(tokens %in% targets)
  if(length(index) > 0){
    flag <- flag_window_cpp(index, window, len, flag_targets)
  }else{
    flag <- rep(FALSE, len)
  }
  names(flag) <- tokens
  return(flag)
}

#' @examples
#' count_collocates(list(LETTERS, letters), 'C|J|o|w', window=2)
#' count_collocates(list(LETTERS, letters), 'C|J|o|w', 'A|z', window=2)
count_collocates <- function(tokens, target, target_negative, window=10){
  types <- unique(unlist(tokens, use.names = FALSE))
  targets <- regex2fixed(target, types)
  cols <- unlist(lapply(tokens, flag_collocates, targets=targets, flag_targets=TRUE, window=window))
  if(!missing(target_negative)){
    # Exclude collocations of negative targets
    targets_negative <- regex2fixed(target_negative, types)
    cols_negative <- unlist(lapply(tokens, flag_collocates, targets_negative, FALSE, window))
    cols <- cols & !cols_negative
  }
  tb <- table(names(cols), factor(cols, levels=c(TRUE, FALSE)))
  mx <- as.matrix(tb)
  mx <- mx[!rownames(mx) %in% targets,] # Exclude target words
  return(mx)
}

regex2fixed <- function(regex, types){
  types_match <- types[stringi::stri_detect_regex(types, regex)]
  return(types_match)
}


#' @examples
#' docs <- readLines('/home/kohei/projects/immigration/data/uk_img/2009-2010.txt')
#' sents <- tokenize(docs, what='sentence', simplify = TRUE)
#' tokens <- tokenize(sents, removePunct=TRUE, removeNumbers=TRUE)
#' entries <- selectEntrywords(tokens, '^(immigra|migra)')
#'
#'
#' @export
selectEntrywords <- function(tokens, target, target_negative, count_min=5, ...){

  cat("Finding collocations...\n")
  if(missing(target_negative)){
    mx <- count_collocates(tokens, target, ...)
  }else{
    mx <- count_collocates(tokens, target, target_negative, ...)
  }
  if(sum(mx[,1])==0) stop("No words within windows\n")

  sum_true <- sum(mx[,1])
  sum_false <- sum(mx[,2])
  mx <- mx[mx[,1] >= count_min,] # Exclude rare words
  print(head(mx, 100))
  df <- as.data.frame.matrix(mx)
  cat("Calculating g-score...\n")
  df$gscore <- apply(mx, 1, function(x, y, z) gscore(x[1], x[2], y, z), sum_true, sum_false)


  df <- df[df$gscore > 10.84,]
  df <- df[order(-df$gscore),]
  df <- df[rownames(df)!='',]

  gscore <- df$gscore
  names(gscore) <- rownames(df)

  return(gscore)
}
