
flag_collocates <- function(tokens, targets, window, len, negative=FALSE){

  flag <- flag_collocates_cppl(tokens, targets, window, len)
  if(negative){
    cols <- flag$col | flag$target
  }else{
    cols <- flag$col & !flag$target
  }
  return(cols)
}

#' @examples
#' count_collocates2(list(LETTERS, letters), 'C|J|o|w', window=2)
#' count_collocates2(list(LETTERS, letters), 'C|J|o|w', 'A|z', window=2)
#'
#' microbenchmark::microbenchmark(
#' count_collocates2(list(LETTERS, letters), 'C|J|o|w', window=2),
#' count_collocates(list(LETTERS, letters), 'C|J|o|w', window=2))
count_collocates <- function(tokens, target, target_negative, window=10){
  tokens_unlist <- unlist(tokens, use.names = FALSE)
  len <- length(tokens_unlist)
  types <- unique(tokens_unlist)
  targets <- regex2fixed(target, types)
  cols <- flag_collocates(tokens, targets, window, len)

  if(!missing(target_negative)){
    targets_negative <- regex2fixed(target_negative, types)
    cols_negative <- flag_collocates(tokens, targets_negative, window, len)
    cols <- cols & !cols_negative
  }
  cat("Counting collocations...\n")
  mx <- as.matrix(table(tokens_unlist, factor(cols, levels=c(TRUE, FALSE))))
  #mx <- mx[!rownames(mx) %in% targets,] # Exclude target words
  return(mx)
}

regex2fixed <- function(regex, types){
  types_match <- types[stringi::stri_detect_regex(types, paste0(regex, collapse='|'))]
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
selectEntrywords <- function(tokens, target, target_negative, count_min=5,
                             word_only=TRUE, g=10.84, ...){

  cat("Finding collocations...\n")
  if(missing(target_negative)){
    mx <- count_collocates(tokens, target, ...)
  }else{
    mx <- count_collocates(tokens, target, target_negative, ...)
  }
  sum_true <- sum(mx[,1])
  sum_false <- sum(mx[,2])
  if(sum(mx[,1])==0) warning("No words within collocation windows\n")
  mx <- mx[mx[,1] >= count_min,] # Exclude rare words
  df <- as.data.frame.matrix(mx)
  cat("Calculating g-score...\n")
  df$gscore <- apply(mx, 1, function(x, y, z) gscore(x[1], x[2], y, z), sum_true, sum_false)

  df <- df[df$gscore > g,]
  df <- df[order(-df$gscore),]
  df <- df[rownames(df)!='',]
  if(word_only){
    return(rownames(df))
  }else{
    return(df)
  }
}

#' Internal function to calcualte g-score
gscore <- function(col, non, sum_col, sum_non, smooth=1){
  tb <- as.table(rbind(c(col, non), c(sum_col - col, sum_non - non)))
  tb <- tb + smooth
  suppressWarnings(
    chi <- chisq.test(tb)
  )
  #print(tb)
  #print(chi$expected)
  col_exp <- chi$expected[1,1]
  if(col > col_exp){
    return(unname(chi$statistic))
  }else{
    return(unname(chi$statistic) * -1)
  }
}
