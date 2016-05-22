
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
  cols <- flag_collocates(tokens, targets, window, len, FALSE)
  if(!missing(target_negative)){
    targets_negative <- regex2fixed(target_negative, types)
    cols_negative <- flag_collocates(tokens, targets_negative, window, len, TRUE)
    cols <- cols & !cols_negative
  }
  cat("Counting collocations...\n")
  tb <- table(tokens_unlist, factor(cols, levels=c(TRUE, FALSE)))
  #mx <- mx[!rownames(mx) %in% targets,] # Exclude target words
  return(tb)
}


regex2fixed <- function(regex, types, case_insensitive=TRUE, ...){
  regex_joined <- paste0(regex, collapse = "|")
  types_match <- types[stringi::stri_detect_regex(types, regex_joined, case_insensitive = case_insensitive, ...)]
  return(types_match)
}


# regex2fixed <- function(regex, types){
#
#   flag <- stringi::stri_startswith_fixed(regex, '^') & stringi::stri_endswith_fixed(regex, '$') # detect fixed patterns
#   types_fixed <- regex[flag]
#   regex <- regex[!flag] # only non-fixed patterns
#   types_match <- types[types %in% types_fixed | stringi::stri_detect_regex(types, paste0(regex, collapse='|'))]
#   return(types_match)
# }


#' @examples
#' docs <- readLines('/home/kohei/projects/immigration/data/uk_img/2009-2010.txt')
#' sents <- tokenize(docs, what='sentence', simplify = TRUE)
#' tokens <- tokenize(sents, removePunct=TRUE, removeNumbers=TRUE)
#' entries <- selectEntrywords(tokens, '^(immigra|migra)')
#'
#'
#' @export
selectEntrywords <- function(tokens, target, target_negative, window=10, count_min=5,
                             word_only=TRUE, p=0.001, ...){

  cat("Finding collocations...\n")
  if(missing(target_negative)){
    tb <- count_collocates(tokens, target, window=window, ...)
  }else{
    tb <- count_collocates(tokens, target, target_negative, window=window, ...)
  }
  df <- as.data.frame.matrix(tb)
  colnames(df) <- c('inside', 'outside')

  sum_inside <- sum(df$inside)
  sum_outside <- sum(df$outside)
  df <- df[rownames(df)!='',]
  if(sum_inside == 0) stop("No words within collocation windows\n")
  if(missing(count_min)) count_min <- sum(df) / 10 ^ 6 # one in million

  cat("Calculating g-score...\n")
  g <- qchisq(1 - p, 1) # chisq appariximation to g-score
  df <- df[df$inside >= count_min,] # Exclude rare words
  df$gscore <- apply(df, 1, function(x, y, z) gscore(x[1], x[2], y, z), sum_inside, sum_outside)
  df <- df[order(-df$gscore),]
  df <- df[df$gscore > g,]
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
