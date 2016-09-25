#' @useDynLib LSS
flag_collocates <- function(tokens, targets, window, len, negative=FALSE){

  flag <- flag_collocates_cppl(tokens, targets, window, len)
  if(negative){
    cols <- flag$col | flag$target
  }else{
    cols <- flag$col & !flag$target
  }
  return(cols)
}


count_collocates <- function(tokens, target, target_negative, window=10, valuetype='fixed', flag_target=FALSE){
  tokens_unlist <- unlist(tokens, use.names = FALSE)
  len <- length(tokens_unlist)
  if(valuetype == 'glob'){
    target <- utils::glob2rx(target)
    if(!missing(target_negative)) target_negative <- utils::glob2rx(target_negative)
    valuetype <- 'regex'
  }
  if(valuetype == 'regex'){
    types <- unique(tokens_unlist)
    target <- regex2fixed(target, types)
    #print(target)
    if(!missing(target_negative)) target_negative <- regex2fixed(target_negative, types)
    #print(target_negative)
  }
  cols <- flag_collocates(tokens, target, window, len, flag_target)
  if(!missing(target_negative)){
    cols_negative <- flag_collocates(tokens, target_negative, window, len, TRUE)
    cols <- cols & !cols_negative
  }
  cat("Counting collocations...\n")
  tb <- table(tokens_unlist, factor(cols, levels=c(TRUE, FALSE)))

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

#' @export
selectEntrywords <- function(...){
  findCollocates(...)
}

#' Find significant collocations of targets
#' @examples
#' docs <- readLines('/home/kohei/projects/immigration/data/uk_img/2009-2010.txt')
#' sents <- quanteda::tokenize(docs, what='sentence', simplify = TRUE)
#' tokens <- quanteda::tokenize(sents, removePunct=TRUE, removeNumbers=TRUE)
#' entries <- selectEntrywords(tokens, '^(immigra|migra)', valuetype='regex')
#'
#' @export
findCollocates <- function(tokens, target, target_negative, window=10, count_min=5,
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
  g <- stats::qchisq(1 - p, 1) # chisq appariximation to g-score
  df <- df[df$inside >= count_min,] # Exclude rare words
  df$gscore <- apply(df, 1, function(x, y, z) gscore(x[1], x[2], y, z), sum_inside, sum_outside)
  df <- df[order(-df$gscore),]
  df <- df[df$gscore > g,]
  if(word_only){
    return(rownames(df))
  }else{
    df$p <- 1 - stats::pchisq(df$g, 1)
    return(df)
  }
}

#' Internal function to calcualte g-score (Chi-square at the moment)
gscore <- function(col, non, sum_col, sum_non, smooth=1){
  tb <- as.table(rbind(c(col, non), c(sum_col - col, sum_non - non)))
  tb <- tb + smooth
  suppressWarnings(
    chi <- stats::chisq.test(tb)
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

#' Internal function to calcualte PMI
pmi <- function(col, non, sum_col, sum_non, power=1){
  tb <- as.table(rbind(c(col, non), c(sum_col - col, sum_non - non)))
  pmi <- log2((sum(tb) * (tb[1,1] ^ power)) / ((tb[1,1] + tb[1,2]) * (tb[1,1] + tb[2,1])))
  return(pmi)
}
