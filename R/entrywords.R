
flag_collocates <- function(tokens, targets, targets_negative, window=10){

  len <- length(tokens)
  if(sum(flag_target) > 0){
    index_target <- which(tokens %in% targets)
    flag <- flag_window_cpp(index_target, window, len, FALSE)
  }
  if(!missing(targets_negative)){
    flag_target_negative <- tokens %in% targets_negative
    flag_negative <- flag_window_cpp(index_target, window, len, FALSE)
    flag <- flag & !flag_negative # exclude window from negative targets
  }
  names(flag) <- tokens
  return(flag)
}

# Old version (not used)
get_colindex <- function(index, window, len){
  index_col <- unique(unlist(lapply(index, FUN=function(x) x + seq(window * -1, window)), use.names = FALSE))
  index_col <- index_col[1 <= index_col & index_col <= max(len)]
  return(index_col)
}

count_collocates <- function(units, target, target_negative){
  types <- unique(unlist(units, use.names = FALSE))
  targets <- regex2fix(types, target)
  if(missing(target_negative)){
    cols <- unlist(lapply(units, function(x, y) flag_collocates(x, y), targets))
  }else{
    targets_negative <- regex2fix(types, target_negative)
    cols <- unlist(lapply(units, function(x, y, z) flag_collocates(x, y, z), targets, targets_negative))
  }
  return(as.matrix(table(names(cols), cols)))
}

regex2fix <- function(types, regex){
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
selectEntrywords <- function(units, target, target_negative, count_min=5, ...){

  cat("Finding collocations...\n")
  if(missing(target_negative)){
    mx_col <- count_collocates(units, target, ...)
  }else{
    mx_col <- count_collocates(units, target, target_negative, ...)
  }
  mx_col <- mx_col[,c(2,1)]
  sum_col <- sum(mx_col[,1])
  sum_non <- sum(mx_col[,2])
  mx_col <- mx_col[mx_col[,1] >= min,] # Exclude infrequent words
  df_col <- as.data.frame.matrix(mx_col)

  cat("Calculating g-score...\n")
  df_col$chisq <- apply(mx_col, 1, function(x, y, z) gscore(x[1], x[2], y, z), sum_col, sum_non)
  df_col <- df_col[df_col$chisq > 10.84,]
  df_col <- df_col[order(-df_col$chisq),]
  df_col <- df_col[rownames(df_col)!='',]
  return(df_col)
}
