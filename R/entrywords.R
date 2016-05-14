
flag_collocates <- function(tokens, targets, window=10){

  flag_target <- tokens %in% targets
  len <- length(tokens)
  cols <- rep(FALSE, len)
  if(sum(flag_target) > 0){
    index_target <- which(flag_target)
    index_col <- flag_window_cpp(index_target, window, len, FALSE)
    cols[index_col] <- TRUE
  }
  names(cols) <- tokens
  return(cols)
}

# Old version (not used)
get_colindex <- function(index, window, len){
  index_col <- unique(unlist(lapply(index, FUN=function(x) x + seq(window * -1, window)), use.names = FALSE))
  index_col <- index_col[1 <= index_col & index_col <= max(len)]
  return(index_col)
}

count_collocates <- function(units, target){
  tokens_unique <- unique(unlist(units, use.names = FALSE))
  tokens_target <- tokens_unique[stringi::stri_detect_regex(tokens_unique, target)]
  #print(tokens_target)
  cols <- unlist(lapply(units, function(x, y) flag_collocates(x, y), tokens_target)) # Name is needed
  return(as.matrix(table(names(cols), cols)))
}


#' @examples
#' docs <- readLines('/home/kohei/projects/immigration/data/uk_img/2009-2010.txt')
#' sents <- tokenize(docs, what='sentence', simplify = TRUE)
#' tokens <- tokenize(sents, removePunct=TRUE, removeNumbers=TRUE)
#' entries <- selectEntrywords(tokens, '^(immigra|migra)')
#'
#'
#' @export
selectEntrywords <- function(units, target, min=5){

  cat("Finding collocations...\n")
  mx_col <- count_collocates(units, target)
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
