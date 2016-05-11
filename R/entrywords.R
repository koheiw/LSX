
flag_collocates <- function(tokens, targets, window=10){

  flag_target <- tokens %in% targets
  len <- length(tokens)
  cols <- rep(FALSE, len)
  if(sum(flag_target) > 0){
    #print(pos)
    index_target <- which(flag_target)
    index_col <- get_colindex(index_target, window, len)
    cols[index_col] <- TRUE
    cols[index_target] <- FALSE # Exclude targets
  }
  names(cols) <- tokens
  return(cols)
}

get_colindex <- function(index, window, len){
  index_col <- unique(unlist(lapply(index, FUN=function(x) x + seq(window * -1, window))))
  index_col <- index_col[1 <= index_col & index_col <= max(len)]
  return(index_col)
}

count_collocates <- function(units, target){
  tokens_unique <- unique(unlist(units))
  tokens_target <- tokens_unique[stri_detect_regex(tokens_unique, target)]
  #print(tokens_target)
  cols <- unlist(lapply(units, function(x, y) flag_collocates(x, y), tokens_target))
  return(as.matrix(table(names(cols), cols)))
}

#' @export
selectEntrywords <- function(tokens, target, min=5){
  mx_col <- count_collocates(tokens, target)
  mx_col <- mx_col[,c(2,1)]
  sum_col <- sum(mx_col[,1])
  sum_non <- sum(mx_col[,2])
  mx_col <- mx_col[mx_col[,1] >= min,] # Exclude infrequent words
  df_col <- as.data.frame.matrix(mx_col)

  df_col$chisq <- apply(mx_col, 1, function(x, y, z) gscore(x[1], x[2], y, z), sum_col, sum_non)
  df_col <- df_col[df_col$chisq > 10.84,]
  df_col <- df_col[order(-df_col$chisq),]
  df_col <- df_col[rownames(df_col)!='',]
  return(df_col)
}
