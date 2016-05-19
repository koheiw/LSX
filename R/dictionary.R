#' Make a LSS dicitonary with seed words
#' @export
makeDictionary <- function(mx, words, seeds, valuetype='fixed'){
  if(valuetype=='glob'){
    seeds <- get_fixed_seeds(seeds, rownames(mx))
  }
  mx_sim <- similarity(mx, words, names(seeds))
  #print(dim(mx_sim))
  mx_wsum <- rowsum_weighted(mx_sim, names(seeds), seeds)
  mx_wsum <- mx_wsum[rownames(mx_wsum) %in% words,,drop=FALSE]
  df_dic <- as.data.frame.matrix(mx_wsum)
  colnames(df_dic) <- 'score'
  df_dic <- df_dic[order(-df_dic$score),,drop=FALSE]
  return(df_dic)
}

#' Read LSS dicitonary in text format
#' @export
readDictionary <-function(file){
  if(file.exists(file)){
    df <- read.csv(file=file,  header=FALSE, sep='\t', col.names=c('word', 'score'))
    dupli <- duplicated(df$word)
    if(sum(dupli) > 0){
      warning('Dupicated entiry word in dicitonary:\n', paste(df_dic$word[dupli], '\n'))
    }
    df2 <- subset(df, dupli==FALSE)
    df3 <- data.frame(score=df2$score, row.names=df2$word)
    return(df3)
  }else{
    stop(paste('Dictionary file is not found:', file))
  }
}

#' Perform singular value decomposition by irlba
#' @export
decompose <- function(mx, nv=300, cache=TRUE, ...){
  file_cache <- paste0('lss_svd_', digest::digest(mx, algo='xxhash64'), '.RDS')
  if(cache & file.exists(file_cache)){
    cat('Reading cache file:', file_cache, '\n')
    mx2 <- readRDS(file_cache)
  }else{
    cat('Starting singular value decompositiono of dfm...\n')
    set.seed(1) # Important for replicable results
    S <- irlba::irlba(mx, nv=300, center=Matrix::colMeans(mx), verbose=TRUE, right_only=TRUE, ...)
    mx2 <- S$v * S$d
    rownames(mx2) <- colnames(mx)
    if(cache){
      cat('Writing cache file:', file_cache, '\n')
      saveRDS(mx2, file_cache)
    }
  }
  return(mx2)
}

#' Calculate document score by LSS dictionary
#' @export
scoreDocument <- function(...){
  calc_scores(...)
}

#' Internal function to calculate document score
calc_scores <- function(mx, df_dic, se=TRUE){

  mx <- Matrix(mx, sparse=FALSE)
  common <- intersect(rownames(df_dic), colnames(mx))
  mx <- mx[,match(common, colnames(mx))] # Ignore words not in the dictionary
  mx_dic <- as.matrix(df_dic[match(common,rownames(df_dic)),,drop=TRUE], row=1)

  # Based on Wordscore
  mx_tf <- sweep(mx, 1, rowSums(mx), FUN="/")
  mns <- as.matrix(mx_tf) %*% mx_dic # Mean scores of documents

  if(se){
    mns <- as.matrix(mx_tf) %*% mx_dic # Mean scores of documents
    mx_binary <- mx > 0
    mx_score <- mx_binary * t(mx_dic[,rep(1,nrow(mx_tf))])
    mx_dev <- mx_score - mns[,rep(1,ncol(mx_binary))] # Difference from the mean
    mx_error <- (mx_dev ** 2) * mx_tf # Square of deviation weighted by frequency
    vars <- rowSums(mx_error) # Variances
    sds <- sqrt(vars) # Standard diviaitons
    ses <- sds / sqrt(rowSums(mx)) # SD divided by sqrt of total number of words
    return(list('lss_mn'=mns[,1], 'lss_se'=ses))
  }else{
    return(list('lss_mn'=mns[,1]))
  }
}

#' Internal function to convert seed words from glob to fiexd format
get_fixed_seeds <- function(seeds, types){
  seeds_score <- c()
  for(i in 1:length(seeds)){
    seed <- seeds[i]
    #print(seeds)
    #print(types)
    seed_regex <- utils::glob2rx(names(seed))
    seed_fixed <- regex2fixed(seed_regex, types)
    if(length(seed_fixed)){
      len <- length(seed_fixed)
      seed_score <- rep(seed / len, len)
      names(seed_score) <- seed_fixed
      seeds_score <- c(seeds_score, seed_score)
    }
  }
  return(seeds_score)
}
