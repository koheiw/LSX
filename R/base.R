library(Matrix)
library(digest)
library(irlba)
library(stringi)



gscore <- function(col, non, sum_col, sum_non){
  tb <- as.table(rbind(c(col, non), c(sum_col - col, sum_non - non)))
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

decompose <- function(mx, nv=300, cache=TRUE, ...){
  file_cache <- paste0('lss_svd_', digest::digest(mx, algo='xxhash64'), '.RDS')
  if(cache & file.exists(file_cache)){
    cat('Reading cache file:', file_cache, '\n')
    mx2 <- readRDS(file_cache)
  }else{
    cat('Starting SVD ...\n')
    set.seed(1) # Important for replicable results
    S <- irlba::irlba(mx, nv=300, center=colMeans(mx), verbose=TRUE, right_only=TRUE, ...)
    mx2 <- S$v * S$d
    rownames(mx2) <- colnames(mx)
    if(cache){
      cat('Writing cache file:', file_cache, '\n')
      saveRDS(mx2, file_cache)
    }
  }
  return(mx2)
}

similarity <- function(mx, words, seeds, cache=TRUE){

  if(missing(seeds)){
    file_cache <- paste0('lss_sim_', digest::digest(list(mx, words), algo='xxhash64'), '.RDS')
    if(cache & file.exists(file_cache)){
      cat('Reading cache file:', file_cache, '\n')
      mx_sim <- readRDS(file_cache)
    }else{
      cat("Creating similarity matrix ...\n")
      mx2 <- mx[rownames(mx) %in% words,]
      n <- nrow(mx2)
      mx_sim <- outer(1:n, 1:n, FUN = Vectorize(function(i,j) cosine(mx2[i,], mx2[j,])))
      colnames(mx_sim) <- rownames(mx_sim) <- rownames(mx2)
      if(cache){
        cat('Writing cache file:', file_cache, '\n')
        saveRDS(mx_sim, file_cache)
      }
    }
  }else{
    is <- which(rownames(mx) %in% words)
    js <- which(rownames(mx) %in% seeds)
    mx_sim <- outer(is, js, FUN = Vectorize(function(i,j) cosine(mx[i,], mx[j,])))
    colnames(mx_sim) <- rownames(mx[js,])
    rownames(mx_sim) <- rownames(mx[is,])
  }
  return(mx_sim)
}

cosine <- function(v1, v2){
  return(sum(v1*v2) / sqrt(sum(v1^2)*sum(v2^2)))
}

rowsum_weighted <- function(mx, words, weights){
  common <- intersect(colnames(mx), words)
  mx2 <- mx[,match(common, colnames(mx))]
  mx3 <- mx2 %*% matrix(weights[match(common, words)], ncol=1)
  rownames(mx3) <- rownames(mx)
  colnames(mx3) <- 'wsum'
  return(mx3)
}



