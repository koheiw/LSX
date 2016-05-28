library(Matrix)
library(digest)
library(irlba)
library(stringi)

similarity_old <- function(mx, words, seeds, cache=TRUE){

  if(missing(seeds)){
    file_cache <- paste0('lss_sim_', digest::digest(list(mx, words), algo='xxhash64'), '.RDS')
  }else{
    file_cache <- paste0('lss_sim_', digest::digest(list(mx, words, seeds), algo='xxhash64'), '.RDS')
  }
  if(cache & file.exists(file_cache)){
    cat('Reading cache file:', file_cache, '\n')
    mx_sim <- readRDS(file_cache)
  }else{
    cat("Creating similarity matrix...\n")
    if(missing(seeds)){
        mx2 <- mx[rownames(mx) %in% words,]
        n <- nrow(mx2)
        mx_sim <- outer(1:n, 1:n, FUN = Vectorize(function(i,j) cosine(mx2[i,], mx2[j,])))
        colnames(mx_sim) <- rownames(mx_sim) <- rownames(mx2)
    }else{
      is <- which(rownames(mx) %in% words)
      js <- which(rownames(mx) %in% seeds)
      mx_sim <- outer(is, js, FUN = Vectorize(function(i,j) cosine(mx[i,], mx[j,])))
      colnames(mx_sim) <- rownames(mx[js,])
      rownames(mx_sim) <- rownames(mx[is,])
    }
    if(cache){
      cat('Writing cache file:', file_cache, '\n')
      saveRDS(mx_sim, file_cache)
    }
  }
  return(mx_sim)
}

similarity <- function(mx, words, seeds){

  cat("Creating similarity matrix...\n")
  if(missing(seeds)){
    colls <- words
  }else{
    seeds <- seeds[seeds %in% rownames(mx)]
    colls <- unique(c(words, seeds))
  }
  mx_sub <- mx[rownames(mx) %in% colls,]
  mx_tmp <- cosine_pairwise(t(mx_sub))
  if(missing(seeds)){
    mx_sim <- mx_tmp[words, words]
    colnames(mx_sim) <- rownames(mx_sim) <- words
  }else{
    mx_sim <- mx_tmp[words, seeds]
    colnames(mx_sim) <- seeds
    rownames(mx_sim) <- words
  }

  return(mx_sim)
}


cosine <- function(v1, v2){
  return(sum(v1*v2) / sqrt(sum(v1^2)*sum(v2^2)))
}

cosine_pairwise <- function(x)
{
  cp <- crossprod(x)
  rtdg <- sqrt(diag(cp))
  cos <- cp / tcrossprod(rtdg)
  return(cos)
}

#' Caluclate weighted row sums of a matrix
rowsum_weighted <- function(mx, words, weights){
  common <- intersect(colnames(mx), words)
  mx2 <- mx[,match(common, colnames(mx))]
  mx3 <- mx2 %*% matrix(weights[match(common, words)], ncol=1)
  rownames(mx3) <- rownames(mx)
  colnames(mx3) <- 'wsum'
  return(mx3)
}



