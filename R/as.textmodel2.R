rowMaxs <- function(x) {
  structure(x[cbind(seq_len(nrow(x))), max.col(x, "first")],
            names = rownames(x))
}

as.textmodel_lss2 <- function(x, seeds) {

  x$seeds <- seed
  x$seeds_weighted <- expand_seeds(seeds, names(x$frequency))
  class(x) <- c("textmodel_lss2", class(x))
  return(x)

}

predict.textmodel_lss2() <- function(x, min_n = 0L, mode = "mean") {
  s <- unlist(unname(x$seed_weights))
  p <- wordvector::probability(x, names(s), layer = "document", mode = "numeric")

  if (mode == "mean") {
    p <- rowMeans(p)
  } else {
    p <- rowMaxs(p)
  }
  if (min_n > 0)
    p <- p * (x$length / p(x$length, min_n))
  return(p)
}
