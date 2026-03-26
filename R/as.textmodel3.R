rowMaxs <- function(x) {
  structure(x[cbind(seq_len(nrow(x)), max.col(x, "first"))],
            names = rownames(x))
}

#' @export
#' @keywords internal
#' @method as.textmodel_lss textmodel_doc2vec
as.textmodel_lss.textmodel_doc2vec <- function(x, seeds, max_prob = TRUE) {

  max_prob <- check_logical(max_prob)

  seeds_weighted <- expand_seeds(seeds, names(x$frequency), nested_weight = FALSE)
  seed <- unlist(unname(seeds_weighted))
  if (max_prob)
    seed <- sign(seed)
  prob <- wordvector::probability(x, names(seed), layer = "document", mode = "numeric")

  if (max_prob) {
    alpha <- rowMaxs(prob %*% diag(seed))
  } else {
    alpha <- rowSums(prob %*% diag(seed))
  }

  # pseudo beta
  data <- dfm(x$data, remove_padding = TRUE)
  data <- dfm_select(data, names(x$frequency), valuetype = "fixed",
                     case_insensitive = TRUE)
  freq <- colSums(data)
  beta <- colSums(data * alpha) / freq

  result <- build_lss(
    seeds = seeds,
    seeds_weighted = seeds_weighted,
    beta = beta,
    beta_type = "dummy",
    terms = names(beta),
    frequency = freq,
    call = try(match.call(sys.function(-1), call = sys.call(-1)), silent = TRUE)
  )
  # extra information
  result$max_prob <- max_prob
  result$alpha <- alpha
  result$length <- rowSums(data)

  class(result) <- c("textmodel_lss3", class(result))
  return(result)

}

#' @export
#' @keywords internal
#' @method predict textmodel_lss3
predict.textmodel_lss3 <- function(x, min_n = 0L) {
  p <- x$alpha
  if (min_n > 0)
    p <- p * (x$length / pmax(x$length, min_n))
  return(p)
}
