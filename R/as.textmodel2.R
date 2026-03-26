
#' @rdname as.textmodel_lss
#' @export
#' @method as.textmodel_lss textmodel_word2vec
as.textmodel_lss.textmodel_word2vec <- function(x, seeds,
                                                  terms = NULL,
                                                  nested_weight = TRUE,
                                                  verbose = FALSE,
                                                  spatial = TRUE,
                                                  ...) {

  #args <- list(terms = terms, seeds = seeds)
  spatial <- check_logical(spatial)
  if (spatial) {

    if (x$version == as.numeric_version("0.1.0")) {
      values <- x$vector
    } else if (x$version >= as.numeric_version("0.6.0")) {
      values <- x$values$word
    } else {
      values <- x$values
    }
    result <- as.textmodel_lss(t(values), seeds = seeds, terms = terms,
                               nested_weight = nested_weight, ...)
    result$frequency <- x$frequency[names(result$beta)]
    result$concatenator <- x$concatenator
    result$type <- "word2vec"
    result$call <- try(match.call(sys.function(-1), call = sys.call(-1)), silent = TRUE)

  } else {

    if (!requireNamespace("wordvector"))
      stop("wordvector package must be installed")
    if (x$version < as.numeric_version("0.2.0"))
      stop("wordvector package must be v0.2.0 or later")

    s <- expand_seeds(seeds, names(x$frequency), nested_weight, verbose)
    seed <- unlist(unname(s))
    theta <- get_theta(terms, names(x$frequency))

    suppressWarnings({
      if (packageVersion("wordvector") >= "0.6.0") {
        prob <- wordvector::probability(x, names(seed), mode = "numeric")
      } else {
        prob <- wordvector::probability(x, names(seed), mode = "values")
      }
    })
    beta <- rowSums(prob[names(theta),,drop = FALSE] %*% seed) * theta

    result <- build_lss(
      beta = beta,
      beta_type = "probability",
      k = x$dim,
      terms = terms,
      seeds = seeds,
      seeds_weighted = seed,
      frequency = x$frequency[names(beta)],
      concatenator = x$concatenator,
      type = "word2vec",
      spatial = FALSE,
      call = try(match.call(sys.function(-1), call = sys.call(-1)), silent = TRUE)
    )
  }
  return(result)
}
