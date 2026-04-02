#' @noRd
#' @export
#' @keywords internal
#' @param prob_mode select how to compute polarity for documents. See details.
#' @details
#' If `x` is a [wordvector::textmodel_doc2vec] object, it computes
#' polarity scores of documents based on their probabilities for seed words.
#' If `prob_mode = "mean"`, it averages the probabilities, but it keeps used only
#' the highest probability for one of the seed words if `"max"`.
#'
#' @method as.textmodel_lss textmodel_doc2vec
as.textmodel_lss.textmodel_doc2vec <- function(x, seeds, prob_mode = c("mean", "max"),
                                               ...) {

  prob_mode <- match.arg(prob_mode)

  if (!requireNamespace("wordvector"))
    stop("wordvector package must be installed")
  if (x$version < as.numeric_version("0.6.0"))
    stop("wordvector package must be v0.6.0 or later")

  s <- expand_seeds(seeds, names(x$frequency), nested_weight = FALSE)
  seed <- unlist(unname(s))
  prob <- wordvector::probability(x, names(seed), layer = "document", mode = "numeric")

  if (prob_mode == "max") {
    alpha <- rowMaxs(prob %*% diag(sign(seed)))
  } else {
    alpha <- rowSums(prob %*% diag(seed))
  }

  # pseudo beta
  if (is.null(x$data)) {
    freq <- x$frequency
    beta <- NULL
  } else {
    data <- dfm(x$data, remove_padding = TRUE)
    data <- dfm_select(data, names(x$frequency), valuetype = "fixed",
                       case_insensitive = TRUE)
    freq <- colSums(data)
    beta <- colSums(data * alpha) / freq
  }

  result <- build_lss(
    seeds = seeds,
    seeds_weighted = seed,
    beta = beta,
    beta_type = "dummy",
    terms = names(freq),
    frequency = freq,
    concatenator = x$concatenator,
    type = "doc2vec",
    spatial = FALSE,
    call = try(match.call(sys.function(-1), call = sys.call(-1)), silent = TRUE)
  )
  # extra information
  result$prob_mode <- prob_mode
  result$alpha <- alpha
  result$length <- x$ntoken

  class(result) <- c("textmodel_lss3", class(result))
  return(result)

}

#' @export
#' @keywords internal
#' @method predict textmodel_lss3
predict.textmodel_lss3 <- function(object, min_n = 0L, ...) {
  p <- object$alpha
  if (min_n > 0)
    p <- p * (object$length / pmax(object$length, min_n))
  return(p)
}
