#' Create a Latent Semantic Scaling model from various objects
#'
#' Create a new [textmodel_lss] object from an existing or foreign objects.
#' @param x an object from which a new [textmodel_lss] object is created. See details.
#' @param spatial if `TRUE`, return a spatial model. Otherwise, a probabilistic model.
#' @param ... arguments used to create a new object. `seeds` must be given
#'   when `x` is a dense matrix or a fitted textmodel_lss.
#' @inheritParams textmodel_lss
#' @details
#' If `x` is a [textmodel_lss], original word vectors are reused to compute polarity
#' scores with new seed words. It is also possible to subset word vectors via `slice`
#' if it was trained originally using SVD.
#'
#' If `x` is a dense matrix, it is treated as a column-oriented word vectors with which
#' polarity of words are computed. If `x` is a named numeric vector, the values are treated
#' as polarity scores of the words in the names.
#'
#' @export
#' @return a dummy [textmodel_lss] object
as.textmodel_lss <- function(x, ...) {
  UseMethod("as.textmodel_lss")
}

#' @rdname as.textmodel_lss
#' @export
#' @method as.textmodel_lss matrix
as.textmodel_lss.matrix <- function(x, seeds,
                                    terms = NULL, slice = NULL,
                                    simil_method = "cosine",
                                    nested_weight = TRUE,
                                    verbose = FALSE, ...) {

  if (is.null(colnames(x)))
    stop("x must have column names for features")
  if (any(is.na(colnames(x))))
    stop("x must not have NA in the column names")
  if (any(is.na(x)))
    stop("x must not have NA")

  s <- expand_seeds(seeds, colnames(x), nested_weight, verbose)
  seed <- unlist(unname(s))
  theta <- get_theta(terms, colnames(x))

  if (is.null(slice)) {
    slice <- nrow(x)
  } else {
    slice <- check_integer(slice, min_len = 1, max_len = nrow(x), min = 1, max = nrow(x))
  }
  if (length(slice) == 1)
    slice <- seq_len(slice)

  simil <- get_simil(x, names(seed), names(theta), slice, simil_method)
  beta <- get_beta(simil, seed) * theta

  result <- build_lss(
    beta = beta,
    k = nrow(x),
    slice = slice,
    terms = terms,
    seeds = seeds,
    seeds_weighted = seed,
    embedding = x,
    similarity = simil$seed,
    call = try(match.call(sys.function(-1), call = sys.call(-1)), silent = TRUE),
    version = utils::packageVersion("LSX")
  )
  return(result)
}

#' @rdname as.textmodel_lss
#' @export
#' @method as.textmodel_lss numeric
as.textmodel_lss.numeric <- function(x, ...) {

  if (is.null(names(x)))
    stop("x must have names for features")
  if (any(is.na(names(x))))
    stop("x must not have NA in the names")
  if (any(is.na(x)))
    stop("x must not have NA")

  result <- build_lss(
    beta = x,
    terms = names(x),
    call = try(match.call(sys.function(-1), call = sys.call(-1)), silent = TRUE)
  )
  return(result)
}

#' @rdname as.textmodel_lss
#' @export
#' @method as.textmodel_lss textmodel_lss
as.textmodel_lss.textmodel_lss <- function(x, ...) {
  if (is.null(x$embedding))
    stop("x must be a valid textmodel_lss object")
  result <- as.textmodel_lss(x$embedding, ...)
  result$concatenator <- x$concatenator
  result$data <- x$data
  result$frequency <- x$frequency[names(result$beta)]
  return(result)
}
