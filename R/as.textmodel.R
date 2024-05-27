#' Create a dummy textmodel_lss object from external objects
#'
#' Create a dummy textmodel_lss object from a numeric vector, dense matrix or an
#' existing textmodel_lss object. Pre-trained word-embedding models could used
#' to perform LSS through this function.
#' @param x an object from which a dummy [textmodel_lss] object is created.
#' @param ... arguments used to create a dummy object. `seeds` must be given
#'   when `x` is a dense matrix.
#' @details A named numeric vector and a dense matrix are set to `beta` and
#'   `embedding` respectively. A dense matrix should have column names for
#'   words.
#' @keywords internal
#' @export
#' @examples
#' v <- c("a" = 0.1, "z" = -0.2, "d" = 0.3, "h" = -0.05)
#' lss <- as.textmodel_lss(v)
#'
#' @return a dummy [textmodel_lss] object
as.textmodel_lss <- function(x, ...) {
    UseMethod("as.textmodel_lss")
}

#' @export
#' @method as.textmodel_lss matrix
as.textmodel_lss.matrix <- function(x, seeds,
                                    terms = NULL, slice = NULL,
                                    simil_method = "cosine",
                                    auto_weight = FALSE,
                                    verbose = FALSE, ...) {

    args <- list(terms = terms, seeds = seeds)
    if (is.null(colnames(x)))
        stop("x must have column names for features")
    if (any(is.na(colnames(x))))
        stop("x must not have NA in the column names")
    if (any(is.na(x)))
        stop("x must not have NA")

    seeds <- expand_seeds(seeds, colnames(x), verbose)
    seed <- unlist(unname(seeds))
    theta <- get_theta(terms, colnames(x))
    feat <- union(names(theta), names(seed))

    if (is.null(slice)) {
        slice <- nrow(x)
    } else {
        slice <- check_integer(slice, min_len = 1, max_len = nrow(x), min = 1, max = nrow(x))
    }
    if (length(slice) == 1)
        slice <- seq_len(slice)

    simil <- get_simil(x, names(seed), names(theta), slice, simil_method)
    if (auto_weight)
        seed <- optimize_weight(seed, simil, verbose, ...)
    beta <- get_beta(simil, seed) * theta

    result <- build_lss(
        beta = beta,
        k = nrow(x),
        slice = slice,
        terms = args$terms,
        seeds = args$seeds,
        seeds_weighted = seed,
        embedding = x,
        similarity = simil$seed,
        call = try(match.call(sys.function(-1), call = sys.call(-1)), silent = TRUE)
    )
    return(result)
}

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

#' @export
#' @method as.textmodel_lss textmodel_lss
as.textmodel_lss.textmodel_lss <- function(x, ...) {
    if (is.null(x$embedding))
        stop("x must be a valid textmodel_lss object")
    result <- as.textmodel_lss(x$embedding, ...)
    result$data <- x$data
    result$frequency <- x$frequency[names(result$beta)]
    return(result)
}
