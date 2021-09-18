#' Create a dummy textmodel_lss object from external objects
#'
#' Create a dummy textmodel_lss object from a numeric vector or matrix.
#' Pre-trained word-embedding models could used to perform LSS.
#' @param x an external object to construct dummy a dummy [textmodel_lss]
#'   object.
#' @param ... arguments used to construct a dummy object. `seeds` must be given
#'   when `x` is a dense matrix.
#' @details A named numeric vector and a dense matrix are set to `beta` and
#'   `embedding` respectively. A dense matrix should have column names for words.
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
                                    terms = NULL,
                                    simil_method = "cosine",
                                    auto_weight = FALSE,
                                    verbose = FALSE, ...) {

    unused_dots(...)
    args <- list(terms = terms, seeds = seeds)
    if (is.null(colnames(x)))
        stop("x must have column names for features")
    if (any(is.na(colnames(x))))
        stop("x must not have NA in the column names")
    if (any(is.na(x)))
        stop("x must not have NA")

    seeds <- expand_seeds(seeds, colnames(x), verbose)
    seed <- unlist(unname(seeds))
    term <- expand_terms(terms, colnames(x))
    feat <- union(term, names(seed))

    x <- x[,feat, drop = FALSE]
    simil <- get_simil(x, names(seed), term, seq_len(nrow(x)), simil_method)
    if (auto_weight)
        seed <- optimize_weight(seed, simil, verbose, ...)
    beta <- get_beta(simil, seed)

    result <- build_lss(
        beta = beta,
        k = nrow(x),
        terms = args$terms,
        seeds = args$seeds,
        seeds_weighted = seed,
        embedding = x,
        similarity = simil$seed,
        call = match.call()
    )
    return(result)
}

#' @export
#' @method as.textmodel_lss numeric
as.textmodel_lss.numeric <- function(x, ...) {

    unused_dots(...)
    if (is.null(names(x)))
        stop("x must have names for features")
    if (any(is.na(names(x))))
        stop("x must not have NA in the names")
    if (any(is.na(x)))
        stop("x must not have NA")

    result <- build_lss(
        beta = x,
        terms = names(x),
        call = match.call()
    )
    return(result)
}
