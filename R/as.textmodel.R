#' Create a dummy textmodel_lss object from numeric vector
#' @param x a named numeric vector
#' @keywords internal
#' @export
#' @examples
#' v <- c("a" = 0.1, "z" = -0.2, "d" = 0.3, "h" = -0.05)
#' lss <- as.textmodel_lss(v)
#' @return a textmodel_lss object with `x` as polarity words
as.textmodel_lss <- function(x, ...) {
    UseMethod("as.textmodel_lss")
}

#' @export
#' @method as.textmodel_lss matrix
as.textmodel_lss.matrix <- function(x, seeds,
                                    terms = NULL,
                                    simil_method = "cosine",
                                    verbose = FALSE, ...) {

    unused_dots(...)
    args <- list(terms = terms, seeds = seeds, ...)
    stopifnot(!is.null(colnames(x)))

    terms <- check_terms(terms, colnames(x))
    seeds <- check_seeds(seeds, colnames(x), verbose)
    simil <- get_simil(x, seeds, terms, seq_len(nrow(x)), simil_method)
    beta <- get_beta(simil$terms, seeds)

    result <- list(beta = beta,
                   k = nrow(x),
                   data = NULL,
                   terms = args$terms,
                   seeds = args$seeds,
                   seeds_weighted = seeds,
                   embedding = x,
                   similarity = simil$seed,
                   call = match.call())
    class(result) <- "textmodel_lss"
    return(result)
}

#' @export
#' @method as.textmodel_lss numeric
as.textmodel_lss.numeric <- function(x) {

    stopifnot(!is.null(names(x)))

    result <- list(beta = x,
                   k = NULL,
                   data = NULL,
                   terms = names(x),
                   seeds = NULL,
                   embedding = NULL,
                   call = match.call())
    class(result) <- "textmodel_lss"
    return(result)
}
