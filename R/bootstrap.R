#' \[experimental\] Compute polarity scores of words with different hyper-parameters.
#' @param x a fitted textmodel_lss object.
#' @param what choose the target of bootstrapping.
#' @param by only for "k"
#' @param n only for "slice"
#' @param size only for "slice"
#' @keywords internal
#' @export
bootstrap_lss <- function(x, what = c("seeds", "k", "slice"),
                          by = 50, n = 10, size = 100, ...) {

    what <- match.arg(what)
    if (what == "seeds") {
        sample <- as.list(names(x$seeds_weight))
        beta <- lapply(sample, function(y) as.textmodel_lss(x, seeds = y, ...)$beta)
        colname <- names(x$seeds_weight)
    } else if (what == "k") {
        sample <- as.list(seq(50, x$k, by = by))
        beta <- lapply(sample, function(y) as.textmodel_lss(x, seeds = x$seeds, slice = y, ...)$beta)
        colname <- NULL
    } else {
        sample <- replicate(n, sample(x$k, size = size, replace = FALSE), simplify = FALSE)
        beta <- lapply(sample, function(y) as.textmodel_lss(x, seeds = x$seeds, slice = y, ...)$beta)
        colname <- NULL
    }

    term <- lapply(beta, function(y) names(head(sort(y, decreasing = TRUE), 100)))
    result <- list(what = what,
                   sample = matrix(unlist(sample), ncol = length(sample)),
                   beta = matrix(unlist(beta), ncol = length(beta),
                                 dimnames = list(names(beta[[1]]), colname)),
                   terms = matrix(unlist(term), ncol = length(term),
                                  dimnames = list(NULL, colname)))
    return(result)
}

