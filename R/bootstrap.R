#' \[experimental\] Compute polarity scores of words with different hyper-parameters.
#' @param x a fitted textmodel_lss object.
#' @param n only for "slice"
#' @param size only for "slice"
#' @param by only for "k"
#' @keywords internal
#' @export
bootstrap_lss <- function(x, n = 10, parameter = c("seeds", "k", "slice"),
                          size = 100, by = 50, ...) {

    parameter <- match.arg(parameter)
    if (parameter == "seeds") {
        param <- as.list(names((x$seeds_weight)))
        beta <- lapply(param, function(y) as.textmodel_lss(x, seeds = y, ...)$beta)
        colname <- names(param)
    } else if (parameter == "k") {
        param <- as.list(seq(50, x$k, by = by))
        beta <- lapply(param, function(y) as.textmodel_lss(x, seeds = x$seeds, slice = y, ...)$beta)
        colname <- NULL
    } else {
        param <- replicate(n, sample(x$k, size = size, replace = FALSE), simplify = FALSE)
        beta <- lapply(param, function(y) as.textmodel_lss(x, seeds = x$seeds, slice = y, ...)$beta)
        colname <- NULL
    }

    term <- lapply(beta, function(y) names(head(sort(y, decreasing = TRUE), 100)))
    result <- list(parameter = parameter,
                   value = matrix(unlist(param), ncol = length(param)),
                   beta = matrix(unlist(beta), ncol = length(beta),
                                 dimnames = list(names(beta[[1]]), colname)),
                   term = matrix(unlist(term), ncol = length(term),
                                 dimnames = list(NULL, colname)))
    return(result)
}

