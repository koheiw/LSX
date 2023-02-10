#' \[experimental\] Compute polarity scores of words with different
#' hyper-parameters
#' A function to compute polarity scores of words by resampling
#' hyper-parameters from a fitted LSS model.
#' @param x a fitted textmodel_lss object.
#' @param what choose the parameter to resample in bootstrapping.
#' @param output choose the types of the output object. If `terms`, words are
#'   sorted in descending order.
#' @param by passed to `seq()` to generate values for `k`; only used for when
#'   `what = "k"`.
#' @param ... additional arguments passed to `as.textmodel_lss()`.
#' @export
#' @importFrom quanteda check_integer
bootstrap_lss <- function(x, what = c("seeds", "k"), output = c("terms", "coef"),
                          by = 50, ...) {

    what <- match.arg(what)
    output <- match.arg(output)
    by = check_integer(by, min = 1, max = x$k - 50)
    if (what == "seeds") {
        param <- names(x$seeds_weighted)
        beta <- lapply(param, function(y)
            as.textmodel_lss(x, seeds = y, terms = x$terms, ...)$beta)
        names(beta) <- param
    } else {
        param <- seq(50, x$k, by = by)
        beta <- lapply(param, function(y)
            as.textmodel_lss(x, seeds = x$seeds, terms = x$terms, slice = y, ...)$beta)
        names(beta) <- as.character(param)
    }
    if (output == "terms") {
        result <- sapply(beta, function(y) names(sort(y, decreasing = TRUE)))
    } else {
        result <- do.call(cbind, beta)
    }
    attr(result, "what") <- what
    attr(result, "values") <- param
    return(result)
}
