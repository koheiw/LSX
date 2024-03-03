#' \[experimental\] Compute polarity scores with different hyper-parameters
#'
#' A function to compute polarity scores of words by resampling hyper-parameters
#' from a fitted LSS model.
#' @param x a fitted textmodel_lss object.
#' @param what choose the hyper-parameter to resample in bootstrapping.
#' @param mode choose the type of the result of bootstrapping. If `coef`,
#'   returns polarity scores; if `terms`, returns words sorted by
#'   the polarity scores in descending order.
#' @param remove if `TRUE`, remove each seed word when `what = "seeds"`.
#' @param from,to,by passed to `seq()` to generate values for `k`; only used
#'   when `what = "k"`.
#' @param ... additional arguments passed to `as.textmodel_lss()`.
#' @param verbose show messages if `TRUE`.
#' @export
#' @importFrom quanteda check_integer check_logical
bootstrap_lss <- function(x, what = c("seeds", "k"), mode = c("terms", "coef"),
                          remove = FALSE,
                          from = 50, to = NULL, by = 50, verbose = FALSE, ...) {

    what <- match.arg(what)
    mode <- match.arg(mode)
    from <- check_integer(from, min = 1, max = x$k)
    remove <- check_logical(remove)
    if (!is.null(to)) {
        to <- check_integer(to, min = 1, max = x$k)
    } else {
        to <- x$k
    }
    by <- check_integer(by, min = 1, max = x$k - 50)
    if (verbose)
        cat(sprintf("Fitting textmodel_lss with a different hyper-parameter...\n"))
    if (what == "seeds") {
        param <- names(x$seeds_weighted)
        beta <- lapply(param, function(y) {
            if (remove) {
                seed <- setdiff(param, y)
                if (verbose) cat(sprintf('  seeds != "%s"\n', y))
            } else {
                seed <- y
                if (verbose) cat(sprintf('  seeds = "%s"\n', y))
            }

            as.textmodel_lss(x, seeds = seed, terms = x$terms, ...)$beta
        })
        names(beta) <- param
    } else {
        param <- seq(from, to, by = by)
        beta <- lapply(param, function(y) {
            if (verbose) cat(sprintf('  k = %d\n', y))
            as.textmodel_lss(x, seeds = x$seeds, terms = x$terms, slice = y, ...)$beta
        })
        names(beta) <- as.character(param)

    }
    if (mode == "terms") {
        result <- sapply(beta, function(y) names(sort(y, decreasing = TRUE)))
    } else {
        result <- do.call(cbind, beta)
    }
    attr(result, "what") <- what
    attr(result, "values") <- param
    return(result)
}
