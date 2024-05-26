#' \[experimental\] Compute polarity scores with different hyper-parameters
#'
#' A function to compute polarity scores of words and documents by resampling
#' hyper-parameters from a fitted LSS model.
#' @param x a fitted textmodel_lss object.
#' @param what choose the hyper-parameter to resample in bootstrapping.
#' @param mode choose the type of the result of bootstrapping. If `coef`,
#'   returns the polarity scores of words; if `terms`, returns words sorted by
#'   the polarity scores in descending order; if `predict`, returns the polarity
#'   scores of documents.
#' @param remove if `TRUE`, remove each seed word when `what = "seeds"`.
#' @param from,to,by passed to `seq()` to generate values for `k`; only used
#'   when `what = "k"`.
#' @param ... additional arguments passed to [as.textmodel_lss()] and
#'   [predict()].
#' @param verbose show messages if `TRUE`.
#' @details This function internally creates LSS fitted textmodel_lss objects by
#'   resampling hyper-parameters and computes polarity of words or documents.
#'   The resulting matrix can be used to asses the validity and the reliability
#'   of seeds or k.
#'
#'   Note that the objects created by [as.textmodel_lss()] does not contain data, users
#'   must pass `newdata` via `...` when `mode = "predict"`.
#' @export
#' @importFrom quanteda check_integer check_logical
bootstrap_lss <- function(x, what = c("seeds", "k"),
                          mode = c("terms", "coef", "predict"),
                          remove = FALSE,
                          from = 100, to = NULL, by = 50, verbose = FALSE, ...) {

    what <- match.arg(what)
    mode <- match.arg(mode)
    from <- check_integer(from, min = 1, max = x$k)
    remove <- check_logical(remove)
    if (!is.null(to)) {
        to <- check_integer(to, min = 1, max = x$k)
    } else {
        to <- x$k
    }
    by <- check_integer(by, min = 1)
    if (verbose)
        cat(sprintf("Call %s(x) with different hyper-parameters...\n", mode))
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
    } else if (mode == "predict") {
        result <- sapply(beta, function(x) {
            predict(as.textmodel_lss(x), se_fit = FALSE, ...)
        })
    } else {
        result <- do.call(cbind, beta)
    }

    attr(result, "what") <- what
    attr(result, "values") <- param
    return(result)
}


#' \[experimental\] Compute variance ratio with different hyper-parameters
#' @inheritParams bootstrap_lss
#' @keywords internal
#' @export
optimize_lss <- function(x, ...) {
    beta <- bootstrap_lss(x, mode = "coef", ...)
    pred <- bootstrap_lss(x, mode = "pred", ..., rescale = FALSE)
    # variance ratio
    disc <- apply(pred, 2, var, na.rm = TRUE) / apply(beta, 2, var, na.rm = TRUE)
    return(disc)
}
