#' \[experimental\] Compute polarity scores of words with different hyper-parameters
#' @param x a fitted textmodel_lss object.
#' @param what choose the parameter to resample in bootstrapping.
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
        colname <- as.character(seq(50, x$k, by = by))
    } else {
        sample <- replicate(n, sample(x$k, size = size, replace = FALSE), simplify = FALSE)
        beta <- lapply(sample, function(y) as.textmodel_lss(x, seeds = x$seeds, slice = y, ...)$beta)
        colname <- NULL
    }

    term <- lapply(beta, function(y) names(head(sort(y, decreasing = TRUE), 100)))
    result <- list(matrix(unlist(sample), ncol = length(sample)),
                   matrix(unlist(beta), ncol = length(beta),
                          dimnames = list(names(beta[[1]]), colname)),
                   matrix(unlist(term), ncol = length(term),
                          dimnames = list(NULL, colname)))
    names(result) <- c(what, "beta", "terms")
    return(result)
}

#' \[experimental\] Find candidates for seed words through bootstrapping
#' @param x a fitted textmodel_lss object.
#' @param rank the lowest rank of candidates returned from [bootstrap_lss()].
#' @keywords internal
#' @export
candidates <- function(x, rank = 10, ...) {
    result <- list()
    cat(sprintf("Searching words similar to %d seed words...\n", length(x$seeds_weighted)))
    for (seed in names(x$seeds_weighted)) {
        cat("  ", seed, "...\n")
        bs_slice <- bootstrap_lss(as.textmodel_lss(x, seeds = seed), what = "slice", ...)
        cand <- unique(as.character(head(bs_slice$terms, rank)))
        bs_seed <- bootstrap_lss(as.textmodel_lss(x, seeds = cand), what = "seeds")
        result[[seed]] <- names(sort(proxyC::colSds(bs_seed$beta), decreasing = TRUE))
    }
    return(result)
}
