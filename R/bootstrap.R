#' \[experimental\] Compute polarity scores of words with different
#' hyper-parameters
#' @param x a fitted textmodel_lss object.
#' @param what choose the parameter to resample in bootstrapping.
#' @param by,from,to passed to `seq()` to generate values for `k`; only used for
#'   when `what = "k"`.
#' @param n the number of resampling; only used for when `what = "slice"`.
#' @param size the number of word vectors to be resampled; only used  when `what
#'   = "slice"`.
#' @param ... additional arguments passed to `as.textmodel_lss()`.
#' @return \item{seeds/k/slice}{sampled hyper-parameters.} \item{beta}{the
#'   polarity scores of words computed with the hyper-parameters.}
#'   \item{terms}{the most polarized words (only the top 100).}
#' @export
#' @importFrom quanteda check_integer
bootstrap_lss <- function(x, what = c("seeds", "k", "slice"),
                          by = 1, from = NULL, to = NULL,
                          n = 10L, size = 100, ...) {

    what <- match.arg(what)
    if (what == "seeds") {
        param <- as.list(names(x$seeds_weight))
        beta <- lapply(param, function(y) as.textmodel_lss(x, seeds = y, ...)$beta)
        colname <- names(x$seeds_weight)
    } else {
        by <- check_integer(by, min = 1, max = x$k)
        if (is.null(from))
            from <- 100
        if (is.null(to))
            to <- x$k
        k <- seq(from, to, by = by)
        if (what == "k") {
            param <- matrix(k, nrow = 1)
            beta <- lapply(param, function(y) as.textmodel_lss(x, seeds = x$seeds, slice = y, ...)$beta)
            colname <- as.character(k)
        } else {
            n <- check_integer(n, min = 1)
            size <- check_integer(size, min = 2, max = x$k)
            param <- replicate(n, sample(k, size = size, replace = FALSE), simplify = FALSE)
            beta <- lapply(param, function(y) as.textmodel_lss(x, seeds = x$seeds, slice = y, ...)$beta)
            param <- matrix(unlist(param), ncol = length(param))
            colname <- NULL
        }
    }

    term <- lapply(beta, function(y) names(head(sort(y, decreasing = TRUE), 100)))
    term <- matrix(unlist(term), ncol = length(term),
                   dimnames = list(NULL, colname))
    beta <- matrix(unlist(beta), ncol = length(beta),
                   dimnames = list(names(beta[[1]]), colname))
    strength <- colMeans(abs(beta[names(x$seeds_weight),,drop = FALSE]))
    result <- list("param" = param,
                   "beta" = beta,
                   "terms" = term,
                   "strength" = strength)
    names(result)[1] <- what
    return(result)
}

#' \[experimental\] Find candidates for seed words through bootstrapping
#' @param x a fitted textmodel_lss object.
#' @param rank the lowest rank of candidates returned from [bootstrap_lss()].
#' @param min_freq,max_freq the minimum and maximum frequency of the candidates
#'   specified in percentile. The frequency is obtained from the original corpus.
#' @param ... additional arguments to passed to `bootstrap_lss()` when `what = "slice"`.
#' @keywords internal
#' @export
candidates <- function(x, rank = 100, min_freq = 0.9, max_freq = 1.0, ...) {

    cat(sprintf("Searching words similar to %d seed words...\n", length(x$seeds_weighted)))

    q <- quantile(x$frequency, c(min_freq, max_freq))
    l <- q[1] <= x$frequency & x$frequency <= q[2]
    term <- names(x$frequency[l])

    result <- list()
    for (seed in names(x$seeds_weighted)) {
        cat(sprintf("   %s ", seed))

        bs_slice <- bootstrap_lss(as.textmodel_lss(x, seeds = seed, terms = term), what = "slice", ...)
        temp <- data.frame(word = as.character(bs_slice$terms), rank = as.integer(row(bs_slice$terms)))
        temp <- temp[!duplicated(temp$word),]
        temp <- temp[order(temp$rank),]
        cand <- head(temp$word, rank)

        bs_seed <- bootstrap_lss(as.textmodel_lss(x, seeds = cand), what = "seeds")
        cand <- names(sort(colMeans(abs(bs_seed$beta)), decreasing = TRUE))
        cat(sprintf("~ %s...\n", paste(head(cand, 10), collapse = ", ")))
        result[[seed]] <- cand
    }
    result <- as.data.frame(result, check.names = FALSE)
    return(result)
}
