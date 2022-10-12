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
bootstrap_lss <- function(x, what = c("seeds", "k", "slice"), n = 10,
                          param = NULL, ...) {

    what <- match.arg(what)
    if (what == "seeds") {
        if (!is.null(param)) {
            param <- as.list(param)
        } else {
            n <- check_integer(n, min = 1)
            seed <- head(names(x$seeds_weighted), n)
            param <- as.list(seed)
        }
        colname <- seed
        beta <- lapply(param, function(y) as.textmodel_lss(x, seeds = y, ...)$beta)
    } else {
        if (what == "k") {
            if (!is.null(param)) {
                param <- check_integer(param, max_len = x$k, min = 1, max = x$k)
            } else {
                n <- check_integer(n, min = 1, max = x$k)
                param <- seq(0, x$k, length.out = n + 1)[-1]
            }
            colname <- as.character(param)
        } else {
            if (!is.null(param)) {
                if (!is.matrix(param))
                    stop("param for slice must be a matrix")
            } else {
                n <- check_integer(n, min = 1, max = x$k)
                param <- replicate(n, sample(x$slice, replace = TRUE), simplify = FALSE)
            }
            colname <- NULL
        }
        beta <- lapply(param, function(y) as.textmodel_lss(x, seeds = x$seeds, slice = y, ...)$beta)
    }

    # term <- lapply(beta, function(y) names(head(sort(y, decreasing = TRUE), 100)))
    # term <- matrix(unlist(term), ncol = length(term),
    #                dimnames = list(NULL, colname))
    beta <- matrix(unlist(beta), ncol = length(beta),
                   dimnames = list(names(beta[[1]]), colname))
    #strength <- colMeans(abs(beta[names(x$seeds_weight),,drop = FALSE]))
    result <- list("param" = param,
                   "beta" = beta)
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
candidates <- function(x, ...) {

    cat(sprintf("Searching words similar to %d seed words...\n", length(x$seeds_weighted)))
    result <- list()
    for (seed in names(x$seeds_weighted)) {
        cat(sprintf("   %s ", seed))
        bs <- bootstrap_lss(as.textmodel_lss(x, seeds = seed), what = "slice", ...)
        beta <- bs$beta
        #if (mode == "mode") {
            b <- beta[cbind(seq_len(nrow(beta)), max.col(beta))]
            names(b) <- rownames(beta)
            b <- sort(b, decreasing = TRUE)
        #} else {
        #    b <- sort(rowMeans(beta), decreasing = TRUE)
        #}
        f <- x$frequency[names(b)]
        temp <- data.frame(candidate = names(b), cosine = b,
                           frequency = 1 - (abs(f - f[1]) / max(f)))
        temp$r <- temp$cosine * temp$frequency
        temp <- temp[order(temp$r, decreasing = TRUE),]
        rownames(temp) <- NULL
        #print(head(temp, 20))
        cat(sprintf("~ %s...\n", paste(head(temp$candidate, 10), collapse = ", ")))
        result[[seed]] <- temp
    }
    return(result)
}
