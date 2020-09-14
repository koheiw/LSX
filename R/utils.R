#' Identify noisy documents in a corpus
#' @param x character or [corpus] object whose texts will be diagnosed
#' @param ... extra arguments passed to `tokens`
#' @export
diagnosys <- function(x, ...) {
    UseMethod("diagnosys")
}

#' @export
#' @import quanteda
diagnosys.character <- function(x, ...) {
    diagnosys(corpus(x), ...)
}

#' @export
#' @import quanteda
diagnosys.corpus <- function(x, ...) {

    .Deprecated("quanteda::textstat_summary")
    dict <- dictionary(list(
        "number" = "\\p{N}",
        "punct" = "\\p{P}",
        "symbol" = "\\p{S}",
        "any" = "[\\p{N}\\p{P}\\p{S}]"
    ))

    n_sent <- ntoken(tokens(x, what = "sentence"))
    temp <- dfm(tokens(x, ...))
    n_token <- ntoken(temp)
    result <- convert(
        dfm_lookup(temp, dictionary = dict, valuetype = "regex"),
        "data.frame"
    )
    result$n_sent <- n_sent
    result$n_token <- n_token
    result$dupli <- duplicated(texts(x))
    result$noise <- result$any / result$n_token
    return(result)
}

#' Function to test quality of word vectors

#' Computes similarity between and within seed words
#'
#' Gauge the quality of word vectors given know seed words by the computing
#' similarities within and between antonyms.
#' @param object a fitted `textmodel_lss`
#' @export
#' @keywords internal
divergence <- function(object) {
    .Deprecated("cohesion")
    stopifnot("textmodel_lss" %in% class(object))
    seed <- unlist(unname(object$seeds))
    seed_sign <- sign(seed)
    temp <- object$similarity[names(seed_sign), names(seed_sign)]
    l <- tcrossprod(seed_sign) > 0
    diag(l) <- NA
    b <- mean(temp[!l], na.rm = TRUE) # between
    w <- mean(temp[l], na.rm = TRUE) # within
    c("within" = w, "between" = b, "diff" = w - b)
}

#' Computes cohesion of components of latent semantic analysis
#' @param object a fitted `textmodel_lss`
#' @param bandwidth size of window for smoothing
#' @export
#' @importFrom Matrix rowMeans rowSums tcrossprod tril
cohesion <- function(object, bandwidth = 10) {
    stopifnot("textmodel_lss" %in% class(object))
    seed <- unlist(unname(object$seeds))
    cross <- tcrossprod(object$embedding[,names(seed), drop = FALSE])
    cross <- tril(cross, -1)
    n <- seq_len(nrow(cross))
    h <- rowSums(abs(cross)) / (n - 1)
    h[1] <- NA # no similarity for k = 1
    temp <- data.frame(k = n, raw = log(h))
    temp$smoothed <- stats::ksmooth(temp$k, temp$raw, kernel = "normal",
                                    bandwidth = bandwidth)$y
    result <- list(
      "overall" = c("selected" = mean(temp$raw[object$slice], na.rm = TRUE),
                    "all" = mean(temp$raw, na.rm = TRUE)),
      "component" = temp
    )

    # d <- tcrossprod(object$embedding[, f, drop = FALSE])
    # d <- tril(d)
    # h <- abs(rowMeans(band(d, -1, -1)))
    # temp <- data.frame(k = seq_along(h), raw = log(h))
    # temp$smoothed <- stats::ksmooth(temp$k, temp$raw, kernel = "normal",
    #                                 bandwidth = bandwidth)$y
    # result <- list(
    #   "overall" = c("selected" = mean(temp$raw[object$s]),
    #                 "all" = mean(temp$raw)),
    #   "component" = temp
    # )
    class(result) <- "listof"
    return(result)
}


#' Function to test quality of seed words
#'
#' Gauge seed words' ability to discriminate unit texts by computing kurtosis of
#' document scores and terms weights.
#' @export
#' @keywords internal
discrimination <- function(object, newdata = NULL) {
    .Deprecated("cohesion")
    stopifnot("textmodel_lss" %in% class(object))
    p <- predict(object, newdata, rescaling = FALSE)
    t <- e1071::kurtosis(coef(object), na.rm = TRUE)
    d <- e1071::kurtosis(p, na.rm = TRUE)
    c("document" = d, "term" = t)
}
#' @rdname discrimination
#' @export
#' @keywords internal
#' @importFrom Matrix rowMeans colMeans diag band
strength <- function(object) {
    stopifnot("textmodel_lss" %in% class(object))
    term <- object$terms
    seed <- unlist(unname(object$seeds))
    sim <- proxyC::simil(object$embedding[object$slice,, drop = FALSE],
                         object$embedding[object$slice, names(seed), drop = FALSE], margin = 2)
    diag(sim) <- NA
    temp <- data.frame(seed = colnames(sim),
                       selected = log(1 / abs(colMeans(sim[term,,drop = FALSE], na.rm = TRUE))),
                       all = log(1 / abs(colMeans(sim, na.rm = TRUE))),
                       stringsAsFactors = FALSE)
    sim_mean <- rowMeans(sim, na.rm = TRUE)
    temp <- temp[order(temp$selected, decreasing = TRUE),, drop = FALSE]
    rownames(temp) <- NULL
    result <- list(
      "overall" = c("selected" =  log(1 / mean(abs(sim_mean[term]))),
                    "all" = log(1 / mean(abs(sim_mean)))
      ),
      "element" = temp
    )
    class(result) <- "listof"
    return(result)
}

#' Convinient function to convert a list to seed words
#' @param x a list of characters vectors or a [dictionary][quanteda::dictionary] object
#' @param upper numeric index or key for seed words for higher scores
#' @param lower numeric index or key for seed words for lower scores
#' @export
#' @return named numeric vector for seed words with polarity scores
as.seedwords <- function(x, upper = 1, lower = 2) {
    if (!"list" %in% class(x) && !quanteda::is.dictionary(x))
        stop("x must be a list or dictionary object")
    if (quanteda::is.dictionary(x))
        x <- quanteda::as.list(x)
    if (is.null(upper)) {
        pos <- character()
    } else {
        pos <- unlist(x[[upper]])
        if(!is.character(pos))
            stop("x must contain character vectors")
    }
    if (is.null(lower)) {
        neg <- character()
    } else {
        neg <- unlist(x[[lower]])
        if(!is.character(neg))
            stop("x must contain character vectors")
    }
    c(structure(rep(1, length(pos)), names = pos),
      structure(rep(-1, length(neg)), names = neg))
}

unused_dots <- function(...) {
    arg <- names(list(...))
    if (length(arg) == 1) {
        warning(arg[1], " argument is not used.\n", call. = FALSE)
    } else if (length(arg) > 1) {
        warning(paste0(arg, collapse = ", "), " arguments are not used.\n", call. = FALSE)
    }
}

