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
    seed_weighted <- unlist(unname(object$seeds_weighted))
    seed_sign <- sign(seed_weighted)
    temp <- object$similarity[names(seed_sign), names(seed_sign)]
    l <- tcrossprod(seed_sign) > 0
    diag(l) <- NA
    b <- mean(temp[!l], na.rm = TRUE) # between
    w <- mean(temp[l], na.rm = TRUE) # within
    c("within" = w, "between" = b, "diff" = w - b)
}

#' Computes cohesion of components of word embedding
#' @param object a fitted `textmodel_lss`
#' @param bandwidth size of window for smoothing
#' @export
#' @keywords internal
cohesion <- function(object, bandwidth = 10) {
    stopifnot("textmodel_lss" %in% class(object))
    s <- sign(unlist(unname(object$seeds_weighted)))
    f <- names(s)
    d <- Matrix::tcrossprod(object$embedding[, f, drop = FALSE])
    d <- Matrix::tril(d)
    temp <- data.frame(k = seq_len(nrow(d)),
                         raw = log(rowSums(abs(d) / seq_len(nrow(d)))))
    temp$smoothed <- stats::ksmooth(temp$k, temp$raw, kernel = "normal",
                                    bandwidth = bandwidth)$y
    result <- list(
      "overall" = c("selected" = mean(temp$raw[object$s]),
                    "all" = mean(temp$raw)),
      "component" = temp
    )
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
strength <- function(object) {
    stopifnot("textmodel_lss" %in% class(object))
    f <- object$features
    s <- sign(unlist(unname(object$seeds_weighted))) > 0
    m <- proxyC::simil(object$embedding,
                       object$embedding[, names(s), drop = FALSE], margin = 2)
    b <- rowMeans(m)
    Matrix::diag(m) <- NA
    temp <- data.frame(seed = colnames(m),
                       selected = log(1 / abs(colMeans(m[f,], na.rm = TRUE))),
                       all = log(1 / abs(colMeans(m, na.rm = TRUE))),
                       #selected = log(1 / apply(m[f,], 2, function(x) abs(mean(x, na.rm = TRUE)))),
                       #all = log(1 / apply(m, 2, function(x) abs(mean(x, na.rm = TRUE)))),
                       stringsAsFactors = FALSE)
    temp <- temp[order(temp$selected, decreasing = TRUE),]
    rownames(temp) <- NULL
    result <- list(
      "overall" = c("selected" =  log(1 / mean(abs(b[f]))),
                    "all" = log(1 / mean(abs(b)))
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

