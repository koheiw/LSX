#' Identify noisy documents in a corpus
#' @param x character or \link{corpus} object whose texts will be diagnosed
#' @param ... extra arguments passed to \code{tokens}
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

#' Function to test qualtiy of word vectors

#' Computes similarity between and within seed words
#' @param object a fitted LSS textmodel
#' @export
#' @keywords internal
divergence <- function(object) {
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

#' Function to test qualtiy of seed words
#'
#' Computes kurtosis of document scores and terms weights
#' @inheritParams predict
#' @export
#' @keywords internal
discrimination <- function(object, newdata = NULL) {
    stopifnot("textmodel_lss" %in% class(object))
    p <- predict(object, newdata, rescaling = FALSE)
    t <- e1071::kurtosis(coef(object), na.rm = TRUE)
    d <- e1071::kurtosis(p, na.rm = TRUE)
    c("document" = d, "term" = t)
}

#' Convinient function to convert a list to seed words
#' @param x a list of characters vectors or a \link[quanteda]{dictionary} object
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




