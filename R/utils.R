#' Diagnose the amount of noise in documents in a corpus
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
        "symbol" = "\\p{S}"
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
    result$noise <- (result$number + result$punct + result$symbol) / result$n_token
    return(result)
}

