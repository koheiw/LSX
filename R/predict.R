#' Prediction method for textmodel_lss
#'
#' @method predict textmodel_lss
#' @param object a fitted LSS textmodel
#' @param newdata a dfm on which prediction should be made
#' @param se_fit if `TRUE`, returns standard error of document scores.
#' @param density if `TRUE`, returns frequency of polarity words in documents.
#' @param rescaling if `TRUE`, normalizes polarity scores using `scale()`.
#' @param min_n set the minimum number of polarity words in documents.
#' @param ... not used
#' @details Polarity scores of documents are the means of polarity scores of
#'   words weighted by their frequency. When `se_fit = TRUE`, this function
#'   returns the weighted means, their standard errors, and the number of
#'   polarity words in the documents. When `rescaling = TRUE`, it converts the
#'   raw polarity scores to z sores for easier interpretation.
#'
#'   Documents tend to receive extreme polarity scores when they have only few
#'   polarity word. This is problematic when LSS is applied to short documents
#'   (e.g. social media posts) or individual sentences, but we can alleviate
#'   this problem by adding zero polarity words to short documents using
#'   `min_n`. This setting does not affect empty documents.
#' @import methods
#' @importFrom Matrix Matrix rowSums t
#' @importFrom quanteda is.dfm dfm_select
#' @export
predict.textmodel_lss <- function(object, newdata = NULL, se_fit = FALSE,
                                  density = FALSE, rescaling = TRUE, min_n = 0L, ...){


    args <- list(...)
    if ("se.fit" %in% names(args)) {
        .Deprecated(msg = "'se.fit' is deprecated; use 'se_fit'\n")
        se_fit <- args$se.fit
    }

    unused_dots(...)
    beta <- Matrix(object$beta, nrow = 1, sparse = TRUE,
                   dimnames = list(NULL, names(object$beta)))

    if (is.null(newdata)) {
        if (is.null(object$data))
            stop("LSS model includes no data, please supply a dfm using newdata.\n")
        data <- object$data
    } else {
        if (!is.dfm(newdata))
            stop("newdata must be a dfm\n", call. = FALSE)
        data <- newdata
    }

    if (density)
        den <- unname(rowSums(dfm_select(data, object$terms)) / rowSums(data))

    data <- dfm_match(data, colnames(beta))
    len <- unname(rowSums(data))
    n <- ifelse(len == 0, 0, pmax(len, min_n))
    fit <- ifelse(len == 0, NA, rowSums(data %*% t(beta)) / n)
    names(fit) <- rownames(data)

    if (rescaling) {
        fit_scaled <- scale(fit)
        result <- list(fit = rowSums(fit_scaled))
    } else {
        result <- list(fit = fit)
    }

    if (se_fit) {
        # sparse variance computation
        weight <- t(t(data > 0) * colSums(beta))
        var <- (rowSums(weight ^ 2 * data) / n) - (rowSums(weight * data) / n) ^ 2
        var <- zapsmall(var)
        se <- ifelse(n > 1, unname(sqrt(var) / sqrt(n)), NA)
        if (rescaling)
            se <- se / attr(fit_scaled, "scaled:scale")
        result$se.fit <- se
        result$n <- n
    }
    if (density)
        result$density <- den

    if (!se_fit && !density) {
        return(result$fit)
    } else {
        return(result)
    }
}
