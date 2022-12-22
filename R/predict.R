#' Prediction method for textmodel_lss
#'
#' @method predict textmodel_lss
#' @param object a fitted LSS textmodel.
#' @param newdata a dfm on which prediction should be made.
#' @param se_fit if `TRUE`, returns standard error of document scores.
#' @param density if `TRUE`, returns frequency of polarity words in documents.
#' @param divide if specified, polarity scores of words are dichotomized by the
#'   percentile value.
#' @param rescale if `TRUE`, normalizes polarity scores using `scale()`.
#' @param min_n set the minimum number of polarity words in documents.
#' @param ... not used
#' @details Polarity scores of documents are the means of polarity scores of
#'   words weighted by their frequency. When `se_fit = TRUE`, this function
#'   returns the weighted means, their standard errors, and the number of
#'   polarity words in the documents. When `rescale = TRUE`, it converts the
#'   raw polarity scores to z sores for easier interpretation. When `rescale =
#'   FALSE` and `divide` is used, polarity scores of documents are bounded by
#'   \[-0.5, 0.5\].
#'
#'   Documents tend to receive extreme polarity scores when they have only few
#'   polarity words. This is problematic when LSS is applied to short documents
#'   (e.g. social media posts) or individual sentences, but users can alleviate
#'   this problem by adding zero polarity words to short documents using
#'   `min_n`. This setting does not affect empty documents.
#' @import methods
#' @importFrom Matrix Matrix rowSums t
#' @importFrom quanteda is.dfm dfm_select check_integer check_double
#' @export
predict.textmodel_lss <- function(object, newdata = NULL, se_fit = FALSE,
                                  density = FALSE, rescale = TRUE,
                                  divide = NULL, min_n = 0L, ...){


    (function(se.fit, recaling, ...) unused_dots(...))(...) # trap deprecated args
    args <- list(...)
    if ("se.fit" %in% names(args)) {
        .Deprecated(msg = "'se.fit' is deprecated; use 'se_fit'\n")
        se_fit <- args$se.fit
    }
    if ("rescaling" %in% names(args)) {
        .Deprecated(msg = "'rescaling' is deprecated; use 'rescale'\n")
        rescale <- args$rescaling
    }
    min_n <- check_integer(min_n, min = 0)

    if (!is.null(divide)) {
        divide <- check_double(divide, min = 0, max = 1)
        object$beta <- structure((object$beta > quantile(object$beta, divide)) - 0.5,
                                 names = names(object$beta))
    }

    beta <- Matrix(object$beta, nrow = 1, sparse = TRUE,
                   dimnames = list(NULL, names(object$beta)))

    if (is.null(newdata)) {
        if (is.null(object$data))
            stop("The model includes no data, use newdata to supply a dfm.\n",
                 call. = FALSE)
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

    if (rescale) {
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
        if (rescale)
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
