
#' Prediction method for textmodel_lss
#'
#' @method predict textmodel_lss
#' @param object a fitted LSS textmodel
#' @param newdata a dfm on which prediction should be made
#' @param se.fit if `TRUE`, it returns standard error of document scores.
#' @param density if `TRUE`, returns frequency of model terms in documents.
#'   Density distribution of model terms can be used to remove documents about
#'   unrelated subjects.
#' @param rescaling if `TRUE`, scores are normalized using `scale()`.
#' @param min_n set the minimum number of polarity words
#'   to avoid short documents to receive extreme polarity scores. This does not
#'   affect empty or longer documents.
#' @param ... not used
#' @import methods
#' @importFrom Matrix Matrix rowSums t
#' @importFrom quanteda is.dfm dfm_select
#' @export
predict.textmodel_lss <- function(object, newdata = NULL, se.fit = FALSE,
                                  density = FALSE, rescaling = TRUE, min_n = 0L, ...){

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
    n <- unname(rowSums(data))
    empty <- n == 0
    n <- pmax(n, min_n)
    # mean scores of documents excluding zeros
    fit <- ifelse(empty, NA, rowSums(data %*% t(beta)) / n)
    names(fit) <- rownames(data)

    if (rescaling) {
        fit_scaled <- scale(fit)
        result <- list(fit = rowSums(fit_scaled))
    } else {
        result <- list(fit = fit)
    }

    if (se.fit) {
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

    if (!se.fit && !density) {
        return(result$fit)
    } else {
        return(result)
    }
}
