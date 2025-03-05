
#' Identify context words
#'
#' Identify context words using user-provided patterns.
#' @param x a tokens object created by [quanteda::tokens()].
#' @param pattern [quanteda::pattern()] to specify target words.
#' @param valuetype the type of pattern matching: `"glob"` for "glob"-style
#'   wildcard expressions; `"regex"` for regular expressions; or `"fixed"` for
#'   exact matching. See [quanteda::valuetype()] for details.
#' @param case_insensitive if `TRUE`, ignore case when matching.
#' @param window size of window for collocation analysis.
#' @param p threshold for statistical significance of collocations.
#' @param min_count minimum frequency of words within the window to be
#'   considered as collocations.
#' @param remove_pattern if `TRUE`, keywords do not contain target words.
#' @inheritParams quanteda::tokens_ngrams
#' @param ... additional arguments passed to [quanteda.textstats::textstat_keyness()].
#' @importFrom quanteda.textstats textstat_keyness
#' @importFrom quanteda is.tokens tokens_remove tokens_select tokens_ngrams dfm
#'   dfm_trim dfm_match featnames as.dfm dfm_remove
#' @export
#' @seealso [quanteda.textstats::textstat_keyness()]
textstat_context <- function(x, pattern, valuetype = c("glob", "regex", "fixed"),
                             case_insensitive = TRUE, window = 10, min_count = 10,
                             remove_pattern = TRUE, n = 1, skip = 0, ...) {

    valuetype <- match.arg(valuetype)
    if (!is.tokens(x))
        stop("x must be a tokens object\n", call. = FALSE)

    # reference
    y <- tokens_remove(x, pattern, valuetype = valuetype,
                       case_insensitive = case_insensitive,
                       window = window, padding = FALSE)
    if (any(n > 1))
        y <- tokens_ngrams(y, n = n, skip = skip)

    # target
    x <- tokens_select(x, pattern, valuetype = valuetype,
                       case_insensitive = case_insensitive,
                       window = window, padding = FALSE)
    if (remove_pattern)
        x <- tokens_remove(x, pattern, valuetype = valuetype,
                           case_insensitive = case_insensitive)
    if (any(n > 1))
        x <- tokens_ngrams(x, n = n, skip = skip)

    y <- dfm_remove(dfm(y), pattern = "")
    x <- dfm_remove(dfm(x), pattern = "")

    f <- union(featnames(x), featnames(y))
    x <- dfm_match(x, f)
    y <- dfm_match(y, f)

    if (sum(x) > 0) {
        result <- textstat_keyness(as.dfm(rbind(quanteda::colSums(x), quanteda::colSums(y))), ...)
        result <- result[result$n_target >= min_count,]
    } else {
        result <- head(textstat_keyness(as.dfm(matrix(c(1, 0))), ...), 0) # dummy object
    }
    colnames(result)[c(4, 5)] <- c("n_inside", "n_outside")
    return(result)
}

#' @rdname textstat_context
#' @export
char_context <- function(x, pattern, valuetype = c("glob", "regex", "fixed"),
                         case_insensitive = TRUE, window = 10, min_count = 10,
                         remove_pattern = TRUE, p = 0.001, n = 1, skip = 0) {
    result <- textstat_context(x, pattern = pattern, valuetype = valuetype,
                               case_insensitive = case_insensitive, window = window,
                               min_count = min_count, remove_pattern = remove_pattern,
                               n = n, skip = skip)
    result <- result[result[[2]] > 0 & result$p < p,]
    return(result$feature)
}
