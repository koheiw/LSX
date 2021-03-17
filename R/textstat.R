
#' Identify context words using user-provided patterns
#'
#' @param x a tokens object created by [quanteda::tokens()].
#' @param pattern [quanteda::pattern()] to specify target words
#' @param valuetype the type of pattern matching: `"glob"` for "glob"-style
#'   wildcard expressions; `"regex"` for regular expressions; or `"fixed"` for
#'   exact matching. See [quanteda::valuetype()] for details.
#' @param case_insensitive ignore case when matching, if `TRUE`
#' @param window size of window for collocation analysis.
#' @param p threshold for statistical significance of collocations.
#' @param min_count minimum frequency of words within the window to be
#'   considered as collocations.
#' @param remove_pattern if `TRUE`, keywords do not contain target words.
#' @param ... additional arguments passed to [textstat_keyness()].
#' @importFrom quanteda.textstats textstat_keyness
#' @importFrom quanteda is.tokens tokens_remove tokens_select dfm dfm_trim dfm_match featnames as.dfm dfm_remove
#' @export
#' @seealso [tokens_select()] and [textstat_keyness()]
#' @examples
#' #' @examples
#' \donttest{
#' require(quanteda)
#' con <- url("https://bit.ly/2GZwLcN", "rb")
#' corp <- readRDS(con)
#' close(con)
#' corp <- corpus_reshape(corp, 'sentences')
#' toks <- tokens(corp, remove_punct = TRUE)
#' toks <- tokens_remove(toks, stopwords())
#'
#' # economy keywords
#' eco <- char_context(toks, 'econom*')
#' head(eco, 20)
#'
#' tstat_eco <- textstat_context(toks, 'econom*')
#' head(tstat_eco)
#'
#' # politics keywords
#' pol <- char_context(toks, 'politi*')
#' head(pol, 20)
#'
#' # politics keywords
#' tstat_pol <- textstat_context(toks, 'politi*')
#' head(tstat_pol)
#' }
textstat_context <- function(x, pattern, valuetype = c("glob", "regex", "fixed"),
                             case_insensitive = TRUE, window = 10, min_count = 10,
                             remove_pattern = TRUE, ...) {

    valuetype <- match.arg(valuetype)
    if (!is.tokens(x))
        stop("x must be a tokens object\n", call. = FALSE)

    # reference
    y <- tokens_remove(x, pattern, valuetype = valuetype,
                       case_insensitive = case_insensitive,
                       window = window, padding = FALSE)
    y <- dfm_remove(dfm(y), "")

    # target
    x <- tokens_select(x, pattern, valuetype = valuetype,
                       case_insensitive = case_insensitive,
                       window = window, padding = FALSE)
    if (remove_pattern)
        x <- tokens_remove(x, pattern, valuetype = valuetype,
                           case_insensitive = case_insensitive)
    x <- dfm_remove(dfm(x), "")

    x <- dfm_trim(x, min_termfreq = min_count)
    y <- dfm_match(y, featnames(x))
    if (sum(x) > 0) {
        result <- textstat_keyness(as.dfm(rbind(quanteda::colSums(x), quanteda::colSums(y))), ...)
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
                         remove_pattern = TRUE, p = 0.001) {
    result <- textstat_context(x, pattern = pattern, valuetype = valuetype,
                               case_insensitive = case_insensitive, window = window,
                               min_count = min_count, remove_pattern = remove_pattern)
    result <- result[result[[2]] > 0 & result$p < p,]
    return(result$feature)
}

#' @rdname textstat_context
#' @export
char_keyness <- function(x, pattern, valuetype = c("glob", "regex", "fixed"),
                         case_insensitive = TRUE, window = 10, min_count = 10,
                         remove_pattern = TRUE, p = 0.001) {
    char_context(x, pattern = pattern, valuetype = valuetype,
                 case_insensitive = case_insensitive, window = window,
                 min_count = min_count, remove_pattern = remove_pattern, p = p)
}
