#' a vector-space model for subject specific sentiment-analysis
#'
#' @param x a dfm created by \code{\link[quanteda]{dfm}}
#' @param y character vector or named character vector that contains seed words.
#' @param pattern pattern to select featues to make models only sensitive to
#'   subject specific words.
#' @param k the size of semantic space passed to \code{\link[RSpectra]{svds}}
#' @param ... additional argument passed to \code{\link[RSpectra]{svds}}
#' @export
#' @references Watanabe, Kohei. “Measuring News Bias: Russia’s Official News
#'   Agency ITAR-TASS’ Coverage of the Ukraine Crisis.” European Journal of
#'   Communication 32, no. 3 (March 20, 2017): 224–41.
#'   https://doi.org/10.1177/0267323117695735.
#' @examples
#' require(quanteda)
#'
#' load('/home/kohei/Dropbox/Public/guardian-sample.RData')
#' corp <- corpus_reshape(data_corpus_guardian, 'sentences')
#' toks <- tokens(corp)
#' mt <- dfm(toks, remove = stopwords())
#' mt <- dfm_trim(mt, min_count = 10)
#' lss <- textmodel_lss(mt, seedwords('pos-neg'))
#'
#' # sentiment model on economy
#' eco <- char_keyness(toks, 'econom*')
#' lss_eco <- textmodel_lss(mt, seedwords('pos-neg'), pattern = eco)
#'
#' # sentiment model on politics
#' pol <- char_keyness(toks, 'polti*')
#' lss_pol <- textmodel_lss(mt, seedwords('pos-neg'), pattern = pol)
textmodel_lss <- function(x, y, pattern = NULL, k = 300, verbose = FALSE, ...) {

    if (is.dictionary(y))
        y <- unlist(y, use.names = FALSE)

    # give equal weight to characters
    if (is.character(y))
        y <- structure(rep(1, length(y)), names = y)

    if (is.null(names(y)))
        stop("y must be a named-numerid vector\n")

    # generalte inflected seed
    seed <- unlist(mapply(weight_seeds, names(y), unname(y) / length(y),
                          MoreArgs = list(featnames(x)), USE.NAMES = FALSE))

    if (verbose) {
        cat('Weighted seed words:\n')
        print(seed)
    }

    if (verbose)
        cat('Calculating term-term similarity...\n')

    if (verbose)
        cat('Starting singular value decomposition of dfm...\n')

    s <- RSpectra::svds(x, k = k, nu = 0, nv = k, ...)
    temp <- t(s$v * s$d)
    colnames(temp) <- featnames(x)
    temp <- as.dfm(temp)
    result <- list(beta = get_beta(temp, seed, pattern),
                   data = x, feature = colnames(temp),
                   seed = seed)
    class(result) <- "textmodel_lss"

    return(result)
}
#' Internal function to beta parameters
#'
#' @param x svd-reduced dfm
#' @param y named-numberic vector for seed words
#' @param feature feature for which beta will be calcualted
#' @keywords internal
get_beta <- function(x, y, feature = NULL) {

    y <- y[intersect(colnames(x), names(y))] # dorp seed not in x
    seed <- names(y)
    weight <- unname(y)

    temp <- textstat_simil(x, selection = seed, margin = 'features')
    if (!is.null(feature))
        temp <- temp[unlist(quanteda:::regex2fixed(feature, rownames(temp), 'glob', FALSE)),,drop = FALSE]
    if (!identical(colnames(temp), seed))
        stop('Columns and seed words do not match')
    sort(rowMeans(temp %*% weight), decreasing = TRUE)
}


#' internal function to generate equally-weighted seed set
#'
#' @keywords internal
weight_seeds <- function(seed, weight, type) {
    s <- unlist(quanteda:::regex2fixed(seed, type, 'glob', FALSE))
    v <- rep(weight / length(s), length(s))
    names(v) <- s
    return(v)
}

#' prediction method for textmodel_lss
#' @param object a fitted LSS textmodel
#' @param newdata dfm on which prediction should be made
#' @param confidence.fit if \code{TRUE}, it also returns standard error of document scores.
#' @export
predict.textmodel_lss <- function(object, newdata = NULL, confidence.fit = FALSE){

    model <- as.dfm(rbind(object$beta))

    if (is.null(newdata)) {
        data <- dfm_select(object$data, model)
    } else {
        if (!is.dfm(newdata))
            stop('newdata must be a dfm\n')
        if (!identical(featnames(newdata), featnames(model))) {
            data <- dfm_select(newdata, model)
        } else {
            data <- newdata
        }
    }

    prop <- quanteda::dfm_weight(data, "prop")
    model <- as(model, 'dgCMatrix')
    mn <- Matrix::rowSums(prop %*% Matrix::t(model)) # mean scores of documents

    if (confidence.fit) {
        binary <- as(data, 'nMatrix') * model[rep(1, nrow(prop)),]
        dev <- mn - binary # deviation from the mean
        error <- (dev ** 2) * prop
        var <- unname(Matrix::rowSums(error))
        n <- unname(Matrix::rowSums(data))
        sd <- sqrt(var)
        se <- sd / sqrt(n)
        se <- ifelse(is.na(se), 0 , se)
        return(list(mue = mn, sigma = se, n = n))
    } else {
        return(mn)
    }
}

#' identify keywords occur frequently with target words
#'
#' @param x tokens object created by \code{\link[quanteda]{tokens}}.
#' @param pattern to specify target words.
#' @param window size of window for collocation analysis.
#' @param p threashold for statistical significance of collocaitons.
#' @param min_count minimum frequency for words within the window to be
#'   considered as collocations.
#' @param remove_pattern if \code{TRUE}, keywords do not containe target words.
#' @param ... additional arguments passed to \code{\link{textstat_keyness}}.
#' @export
#' @seealso \code{\link{textstat_keyness}}
#' @examples
#' require(quanteda)
#' load('/home/kohei/Dropbox/Public/guardian-sample.RData')
#' corp <- corpus_reshape(data_corpus_guardian, 'sentences')
#' toks <- tokens(corp, remove_punct = TRUE)
#' toks <- tokens_remove(toks, stopwords())
#'
#' # economy keywords
#' eco <- char_keyness(toks, 'econom*')
#' head(eco)
#'
#' # politics keywords
#' pol <- char_keyness(toks, 'politi*')
#' head(pol)
char_keyness <- function(x, pattern, window = 10, p = 0.001, min_count = 10,
                         remove_pattern = TRUE, ...) {

    if (!is.tokens(x))
        stop('x must be a tokens object\n')
    m <- dfm(tokens_keep(x, pattern, window = window))
    m <- dfm_trim(m, min_count = min_count)
    if (remove_pattern)
        m <- dfm_remove(m, pattern)
    n <- dfm(tokens_remove(x, pattern, window = window))
    key <- textstat_keyness(rbind(m, n), seq_len(ndoc(m)), ...)
    key <- key[key$p < p,]
    key$feature
}

#' seed words for sentiment analysis
#'
#' @param type type of seed words currently only for sentiment (\code{pos-neg})
#'   or political ideology (\code{left-right}).
#' @export
#' @examples
#' seedwords('pos-neg')
#' @references Turney, P. D., & Littman, M. L. (2003). Measuring Praise and
#'   Criticism: Inference of Semantic Orientation from Association. ACM Trans.
#'   Inf. Syst., 21(4), 315–346. https://doi.org/10.1145/944012.944013
seedwords <- function(type) {

    if (type == 'pos-neg') {
        seeds <- c(rep(1, 7), rep(-1, 7))
        names(seeds) <- c('good', 'nice', 'excellent', 'positive', 'fortunate', 'correct', 'superior',
                          'bad', 'nasty', 'poor', 'negative', 'unfortunate', 'wrong', 'inferior')
    } else if (type == 'left-right') {
        seeds <- c(rep(1, 7), rep(-1, 7))
        names(seeds) <- c('deficit', 'austerity', 'unstable', 'recession', 'inflation', 'currency', 'workforce',
                          'poor', 'poverty', 'free', 'benefits', 'prices', 'money', 'workers')
    } else {
        stop(type, 'is not currently available')
    }
    return(seeds)
}
