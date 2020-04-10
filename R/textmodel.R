#' A word embeddings-based semisupervised model for document scaling
#'
#' @param x a dfm or fcm created by [quanteda::dfm()] or [quanteda::fcm()]
#' @param seeds a character vector, named numeric vector or dictionary that
#'   contains seed words.
#' @param terms words weighted as model terms. All the features of
#'   [quanteda::dfm()] or [quanteda::fcm()] will be used if not specified.
#' @param weight weighting scheme passed to [quanteda::dfm_weight()]. Ignored
#'   when `engine` is "rsparse".
#' @param simil_method specifies method to compute similarity between features.
#'   The value is passed to [quanteda::textstat_simil()], "cosine" is used
#'   otherwise.
#' @param cache if `TRUE`, save retult of SVD for next execution with identical
#'   `x` and settings.
#' @param engine choose SVD engine between [RSpectra::svds()], [irlba::irlba()],
#'   and [rsparse::GloVe()].
#' @param verbose show messages if `TRUE`.
#' @param ... additional argument passed to the SVD engine
#' @import quanteda
#' @export
#' @references Watanabe, Kohei. "Measuring News Bias: Russia's Official News
#'   Agency ITAR-TASS' Coverage of the Ukraine Crisis." European Journal of
#'   Communication 32, no. 3 (March 20, 2017): 224–41.
#'   https://doi.org/10.1177/0267323117695735.
#' @examples
#' \dontrun{
#' require(quanteda)
#'
#' # Available at https://bit.ly/2GZwLcN
#' corp <- readRDS("data_corpus_guardian2016-10k.rds")
#'
#' toks <- corpus_reshape(corp, "sentences") %>%
#'         tokens(remove_punct = TRUE) %>%
#'         tokens_remove(stopwords("en")) %>%
#'         tokens_select("^[\\p{L}]+$", valuetype = "regex", padding = TRUE)
#' dfmt <- dfm(toks) %>%
#'         dfm_trim(min_termfreq = 10)
#'
#' seed <- as.seedwords(data_dictionary_sentiment)
#'
#' # SVD
#' svd <- textmodel_lss(dfmt, seed)
#' summary(lss)
#'
#' # sentiment model on economy
#' eco <- head(char_keyness(toks, 'econom*'), 500)
#' svd_eco <- textmodel_lss(dfmt, seed, terms = eco)
#'
#' # sentiment model on politics
#' pol <- head(char_keyness(toks, 'politi*'), 500)
#' svd_pol <- textmodel_lss(dfmt, seed, terms = pol)
#'
#' # GloVe
#' fcmt  <- fcm(toks, context = "window", count = "weighted", weights = 1 / (1:5), tri = TRUE)
#' glov <- textmodel_lss(fcmt, seed)
#' }
#'
#' @export
textmodel_lss <- function(x, ...) {
    UseMethod("textmodel_lss")
}

#' @rdname textmodel_lss
#' @param k the number of singular values requested to the SVD engine. Only used
#'   when `x` is a `dfm`.
#' @param slice a number or indices of the components of word vectors used to
#'   compute similarity; `slice < k` to truncate word vectors; useful for diagnosys
#'   and simulation.
#' @param include_data if `TRUE`, fitted model include the dfm supplied as `x`.
#' @method textmodel_lss dfm
#' @export
textmodel_lss.dfm <- function(x, seeds, terms = NULL, k = 300, slice = NULL,
                              weight = "count", cache = FALSE,
                              simil_method = "cosine",
                              engine = c("RSpectra", "irlba", "rsvd"),
                              include_data = FALSE,
                              verbose = FALSE, ...) {

    unused_dots(...)
    args <- list(...)
    if ("features" %in% names(args)) {
        .Deprecated(msg = "'features' is deprecated; use 'terms'\n")
        terms <- args$features
    }
    engine <- match.arg(engine)
    terms <- check_terms(terms, featnames(x))
    seeds <- check_seeds(seeds, featnames(x), verbose)

    if (engine %in% c("RSpectra", "irlba", "rsvd")) {
        if (verbose)
            cat("Performing SVD by ", engine, "...\n")
        svd <- cache_svd(x, k, weight, engine, cache, ...)
        temp <- t(svd$v)
        colnames(temp) <- featnames(x)
        embed <- Matrix::Matrix(temp, sparse = TRUE)
        import <- svd$d
    }
    if (is.null(slice))
        slice <- k

    # identify relevance to seed words
    seed <- unlist(unname(seeds))
    cos <- proxyC::simil(embed[,names(seed),drop = FALSE],
                         Matrix::Matrix(seed, nrow = 1, sparse = TRUE),
                         margin = 1)
    relev <- abs(as.numeric(cos))

    k <- as.integer(k)
    slice <- as.integer(slice)
    if (any(slice < 1L) || any(k < slice))
        stop("'slice' must be between 1 and k")
    if (length(slice) == 1)
        slice <- seq_len(slice)
    simil <- get_simil(embed, seeds, terms, slice, simil_method)
    beta <- get_beta(simil$terms, seeds)
    result <- list(beta = beta,
                   k = k, slice = slice,
                   frequency = colSums(x)[names(beta)],
                   terms = terms,
                   seeds = seeds,
                   embedding = embed,
                   similarity = simil$seed,
                   relevance = relev,
                   importance = import,
                   call = match.call())

    if (include_data)
        result$data <- x
    class(result) <- "textmodel_lss"
    return(result)
}

#' @rdname textmodel_lss
#' @param w the size of word vectors. Only used when `x` is a `fcm`
#' @method textmodel_lss fcm
#' @export
textmodel_lss.fcm <- function(x, seeds, terms = NULL, w = 50,
                              weight = "count", cache = FALSE,
                              simil_method = "cosine",
                              engine = c("rsparse"),
                              verbose = FALSE, ...) {

    unused_dots(...)
    args <- list(...)
    if ("features" %in% names(args)) {
        .Deprecated(msg = "'features' is deprecated; use 'terms'.\n")
        terms <- args$features
    }
    if (engine == "text2vec") {
        .Deprecated(msg = "GloVe engine has been moved to from text2vec to rsparse.\n")
        engine <- "rsparse"
    }
    engine <- match.arg(engine)
    terms <- check_terms(terms, featnames(x))
    seeds <- check_seeds(seeds, featnames(x), verbose)

    if (engine == "rsparse") {
        if (verbose)
            cat("Fitting GloVe model by rsparse...\n")
        glove <- cache_glove(x, w, cache, ...)
        embed <- as(glove, "dgCMatrix")
    }

    simil <- get_simil(embed, seeds, terms, seq_len(w), simil_method)
    beta <- get_beta(simil$terms, seeds)

    result <- list(beta = beta,
                   w = w,
                   terms = terms,
                   seeds = seeds,
                   embedding = embed,
                   similarity = simil$seed,
                   call = match.call())

    class(result) <- "textmodel_lss"
    return(result)
}

check_terms <- function(terms, features) {
    if (is.null(terms))
        terms <- features
    if (!is.character(terms))
        stop("features must be a character vector\n", call. = FALSE)
    intersect(terms, features)
}

check_seeds <- function(seeds, features, verbose = FALSE) {
    seeds <- get_seeds(seeds)
    seeds_weighted <- mapply(weight_seeds, names(seeds), unname(seeds) / length(seeds),
                             MoreArgs = list(features), USE.NAMES = TRUE, SIMPLIFY = FALSE)

    if (all(lengths(seeds_weighted) == 0))
        stop("No seed word is found in the dfm", call. = FALSE)

    if (verbose)
        cat("Calculating term-term similarity to", sum(lengths(seeds_weighted)),
            "seed words...\n")

    return(seeds_weighted)
}

get_simil <- function(embed, seeds, terms, slice, method = "cosine") {
    seed <- unlist(unname(seeds))
    simil <- as.matrix(proxyC::simil(embed[slice,,drop = FALSE],
                                     embed[slice,names(seed),drop = FALSE],
                                     margin = 2, method = method))
    list("terms" = simil[unlist(pattern2fixed(terms, rownames(simil), "glob", FALSE)),,drop = FALSE],
         "seeds" = simil[rownames(simil) %in% names(seed),
                         colnames(simil) %in% names(seed), drop = FALSE])
}

get_beta <- function(simil, seeds) {
    seed <- unlist(unname(seeds))
    if (!identical(colnames(simil), names(seed)))
        stop("Columns and seed words do not match", call. = FALSE)
    sort(Matrix::rowMeans(simil %*% seed), decreasing = TRUE)
}

cache_svd <- function(x, k, weight, engine, cache = TRUE, ...) {

    x <- dfm_weight(x, scheme = weight)
    hash <- digest::digest(list(as(x, "dgCMatrix"), k,
                                utils::packageVersion("LSS")),
                           algo = "xxhash64")
    if (cache && !dir.exists("lss_cache"))
        dir.create("lss_cache")
    if (engine == "RSpectra") {
        file_cache <- paste0("lss_cache/svds_", hash, ".RDS")
    } else if (engine == "rsvd") {
        file_cache <- paste0("lss_cache/rsvd_", hash, ".RDS")
    } else {
        file_cache <- paste0("lss_cache/irlba_", hash, ".RDS")
    }

    if (cache && file.exists(file_cache)){
        message("Reading cache file: ", file_cache)
        result <- readRDS(file_cache)
    } else {
        if (engine == "RSpectra") {
            result <- RSpectra::svds(as(x, "dgCMatrix"), k = k, nu = 0, nv = k, ...)
        } else if (engine == "rsvd") {
            result <- rsvd::rsvd(as(x, "dgCMatrix"), k = k, nu = 0, nv = k, ...)
        } else {
            result <- irlba::irlba(as(x, "dgCMatrix"), nv = k, right_only = TRUE, ...)
        }
        if (cache) {
            message("Writing cache file: ", file_cache)
            saveRDS(result, file_cache)
        }
    }
    return(result)
}

cache_glove <- function(x, w, x_max = 10, n_iter = 10, cache = TRUE, ...) {

    hash <- digest::digest(list(as(x, "dgCMatrix"), w, x_max, n_iter,
                                utils::packageVersion("LSS")),
                           algo = "xxhash64")
    if (cache && !dir.exists("lss_cache"))
        dir.create("lss_cache")
    file_cache <- paste0("lss_cache/rsparse_", hash, ".RDS")

    if (cache && file.exists(file_cache)){
        message("Reading cache file: ", file_cache)
        result <- readRDS(file_cache)
    } else {
        glove <- rsparse::GloVe$new(rank = w, x_max = x_max)
        result <- t(glove$fit_transform(Matrix::drop0(x), n_iter = n_iter, ...))
        result <- result + glove$components
        if (cache) {
            message("Writing cache file: ", file_cache)
            saveRDS(result, file_cache)
        }
    }
    return(result)
}

get_seeds <- function(seeds) {

    if (is.dictionary(seeds))
        seeds <- unlist(seeds, use.names = FALSE)

    # give equal weight to characters
    if (is.character(seeds))
        seeds <- structure(rep(1, length(seeds)), names = seeds)

    if (is.null(names(seeds)))
        stop("y must be a named-numerid vector\n", call. = FALSE)

    return(seeds)
}

#' @export
#' @noRd
#' @importFrom stats coef
#' @importFrom utils head
#' @importFrom quanteda.textmodels as.coefficients_textmodel
#' @importFrom quanteda.textmodels as.summary.textmodel
#' @method summary textmodel_lss
summary.textmodel_lss <- function(object, n = 30L, ...) {
    result <- list(
        "call" = object$call,
        "seeds" = unlist(unname(object$seeds)),
        "beta" = as.coefficients_textmodel(head(coef(object), n))
    )
    if (!any("data" == names(object)))
        result$data.dimension <- dim(object$data)
    as.summary.textmodel(result)
}

#' Extract model coefficients from a fitted textmodel_lss object
#'
#' `coef()` extract model coefficients from a fitted `textmodel_lss`
#' object.  `coefficients()` is an alias.
#' @param object a fitted [textmodel_lss] object
#' @param ... unused
#' @keywords textmodel internal
#' @export
coef.textmodel_lss <- function(object, ...) {
    object$beta
}

#' @rdname coef.textmodel_lss
#' @export
coefficients.textmodel_lss <- function(object, ...) {
    UseMethod("coef")
}

#' Internal function to generate equally-weighted seed set
#'
#' @keywords internal
weight_seeds <- function(seed, weight, type) {
    s <- unlist(pattern2fixed(seed, type, "glob", FALSE))
    v <- rep(weight / length(s), length(s))
    names(v) <- s
    return(v)
}

#' Prediction method for textmodel_lss
#'
#' @method predict textmodel_lss
#' @param object a fitted LSS textmodel
#' @param newdata dfm on which prediction should be made
#' @param se.fit if `TRUE`, it returns standard error of document scores.
#' @param density if `TRUE`, returns frequency of model terms in documents.
#'   Density distribution of model terms can be used to remove documents about
#'   unrelated subjects.
#' @param rescaling if `TRUE`, scores are normalized using `scale()`.
#' @param ... not used
#' @keywords internal
#' @import methods
#' @importFrom Matrix Matrix rowSums t
#' @export
predict.textmodel_lss <- function(object, newdata = NULL, se.fit = FALSE,
                                  density = FALSE, rescaling = TRUE, ...){

    beta <- Matrix(object$beta, nrow = 1, sparse = TRUE,
                    dimnames = list(NULL, names(object$beta)))

    if (is.null(newdata)) {
        if (!any("data" == names(object)))
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
    # mean scores of documents excluding zeros
    fit <- ifelse(n > 0, rowSums(data %*% t(beta)) / n, NA)
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

#' Identify keywords occur frequently with target words
#'
#' @param x tokens object created by [quanteda::tokens()].
#' @param pattern to specify target words. See [quanteda::pattern()] for details.
#' @param valuetype the type of pattern matching: `"glob"` for
#'   "glob"-style wildcard expressions; `"regex"` for regular expressions;
#'   or `"fixed"` for exact matching. See [quanteda::valuetype()] for details.
#' @param case_insensitive ignore case when matching, if `TRUE`
#' @param window size of window for collocation analysis.
#' @param p threshold for statistical significance of collocations.
#' @param min_count minimum frequency for words within the window to be
#'   considered as collocations.
#' @param remove_pattern if `TRUE`, keywords do not containe target words.
#' @param ... additional arguments passed to [textstat_keyness()].
#' @export
#' @seealso [tokens_select()] and [textstat_keyness()]
#' @examples
#' \dontrun{
#' require(quanteda)
#' # Available at https://bit.ly/2GZwLcN
#' corp <- readRDS("data_corpus_guardian2016-10k.rds")
#' corp <- corpus_reshape(data_corpus_guardian, 'sentences')
#' toks <- tokens(corp, remove_punct = TRUE)
#' toks <- tokens_remove(toks, stopwords())
#'
#' # economy keywords
#' eco <- char_keyness(toks, 'econom*')
#' head(eco, 20)
#'
#' # politics keywords
#' pol <- char_keyness(toks, 'politi*')
#' head(pol, 20)
#' }
char_keyness <- function(x, pattern, valuetype = c("glob", "regex", "fixed"),
                         case_insensitive = TRUE, window = 10, p = 0.001, min_count = 10,
                         remove_pattern = TRUE, ...) {
    if (!is.tokens(x))
        stop("x must be a tokens object\n", call. = FALSE)

    # reference
    ref <- dfm(tokens_remove(x, pattern, valuetype = valuetype,
                             case_insensitive = case_insensitive,
                             window = window), remove = "")

    # target
    x <- tokens_select(x, pattern, valuetype = valuetype,
                       case_insensitive = case_insensitive,
                       window = window)
    if (remove_pattern)
        x <- tokens_remove(x, pattern, valuetype = valuetype,
                           case_insensitive = case_insensitive)

    tar <- dfm(x, remove = "")
    if (nfeat(tar) == 0)
        stop(paste(unlist(pattern), collapse = ", "), " is not found\n", call. = FALSE)
    tar <- dfm_trim(tar, min_termfreq = min_count)
    if (nfeat(tar) == 0)
        return(character())
    ref <- dfm_match(ref, featnames(tar))
    key <- textstat_keyness(rbind(tar, ref), target = seq_len(ndoc(tar)), ...)
    key <- key[key$p < p,]
    return(key$feature)
}

#' Seed words for Latent Semantci Analysis
#'
#' @param type type of seed words currently only for sentiment (`sentiment`)
#'   or political ideology (`ideology`).
#' @export
#' @examples
#' seedwords('sentiment')
#' @references Turney, P. D., & Littman, M. L. (2003). Measuring Praise and
#'   Criticism: Inference of Semantic Orientation from Association. ACM Trans.
#'   Inf. Syst., 21(4), 315–346. https://doi.org/10.1145/944012.944013
seedwords <- function(type) {

    if (type == "pos-neg" || type == "sentiment") {
        seeds <- c(rep(1, 7), rep(-1, 7))
        names(seeds) <- c("good", "nice", "excellent", "positive", "fortunate", "correct", "superior",
                          "bad", "nasty", "poor", "negative", "unfortunate", "wrong", "inferior")
    } else if (type == "left-right" || type == "ideology") {
        seeds <- c(rep(1, 7), rep(-1, 7))
        names(seeds) <- c("deficit", "austerity", "unstable", "recession", "inflation", "currency", "workforce",
                          "poor", "poverty", "free", "benefits", "prices", "money", "workers")
    } else {
        stop(type, "is not currently available", call. = FALSE)
    }
    return(seeds)
}

#' Create a dummy textmodel_lss object from numeric vector
#' @param x named numeric vector
#' @keywords internal
#' @export
as.textmodel_lss <- function(x) {

    stopifnot(is.numeric(x))
    stopifnot(!is.null(names(x)))

    result <- list(beta = x,
                   data = NULL,
                   terms = names(x),
                   seeds = character(),
                   call = match.call())
    class(result) <- "textmodel_lss"
    return(result)
}

#' Smooth predicted LSS scores by local polynomial regression
#'
#' @param x a `data.frame` containing variables for LSS scores and dates
#' @param lss_var the name of the column for LSS scores
#' @param date_var the name of the columns for dates
#' @param span determines the level of smoothing
#' @param from start of the time period
#' @param to end of the time period
#' @param ... extra arguments passed to [loess()]
#' @export
#' @import stats
smooth_lss <- function(x, lss_var = "fit", date_var = "date", span = 0.1,
                       from = NULL, to = NULL, ...) {

    if (lss_var %in% names(x)) {
        if (!identical(class(x[[lss_var]]), "numeric"))
            stop(lss_var, " must be a numeric column")
    } else {
        stop(lss_var, " does not exist in x")
    }

    if (date_var %in% names(x)) {
        if (!identical(class(x[[date_var]]), "Date"))
            stop(date_var, " must be a date column")
    } else {
        stop(date_var, " does not exist in x")
    }

    x$lss <- x[[lss_var]]
    x$date <- x[[date_var]]
    if (is.null(from))
        from <- min(x$date)
    if (is.null(to))
        to <- max(x$date)
    x$time <- as.numeric(difftime(x$date, from, units = "days"))
    dummy <- data.frame(date = seq(from, to, '1 day'))
    dummy$time <- as.numeric(difftime(dummy$date, from, units = "days"))
    dummy$fit <- NA
    suppressWarnings(
        temp <- predict(loess(lss ~ time, data = x, span = span, ...),
                        newdata = dummy, se = TRUE)
    )
    result <- cbind(dummy[c("date", "time")], temp[c("fit", "se.fit")])
    return(result)
}
