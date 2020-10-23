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
#'   `x` and settings. Use the `base::options(lss_cache_dir)` to change the
#'   location cache files to be save.
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
#' \donttest{
#' require(quanteda)
#' con <- url("https://bit.ly/2GZwLcN", "rb")
#' corp <- readRDS(con)
#' close(con)
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
#' lss_svd <- textmodel_lss(dfmt, seed)
#' summary(lss_svd)
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
#' lss_glov <- textmodel_lss(fcmt, seed)
#' summary(lss_glov)
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
    args <- list(terms = terms, seeds = seeds, ...)
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
        embed <- t(svd$v)
        colnames(embed) <- featnames(x)
        import <- svd$d
    }
    if (is.null(slice))
        slice <- k

    k <- as.integer(k)
    slice <- as.integer(slice)
    if (any(slice < 1L) || any(k < slice))
        stop("'slice' must be between 1 and k")
    if (length(slice) == 1)
        slice <- seq_len(slice)
    simil <- get_simil(embed, seeds, terms, slice, simil_method)
    beta <- get_beta(simil$terms, seeds)
    result <- build_lss(
        beta = beta,
        k = k,
        slice = slice,
        frequency = colSums(x)[names(beta)],
        terms = args$terms,
        seeds = args$seeds,
        seeds_weighted = seeds,
        embedding = embed,
        similarity = simil$seed,
        importance = import,
        concatenator = meta(x, field = "concatenator", type = "object"),
        call = match.call()
    )
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
    args <- list(terms = terms, seeds = seeds, ...)
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
        embed <- cache_glove(x, w, cache, ...)
    }

    simil <- get_simil(embed, seeds, terms, seq_len(w), simil_method)
    beta <- get_beta(simil$terms, seeds)

    result <- build_lss(
        beta = beta,
        w = w,
        terms = args$terms,
        seeds = args$seeds,
        seeds_weighted = seeds,
        embedding = embed,
        similarity = simil$seed,
        call = match.call()
    )
    class(result) <- "textmodel_lss"
    return(result)
}

build_lss <- function(...) {

    args <- list(...)
    result <- list(
        data = NULL,
        beta = NULL,
        k = NULL,
        slice = NULL,
        frequency = NULL,
        terms = NULL,
        seeds = NULL,
        seeds_weighted = NULL,
        embedding = NULL,
        similarity = NULL,
        importance = NULL,
        concatenator = "_",
        dummy = FALSE,
        call = NULL
    )
    for (m in intersect(names(result), names(args))) {
        result[m] <- args[m]
    }
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
    seeds_weighted <- weight_seeds(seeds, features)

    if (all(lengths(seeds_weighted) == 0))
        stop("No seed word is found in the dfm", call. = FALSE)

    if (verbose)
        cat("Calculating term-term similarity to", sum(lengths(seeds_weighted)),
            "seed words...\n")

    return(seeds_weighted)
}

get_simil <- function(embed, seeds, terms, slice, method = "cosine") {
    seed <- unlist(unname(seeds))
    embed <- as(embed, "dgCMatrix")
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
                                utils::packageVersion("LSX")),
                           algo = "xxhash64")

    dir_cache <- getOption("lss_cache_dir", "lss_cache")
    if (cache && !dir.exists(dir_cache))
        dir.create(dir_cache)
    if (engine == "RSpectra") {
        file_cache <- paste0(dir_cache, "/svds_", hash, ".RDS")
    } else if (engine == "rsvd") {
        file_cache <- paste0(dir_cache, "/rsvd_", hash, ".RDS")
    } else {
        file_cache <- paste0(dir_cache, "/irlba_", hash, ".RDS")
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
                                utils::packageVersion("LSX")),
                           algo = "xxhash64")

    dir_cache <- getOption("lss_cache_dir", "lss_cache")
    if (cache && !dir.exists(dir_cache))
        dir.create(dir_cache)
    file_cache <- paste0(dir_cache, "/rsparse_", hash, ".RDS")

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
weight_seeds <- function(seeds, type) {
    seeds_fix <- lapply(names(seeds), function(x) {
        s <- unlist(pattern2fixed(x, type, "glob", FALSE))
        if (is.null(s))
            return(character())
        return(s)
    })
    weight <- 1 / table(seeds > 0)
    mapply(function(x, y) {
              if (!length(y))
                  return(numeric())
              v <- unname(x * weight[as.character(x > 0)]) / length(y)
              v <- rep(v, length(y))
              names(v) <- y
              return(v)
           }, seeds, seeds_fix, SIMPLIFY = FALSE)
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
#' @param remove_pattern if `TRUE`, keywords do not containe target words.
#' @param ... additional arguments passed to [textstat_keyness()].
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
    if (!is.tokens(x))
        stop("x must be a tokens object\n", call. = FALSE)

    # reference
    y <- tokens_remove(x, pattern, valuetype = valuetype,
                       case_insensitive = case_insensitive,
                       window = window, padding = FALSE)
    y <- dfm(y, remove = "")

    # target
    x <- tokens_select(x, pattern, valuetype = valuetype,
                       case_insensitive = case_insensitive,
                       window = window, padding = FALSE)
    if (remove_pattern)
        x <- tokens_remove(x, pattern, valuetype = valuetype,
                           case_insensitive = case_insensitive)
    x <- dfm(x, remove = "")

    x <- dfm_trim(x, min_termfreq = min_count)
    y <- dfm_match(y, featnames(x))
    if (sum(x) > 0) {
        result <- textstat_keyness(as.dfm(rbind(colSums(x), colSums(y))), ...)
    } else {
        result <- head(textstat_keyness(as.dfm(matrix(c(1, 0))), ...), 0) # dummy object
    }
    colnames(result)[c(4, 5)] <- c("n_inside", "n_outside")
    return(result)
}

#' @rdname textstat_context
#' @export
char_context <- function(x, ..., p = 0.001) {
    result <- textstat_context(x, ...)
    result <- result[result$p < p,]
    return(result$feature)
}

#' @rdname textstat_context
#' @export
char_keyness <- function(x, ..., p = 0.001) {
    char_context(x, ..., p = p)
}

