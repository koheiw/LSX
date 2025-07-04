#' Fit a Latent Semantic Scaling model
#'
#' Latent Semantic Scaling (LSS) is a semi-supervised algorithm for document scaling based on
#' word embedding.
#' @param x a dfm or fcm created by [quanteda::dfm()], [quanteda::fcm()],
#'   [quanteda::tokens] or [quanteda::tokens_xptr] object.
#' @param k the size of word vectors.
#' @param seeds a character vector or named numeric vector that contains seed
#'   words. If seed words contain "*", they are interpreted as glob patterns.
#'   See [quanteda::valuetype].
#' @param terms a character vector or named numeric vector that specify words
#'   for which polarity scores will be computed; if a numeric vector, words' polarity
#'   scores will be weighted accordingly; if `NULL`, all the features in `x` except
#'   those less frequent than `min_count` will be used.
#' @param weight weighting scheme passed to [quanteda::dfm_weight()]. Ignored
#'   when `engine = "rsparse"`.
#' @param simil_method specifies method to compute similarity between features.
#'   The value is passed to [quanteda.textstats::textstat_simil()], "cosine" is
#'   used otherwise.
#' @param cache if `TRUE`, save the result of SVD for next execution with identical
#'   `x` and settings. Use the `base::options(lss_cache_dir)` to change the
#'   location cache files to be save.
#' @param tolower if `TRUE`, lower-case all the words in the model.
#' @param engine select the engine to factorize `x` to generate word vectors.
#'    If `x` is a dfm, [RSpectra::svds()], [irlba::irlba()] or [rsvd::rsvd()].
#'    If `x` is a fcm, [rsparse::GloVe()].
#'    If `x` is a tokens (or tokens_xptr), [wordvector::textmodel_word2vec()].
#' @param auto_weight automatically determine weights to approximate the
#'   polarity of terms to seed words. Deprecated.
#' @param verbose show messages if `TRUE`.
#' @param ... additional arguments passed to the underlying engine.
#' @export
#' @details Latent Semantic Scaling (LSS) is a semisupervised document scaling
#'   method. `textmodel_lss()` constructs word vectors from use-provided
#'   documents (`x`) and weights words (`terms`) based on their semantic
#'   proximity to seed words (`seeds`). Seed words are any known polarity words
#'   (e.g. sentiment words) that users should manually choose. The required
#'   number of seed words are usually 5 to 10 for each end of the scale.
#'
#'   If `seeds` is a named numeric vector with positive and negative values, a
#'   bipolar model is construct; if `seeds` is a character vector, a
#'   unipolar model. Usually bipolar models perform better in document
#'   scaling because both ends of the scale are defined by the user.
#'
#'   A seed word's polarity score computed by `textmodel_lss()` tends to diverge
#'   from its original score given by the user because it's score is affected
#'   not only by its original score but also by the original scores of all other
#'   seed words. If `auto_weight = TRUE`, the original scores are weighted
#'   automatically using [stats::optim()] to minimize the squared difference
#'   between seed words' computed and original scores. Weighted scores are saved
#'   in `seed_weighted` in the object.
#'
#'   When `x` is a tokens or tokens_xptr object, [wordvector::textmodel_word2vec]
#'   is called internally with `type = "skip-gram"` and other arguments passed via `...`.
#'   If `spatial = TRUE`, it return a spatial model; otherwise a probabilistic model.
#'   While the polarity scores of words are their cosine similarity to seed words in
#'   spatial models, they are predicted probability that the seed words to occur in
#'   their contexts. The probabilistic models are still experimental, so use them with caution.
#'
#'   Please visit the [package website](https://koheiw.github.io/LSX/) for examples.
#' @references Watanabe, Kohei. 2020. "Latent Semantic Scaling: A Semisupervised
#'   Text Analysis Technique for New Domains and Languages", Communication
#'   Methods and Measures. \doi{10.1080/19312458.2020.1832976}.
#'
#'   Watanabe, Kohei. 2017. "Measuring News Bias: Russia's Official News Agency
#'   ITAR-TASS' Coverage of the Ukraine Crisis" European Journal of
#'   Communication. \doi{10.1177/0267323117695735}.
#' @export
textmodel_lss <- function(x, ...) {
    UseMethod("textmodel_lss")
}

#' @rdname textmodel_lss
#' @param k the number of singular values requested to the SVD engine. Only used
#'   when `x` is a `dfm`.
#' @param slice a number or indices of the components of word vectors used to
#'   compute similarity; `slice < k` to further truncate word vectors; useful
#'   for diagnosys and simulation.
#' @param include_data if `TRUE`, the fitted model includes the dfm supplied as `x`.
#' @param group_data if `TRUE`, apply `dfm_group(x)` before saving the dfm.
#' @method textmodel_lss dfm
#' @importFrom quanteda featnames meta check_integer dfm_group
#' @importFrom Matrix colSums
#' @export
textmodel_lss.dfm <- function(x, seeds, terms = NULL, k = 300, slice = NULL,
                              weight = "count", cache = FALSE,
                              simil_method = "cosine",
                              engine = c("RSpectra", "irlba", "rsvd"),
                              auto_weight = FALSE,
                              include_data = FALSE,
                              group_data = FALSE,
                              verbose = FALSE, ...) {

    args <- list(terms = terms, seeds = seeds, ...)
    if ("features" %in% names(args)) {
        .Deprecated(msg = "'features' is deprecated; use 'terms'\n")
        terms <- args$terms <- args$features
    }

    k <- check_integer(k, min_len = 1, max_len = 1, min = 2, max = nrow(x))
    engine <- match.arg(engine)
    seeds <- expand_seeds(seeds, featnames(x), verbose)
    seed <- unlist(unname(seeds))
    theta <- get_theta(terms, featnames(x))
    feat <- union(names(theta), names(seed))

    if (engine %in% c("RSpectra", "irlba", "rsvd")) {
        if (verbose)
            cat(sprintf("Performing SVD by %s...\n", engine))
        svd <- cache_svd(x, k, weight, engine, cache, ...)
        embed <- t(svd$v)
        colnames(embed) <- featnames(x)
        embed <- embed[,feat, drop = FALSE]
    }

    if (is.null(slice)) {
        slice <- k
    } else {
        slice <- check_integer(slice, min_len = 1, max_len = k, min = 1, max = k)
    }
    if (length(slice) == 1)
        slice <- seq_len(slice)

    simil <- get_simil(embed, names(seed), names(theta), slice, simil_method)
    if (auto_weight)
        seed <- optimize_weight(seed, simil, verbose)

    beta <- get_beta(simil, seed) * theta

    result <- build_lss(
        beta = beta,
        k = k,
        slice = slice,
        frequency = colSums(x)[names(beta)],
        terms = args$terms,
        seeds = args$seeds,
        seeds_weighted = seed,
        embedding = embed,
        similarity = simil$seed,
        concatenator = meta(x, field = "concatenator", type = "object"),
        type = "svd",
        call = try(match.call(sys.function(-1), call = sys.call(-1)), silent = TRUE),
        version = utils::packageVersion("LSX")
    )
    if (include_data) {
        if (group_data) {
            result$data <- dfm_group(x)
        } else {
            result$data <- x
        }
    } else {
        if (group_data)
            warning("group_data is ignored when include_data = FALSE", call. = FALSE)
    }
    class(result) <- "textmodel_lss"
    return(result)
}

#' @rdname textmodel_lss
#' @param max_count passed to `x_max` in `rsparse::GloVe$new()` where cooccurrence
#'   counts are ceiled to this threshold. It should be changed according to the
#'   size of the corpus. Used only when `x` is a `fcm`.
#' @method textmodel_lss fcm
#' @importFrom quanteda featnames
#' @export
textmodel_lss.fcm <- function(x, seeds, terms = NULL, k = 50,
                              max_count = 10,
                              weight = "count", cache = FALSE,
                              simil_method = "cosine",
                              engine = "rsparse",
                              auto_weight = FALSE,
                              verbose = FALSE, ...) {

    args <- list(terms = terms, seeds = seeds, ...)
    if ("features" %in% names(args)) {
        .Deprecated(msg = "'features' is deprecated; use 'terms'.\n")
        terms <- args$terms <- args$features
    }
    if (engine == "text2vec") {
        .Deprecated(msg = "GloVe engine has been moved to from text2vec to rsparse.\n")
        engine <- "rsparse"
    }
    if ("w" %in% names(args)) {
        .Deprecated(msg = "'w' is deprecated; use 'k'.\n")
        k <- args$w
    }

    seeds <- expand_seeds(seeds, featnames(x), verbose)
    seed <- unlist(unname(seeds))
    term <- expand_terms(terms, featnames(x))
    feat <- union(term, names(seed))

    if (engine == "rsparse") {
        if (verbose)
            cat("Fitting GloVe model by rsparse...\n")
        embed <- (function(x, k, max_count, cache, features, w, ...) {
            cache_glove(x, w = k, x_max = max_count, cache = cache, ...)
        })(x, k, max_count, cache, ...) # trap old argument
        embed <- embed[,feat, drop = FALSE]
    }

    simil <- get_simil(embed, names(seed), term, seq_len(k), simil_method)
    if (auto_weight)
        seed <- optimize_weight(seed, simil, verbose)

    beta <- get_beta(simil, seed)

    result <- build_lss(
        beta = beta,
        k = k,
        frequency = x@meta$object$margin[names(beta)],
        terms = args$terms,
        seeds = args$seeds,
        seeds_weighted = seed,
        embedding = embed,
        similarity = simil$seed,
        concatenator = meta(x, field = "concatenator", type = "object"),
        type = "glove",
        call = try(match.call(sys.function(-1), call = sys.call(-1)), silent = TRUE),
        version = utils::packageVersion("LSX")
    )
    class(result) <- "textmodel_lss"
    return(result)
}

build_lss <- function(...) {

    args <- list(...)
    result <- list(
        data = NULL,
        beta = NULL,
        beta_type = "similarity",
        k = NULL,
        slice = NULL,
        frequency = NULL,
        terms = NULL,
        seeds = NULL,
        seeds_weighted = NULL,
        embedding = NULL,
        similarity = NULL,
        concatenator = "_",
        type = NULL,
        spatial = TRUE,
        call = NULL,
        version = utils::packageVersion("LSX")
    )
    for (m in intersect(names(result), names(args))) {
        result[m] <- args[m]
    }
    class(result) <- "textmodel_lss"
    return(result)
}

expand_terms <- function(terms, features) {
    if (is.null(terms)) {
        result <- setdiff(features, "")
    } else {
        temp <- quanteda::pattern2fixed(terms, features, valuetype = "glob", case_insensitive = FALSE)
        result <- unlist(unname(temp))
    }
    return(result)
}

expand_seeds <- function(seeds, features, verbose = FALSE) {

    seeds <- get_seeds(seeds)
    seeds_weighted <- weight_seeds(seeds, features)

    if (all(lengths(seeds_weighted) == 0))
        stop("No seed word is found in the dfm", call. = FALSE)

    if (verbose)
        cat(sprintf("Calculating term-term similarity to %d seed words...\n",
            sum(lengths(seeds_weighted))))

    return(seeds_weighted)
}

get_simil <- function(embed, seed, term, slice, method = "cosine") {
    embed <- as(embed, "dgCMatrix")
    simil <- as.matrix(proxyC::simil(embed[slice,,drop = FALSE],
                                     embed[slice,seed,drop = FALSE],
                                     margin = 2, method = method))
    list("terms" = simil[term,, drop = FALSE],
         "seeds" = simil[seed, seed, drop = FALSE])
}

get_beta <- function(simil, seed) {
    if (!identical(colnames(simil$terms), names(seed)))
        stop("Columns and seed words do not match", call. = FALSE)
    Matrix::rowSums(simil$terms %*% seed)
}

get_theta <- function(terms, feature) {
    if (is.numeric(terms)) {
        if (is.null(names(terms)))
            stop("terms must be named", call. = FALSE)
        if (any(terms < 0) || any(is.na(terms)))
            stop("terms must be positive values without NA", call. = FALSE)
        theta <- terms
    } else {
        terms <- expand_terms(terms, feature)
        theta <- rep(1.0, length(terms))
        names(theta) <- terms
    }
    return(theta)
}

cache_svd <- function(x, k, weight, engine, cache = TRUE, ...) {

    x <- quanteda::dfm_weight(x, scheme = weight)
    hash <- digest::digest(list(as(x, "dgCMatrix"), k, engine, ...,
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
        if (engine == "rsvd") {
            if (!requireNamespace("rsvd"))
                stop("wordvector package must be installed")
            result <- rsvd::rsvd(as(x, "dgCMatrix"), k = k, nu = 0, nv = k, ...)
        } else if (engine == "irlba") {
            if (!requireNamespace("irlba"))
                stop("irlba package must be installed")
            result <- irlba::irlba(as(x, "dgCMatrix"), nv = k, right_only = TRUE, ...)
        } else {
            result <- RSpectra::svds(as(x, "dgCMatrix"), k = k, nu = 0, nv = k, ...)
        }
        if (cache) {
            message("Writing cache file: ", file_cache)
            saveRDS(result, file_cache)
        }
    }
    return(result)
}

cache_glove <- function(x, w, x_max = 10, n_iter = 10, cache = TRUE, ...) {

    hash <- digest::digest(list(as(x, "dgCMatrix"), w, x_max, n_iter, ...,
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
        if (!requireNamespace("rsparse"))
            stop("wordvector package must be installed")
        utils::capture.output({
            glove <- rsparse::GloVe$new(rank = w, x_max = x_max, ...)
            temp <- glove$fit_transform(Matrix::drop0(x), n_iter = n_iter,
                                        n_threads = getOption("quanteda_threads", 1L))
        })
        result <- t(temp)
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
#' @method summary textmodel_lss
summary.textmodel_lss <- function(object, n = 30L, ...) {
    result <- list(
        "call" = object$call,
        "seeds" = object$seeds,
        "beta" = as.coefficients_textmodel(head(coef(object), n))
    )
    if (!is.null(object$data))
        result$data.dimension <- dim(object$data)
    as.summary.textmodel(result)
}

#' Extract model coefficients from a fitted textmodel_lss object
#'
#' `coef()` extract model coefficients from a fitted `textmodel_lss`
#' object.  `coefficients()` is an alias.
#' @param object a fitted [textmodel_lss] object.
#' @param ... not used.
#' @export
coef.textmodel_lss <- function(object, ...) {
    sort(object$beta, decreasing = TRUE)
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
        s <- unlist(quanteda::pattern2fixed(x, type, "glob", FALSE))
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

# automatically align polarity score with original weight
optimize_weight <- function(seed, simil, verbose) {
    .Deprecated(old = "auto_weight")
    if (verbose)
        cat("Optimizing seed weights...\n")
    result <- optim(seed, function(x) {
        sum((rowSums(simil$seeds %*% x) - seed) ^ 2)
    })
    return(result$par)
}


