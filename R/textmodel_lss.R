

mt <- dfm(x, tolower = FALSE, remove = '')
mt <- dfm_trim(mt, min_count = 10)

textmodel_lss <- function(x, y, features = NULL, nv = 300, verbose = TRUE) {

    package <- match.arg(package)

    if (verbose)
        cat('Starting singular value decomposition of dfm...\n')
    s <- RSpectra::svds(x, k = 300, nu = 0, nv = nv, opts = list(tol = 1e-5))
    temp <- s$v * s$d
    rownames(temp) <- colnames(x)
    temp <- as.dfm(t(temp))

    if (is.character(y)) {
        seed <- y
        weight <- rep(1, length(y))
    } else {
        weight <- weight / length(weight)
        seed <- unlist(mapply(weight_seeds, seed, weight, MoreArgs = list(featnames(x)),
                              USE.NAMES = FALSE))
        seed <- names(y)
        weight <- unnname(y)
    }

    temp <- textstat_simil(temp, selection = names(seed), margin = 'features')
    model <- sort(rowMeans(temp %*% weight), decreasing = TRUE)

    result <- list(model = model, data = x, feature = colnames(model))
    class(result) <- "textmodel_lss_fitted"

    return(sort(rowMeans(sim), decreasing = TRUE))
}


#' internal function to generate equally-weighted seed set
#'
#' @keyword internal
weight_seeds <- function(x, type) {
    seed <- names(x)
    weight <- unnname(x)
    s <- unlist(quanteda:::regex2fixed(seed, type, 'glob', FALSE))
    v <- rep(weight / length(s), length(s))
    names(v) <- s
    return(v)
}

preidict.textmodel_lss_fitted <- function(mx, words, score_only = TRUE){

    mx_word <- Matrix::as.matrix(words)
    common <- intersect(rownames(mx_word), colnames(mx))
    mx <- mx[,match(common, colnames(mx))] # ignore words not in the model
    mx_word <- mx_word[match(common, rownames(mx_word)),,drop=FALSE] # ignore words not in documents
    mx_tf <- quanteda::dfm_weight(mx, "relFreq")
    mn <- mx_tf %*% mx_word # mean scores of documents

    if(score_only){
        return(as.vector(mn))
    }else{
        #mx_bin <- mx > 0
        mx_bin <- as(mx, 'nMatrix')
        mx_scr <- mx_bin * t(mx_word[, rep(1, nrow(mx_tf))]) # repeat the dictionary
        mx_dev <- mx_scr - mn[, rep(1, ncol(mx_bin))] # difference from the mean
        mx_err <- (mx_dev ** 2) * mx_tf # square of deviation weighted by frequency
        var <- Matrix::rowSums(mx_err) # variances
        sd <- sqrt(var) # standard diviaitons
        se <- sd / sqrt(Matrix::rowSums(mx)) # SD divided by sqrt of total number of words
        return(data.frame(lss_mn=mn[,1], lss_se=se, lss_n=Matrix::rowSums(mx_bin)))
    }
}

get_keywords <- function(x, pattern) {
    mt <- dfm(tokens_keep(x, pattern, window = 10))
    mt <- dfm_trim(mt, min_count = 10)
    mt <- dfm_remove(mt, pattern)
    mt_not <- dfm(tokens_remove(x, pattern, window = 10))
    rownames(textstat_keyness(rbind(mt, mt_not), seq_len(ndoc(mt))))
}


