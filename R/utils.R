#' Identify noisy documents in a corpus
#' @param x character or [corpus] object whose texts will be diagnosed
#' @param ... extra arguments passed to `tokens`
#' @export
#' @importFrom quanteda corpus tokens texts
diagnosys <- function(x, ...) {
    UseMethod("diagnosys")
}

#' @export
diagnosys.character <- function(x, ...) {
    diagnosys(corpus(x), ...)
}

#' @export
#' @importFrom quanteda dictionary ntoken dfm convert dfm_lookup
diagnosys.corpus <- function(x, ...) {

    .Deprecated("quanteda.textstats::textstat_summary")
    dict <- dictionary(list(
        "number" = "\\p{N}",
        "punct" = "\\p{P}",
        "symbol" = "\\p{S}",
        "any" = "[\\p{N}\\p{P}\\p{S}]"
    ))

    n_sent <- ntoken(tokens(x, what = "sentence"))
    temp <- dfm(tokens(x, ...))
    n_token <- ntoken(temp)
    result <- convert(
        dfm_lookup(temp, dictionary = dict, valuetype = "regex"),
        "data.frame"
    )
    result$n_sent <- n_sent
    result$n_token <- n_token
    result$dupli <- duplicated(texts(x))
    result$noise <- result$any / result$n_token
    return(result)
}

#' Computes cohesion of components of latent semantic analysis
#' @param object a fitted `textmodel_lss`
#' @param bandwidth size of window for smoothing
#' @export
#' @importFrom Matrix rowMeans rowSums tcrossprod tril
cohesion <- function(object, bandwidth = 10) {
    stopifnot("textmodel_lss" %in% class(object))
    seed <- unlist(unname(object$seeds_weighted))
    embed <- as(object$embedding, "dgCMatrix")
    cross <- tcrossprod(embed[,names(seed), drop = FALSE])
    cross <- tril(cross, -1)
    n <- seq_len(nrow(cross))
    h <- rowSums(abs(cross)) / (n - 1)
    h[1] <- NA # no similarity for k = 1
    result <- data.frame(k = n, raw = log(h))
    result$smoothed <- stats::ksmooth(
      result$k, result$raw, kernel = "normal",
      bandwidth = bandwidth)$y
    return(result)
}

#' Experimental function to test quality of seed words
#' @param object a fitted `textmodel_lss`
#' @export
#' @keywords internal
#' @importFrom Matrix rowMeans colMeans diag band
strength <- function(object) {
    stopifnot("textmodel_lss" %in% class(object))
    term <- object$terms
    seed <- unlist(unname(object$seeds_weighted))
    embed <- as(object$embedding, "dgCMatrix")
    sim <- proxyC::simil(embed[object$slice,, drop = FALSE],
                         embed[object$slice, names(seed), drop = FALSE], margin = 2)
    diag(sim) <- NA
    temp <- data.frame(seed = colnames(sim),
                       selected = log(1 / abs(colMeans(sim[term,,drop = FALSE], na.rm = TRUE))),
                       all = log(1 / abs(colMeans(sim, na.rm = TRUE))),
                       stringsAsFactors = FALSE)
    sim_mean <- rowMeans(sim, na.rm = TRUE)
    temp <- temp[order(temp$selected, decreasing = TRUE),, drop = FALSE]
    rownames(temp) <- NULL
    result <- list(
      "overall" = c("selected" =  log(1 / mean(abs(sim_mean[term]))),
                    "all" = log(1 / mean(abs(sim_mean)))
      ),
      "element" = temp
    )
    class(result) <- "listof"
    return(result)
}

#' Convenient function to convert a list to seed words
#' @param x a list of characters vectors or a [dictionary][quanteda::dictionary] object
#' @param upper numeric index or key for seed words for higher scores
#' @param lower numeric index or key for seed words for lower scores
#' @param concatenator character to replace separators of multi-word seed words
#' @export
#' @return named numeric vector for seed words with polarity scores
as.seedwords <- function(x, upper = 1, lower = 2, concatenator = "_") {
    if (!"list" %in% class(x) && !quanteda::is.dictionary(x))
        stop("x must be a list or dictionary object")
    if (quanteda::is.dictionary(x)){
        x <- quanteda::as.dictionary(x) # upgrade object
        separator <- x@meta$object$separator
        x <- quanteda::as.list(x)
    } else {
        separator <- " "
    }
    pos <- neg <- character()
    if (!is.null(upper))
        pos <- as.character(unique(unlist(x[upper], use.names = FALSE)))
    if (!is.null(lower))
        neg <- as.character(unique(unlist(x[lower], use.names = FALSE)))
    result <- rep(c(1, -1), c(length(pos), length(neg)))
    names(result) <- stringi::stri_replace_all_fixed(c(pos, neg), separator, concatenator)
    return(result)
}

unused_dots <- function(...) {
    arg <- names(list(...))
    if (length(arg) == 1) {
        warning(arg[1], " argument is not used.\n", call. = FALSE)
    } else if (length(arg) > 1) {
        warning(paste0(arg, collapse = ", "), " arguments are not used.\n", call. = FALSE)
    }
}


#' Seed words for Latent Semantic Analysis
#'
#' @param type type of seed words currently only for sentiment (`sentiment`)
#'   or political ideology (`ideology`).
#' @export
#' @examples
#' seedwords('sentiment')
#' @references Turney, P. D., & Littman, M. L. (2003). Measuring Praise and
#'   Criticism: Inference of Semantic Orientation from Association. ACM Trans.
#'   Inf. Syst., 21(4), 315–346. \doi{10.1145/944012.944013}
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


#' Smooth predicted LSS scores by local polynomial regression
#'
#' @param x a `data.frame` containing LSS scores and dates
#' @param lss_var the name of the column for LSS scores
#' @param date_var the name of the columns for dates
#' @param span determines the level of smoothing.
#' @param from start of the time period
#' @param to end of the time period
#' @param engine specifies the function to smooth LSS scores: [loess()] or [locfit()].
#' The latter should be used when n > 10000.
#' @param ... extra arguments passed to [loess()] or [lp()]
#' @export
#' @import stats locfit
smooth_lss <- function(x, lss_var = "fit", date_var = "date", span = 0.1,
                       from = NULL, to = NULL, engine = c("loess", "locfit"), ...) {

  engine <- match.arg(engine)

  if (lss_var %in% names(x)) {
    if (!identical(class(x[[lss_var]]), "numeric"))
      stop("lss_var must be a numeric column")
  } else {
    stop(lss_var, " does not exist in x")
  }

  if (date_var %in% names(x)) {
    if (!identical(class(x[[date_var]]), "Date"))
      stop("date_var must be a date column")
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
  if (engine == "loess") {
    suppressWarnings(
      temp <- predict(loess(lss ~ time, data = x, span = span, ...),
                      newdata = dummy, se = TRUE)
    )
  } else {
    suppressWarnings(
      temp <- predict(locfit(lss ~ lp(time, nn = span, ...), data = x),
                      newdata = dummy, se = TRUE)
    )
  }
  result <- cbind(dummy[c("date", "time")], temp[c("fit", "se.fit")])
  return(result)
}

#' @export
#' @method print textmodel_lss
print.textmodel_lss <- function(x, ...) {
  cat("\nCall:\n")
  print(x$call)
  cat("\n")
  if (!is.null(x$seed)) {
    cat("Seeds words:\n", sep = "")
    print(x$seed)
  }
  if (!is.null(x$k)) {
    cat("Hyperparameters: ", sep = "")
    cat("k =", x$k, "\n")
  }
}
