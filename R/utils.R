#' Identify noisy documents in a corpus
#' @param x character or [quanteda::corpus()] object whose texts will be diagnosed.
#' @param ... extra arguments passed to [quanteda::tokens()].
#' @keywords internal
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

    .Deprecated("quanteda.textstats::textstat_summary()")
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
    result$dupli <- duplicated(as.character(x))
    result$noise <- result$any / result$n_token
    return(result)
}

#' Computes cohesion of components of latent semantic analysis
#' @param x a fitted `textmodel_lss`
#' @param bandwidth size of window for smoothing
#' @keywords internal
#' @export
#' @importFrom Matrix rowMeans rowSums tcrossprod tril
cohesion <- function(x, bandwidth = 10) {
    if (!"textmodel_lss" %in% class(x))
        stop("x must be a textmodel_lss object")
    seed <- names(x$seeds_weighted)
    embed <- as(x$embedding, "dgCMatrix")
    cross <- tcrossprod(embed[,seed, drop = FALSE])
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

#' Convert a list or a dictionary to seed words
#' @param x a list of characters vectors or a [dictionary][quanteda::dictionary] object.
#' @param upper numeric index or key for seed words for higher scores.
#' @param lower numeric index or key for seed words for lower scores.
#' @param concatenator character to replace separators of multi-word seed words.
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


#' Smooth predicted polarity scores
#'
#' Smooth predicted polarity scores by local polynomial regression.
#' @param x a [data.frame] containing polarity scores and dates.
#' @param lss_var the name of the column in `x` for polarity scores.
#' @param date_var the name of the column in `x` for dates.
#' @param span the level of smoothing.
#' @param groups specify the columns in `x` to smooth separately
#'   by the group; the columns must be factor, character or logical.
#' @param from,to,by the the range and the internal of the smoothed scores;
#'   passed to [seq.Date].
#' @param engine specifies the function to be used for smoothing.
#' @param ... additional arguments passed to the smoothing function.
#' @details Smoothing is performed using [stats::loess()] or [locfit::locfit()].
#'   When the `x` has more than 10000 rows, it is usually better to choose
#'   the latter by setting `engine = "locfit"`. In this case, `span` is passed to
#'   `locfit::lp(nn = span)`.
#'
#' @export
#' @import stats locfit
#' @importFrom quanteda check_character
smooth_lss <- function(x, lss_var = "fit", date_var = "date",
                       span = 0.1, groups = NULL,
                       from = NULL, to = NULL, by = 'day',
                       engine = c("loess", "locfit"), ...) {

  lss_var <- check_character(lss_var)
  date_var <- check_character(date_var)
  groups <- check_character(groups, max_len = 5, allow_null = TRUE)
  engine <- match.arg(engine)

  if (!lss_var %in% names(x)) {
    stop(lss_var, " does not exist in x")
  } else {
    x$lss <- x[[lss_var]]
    if (!identical(class(x$lss), "numeric"))
      stop("lss_var must be a numeric column")
  }

  if (!date_var %in% names(x)) {
    stop(date_var, " does not exist in x")
  } else {
    x$date <- x[[date_var]]
    if (!identical(class(x$date), "Date"))
      stop("date_var must be a date column")
  }

  if (is.null(from)) from <- min(x$date)
  if (is.null(to)) to <- max(x$date)
  x$time <- as.numeric(difftime(x$date, from, units = "days"))
  dummy <- data.frame(date = seq(from, to, by))
  dummy$time <- as.numeric(difftime(dummy$date, from, units = "days"))
  dummy$fit <- rep(NA_real_, nrow(dummy))

  if (!is.null(groups)) {
    b <- !groups %in% names(x)
    if (any(b))
      stop(groups[b], " does not exist in x")

    if (any(sapply(x[groups], function(y) is.numeric(y))))
      stop("columns for grouping cannot be numeric")

    x[groups] <- droplevels(x[groups])
    lis <- split(x, x[groups])
    lis <- lapply(lis, function(y) {
      # NOTE: unnecessary thakns to droplevels
      #if (nrow(y) == 0)
      #  return(NULL)
      temp <- smooth_data(y, dummy, span, engine, ...)
      temp[groups] <- as.data.frame(lapply(y[groups], function(z) {
        rep(head(z, 1), nrow(temp))
      }))
      return(temp)
    })
    result <- do.call(rbind, lis)
  } else {
    result <- smooth_data(x, dummy, span, engine, ...)
  }
  rownames(result) <- NULL
  return(result)
}

smooth_data <- function(x, dummy, span, engine, ...) {
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
  cbind(dummy[c("date", "time")], temp[c("fit", "se.fit")])
}

#' @export
#' @method print textmodel_lss
print.textmodel_lss <- function(x, ...) {
  cat("\nCall:\n")
  print(x$call)
  cat("\n")
}
