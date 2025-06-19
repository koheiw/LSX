#' @rdname textmodel_lss
#' @param spatial \[experimental\] if `FALSE`, return a probabilistic model. See the details.
#' @export
#' @inheritParams wordvector::textmodel_word2vec
#' @importFrom quanteda dfm dfm_group
textmodel_lss.tokens <- function(x, seeds, terms = NULL, k = 200,
                                 min_count = 5,
                                 engine = "wordvector",
                                 tolower = TRUE,
                                 include_data = FALSE,
                                 group_data = FALSE,
                                 spatial = TRUE,
                                 verbose = FALSE, ...) {


  k <- check_integer(k, min = 2)
  engine <- match.arg(engine)

  if (!requireNamespace("wordvector"))
    stop("wordvector package must be installed")
  if (utils::packageVersion("wordvector") < as.numeric_version("0.5.0"))
    stop("wordvector package must be v0.5.0 or later")

  w2v <- wordvector::textmodel_word2vec(x, dim = k, min_count = min_count,
                                        type = "skip-gram", tolower = tolower,
                                        normalize = FALSE, verbose = verbose, ...)
  result <- as.textmodel_lss(w2v, seeds = seeds, terms = terms, spatial = spatial,
                             verbose = FALSE)
  result$type <- "word2vec"
  result$call <- try(match.call(sys.function(-1), call = sys.call(-1)), silent = TRUE)

  if (include_data) {
    x <- dfm(x, remove_padding = TRUE, tolower = tolower)
    if (group_data) {
      result$data <- dfm_group(x)
    } else {
      result$data <- x
    }
  } else {
    if (group_data)
      warning("group_data is ignored when include_data = FALSE", call. = FALSE)
  }
  return(result)
}
