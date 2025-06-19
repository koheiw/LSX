#' \[experimental\] Fit a Probabilistic Latent Semantic Scaling model
#'
#' Probabilistic Latent Semantic Scaling (PLSS) is a semi-supervised algorithm
#' document scaling based on language models.
#' @param x a [quanteda::tokens] or [quanteda::tokens_xptr] object.
#' @param engine currently only supports the word2vec model with the skip-gram algorithm.
#' @export
#' @inheritParams textmodel_lss
textmodel_plss <- function(x, seeds, terms = NULL, k = 200,
                          engine = c("word2vec"),
                          tolower = TRUE,
                          include_data = FALSE,
                          group_data = FALSE,
                          verbose = FALSE, ...) {
  UseMethod("textmodel_plss")
}


#' @export
#' @importFrom quanteda dfm dfm_group
textmodel_plss.tokens <- function(x, seeds, terms = NULL, k = 200,
                                 engine = c("word2vec"),
                                 tolower = TRUE,
                                 include_data = FALSE,
                                 group_data = FALSE,
                                 verbose = FALSE, ...) {


  k <- check_integer(k, min = 2)
  engine <- match.arg(engine)

  if (!requireNamespace("wordvector"))
    stop("wordvector package must be installed")
  if (utils::packageVersion("wordvector") < as.numeric_version("0.5.0"))
    stop("wordvector package must be v0.5.0 or later")

  w2v <- wordvector::textmodel_word2vec(x, dim = k, type = "skip-gram", tolower = tolower,
                                        normalize = FALSE, verbose = verbose, ...)
  result <- as.textmodel_lss(w2v, seeds = seeds, terms = terms, verbose = FALSE)
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
