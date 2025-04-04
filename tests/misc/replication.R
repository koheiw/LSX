require(quanteda)
require(wordvector)
require(LSX)

as.textmodel_lss2 <- function(x, seeds) {

  prob <- probability(x, names(seeds), "values")
  seeds <- seeds[names(seeds) %in% rownames(prob)]
  res <- list(model = prob,
              seeds = seeds,
              beta = rowMeans(prob %*% diag(seeds)),
              frequency = x$frequency)
  class(res) <- "textmodel_lss"
  return(res)
}

toks <- tokens(data_corpus_news2014) %>%
  tokens_tolower()

seed <- as.seedwords(data_dictionary_sentiment)
wdv <- textmodel_word2vec(toks, dim = 100, type = "skip-gram", normalize = FALSE,
                          verbpse = TRUE)

lss0 <- as.textmodel_lss2(wdv, seed)
lss <- as.textmodel_lss(wdv, seed, spatial = FALSE)

setequal(names(lss$beta), names(lss0$beta))
cor(coef(lss0), coef(lss))

