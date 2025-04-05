require(quanteda)
require(wordvector)

toks <- readRDS("tests/data/tokens.RDS") %>%
  tokens_remove(stopwords("en"), min_nchar = 2) %>%
  tokens_tolower()

feat <- head(char_context(toks, "america*", min_count = 1, p = 0.05), 100)
dfmt <- dfm(toks)
seed <- as.seedwords(data_dictionary_sentiment)

lss <- textmodel_lss(dfmt, seed, terms = feat, k = 300)
saveRDS(lss, "tests/data/lss_k300.RDS")

wdv <- textmodel_word2vec(head(toks, 10), min_count = 1)
saveRDS(wdv, "tests/data/word2vec.RDS")

wdv2 <- textmodel_word2vec(head(toks, 10), min_count = 1, normalize = FALSE)
saveRDS(wdv2, "tests/data/word2vec-prob.RDS")
