require(quanteda)
require(wordvector)

toks_test <- readRDS("tests/data/tokens_test.RDS") %>%
  tokens_tolower()
feat_test <- head(char_context(toks_test, "america*", min_count = 1, p = 0.05), 100)
dfmt_test <- dfm(toks_test)
seed_test <- as.seedwords(data_dictionary_sentiment)

lss_test <- textmodel_lss(dfmt_test, seed_test, terms = feat_test, k = 300)
saveRDS(lss_test, "tests/data/lss_test.RDS")

wdv_test <- textmodel_word2vec(head(toks_test, 10), min_count = 1)
saveRDS(wdv_test, "tests/data/word2vec_test.RDS")

wdv_test2 <- textmodel_word2vec(head(toks_test, 10), min_count = 1, normalize = FALSE)
saveRDS(wdv_test2, "tests/data/word2vec-prob_test.RDS")
