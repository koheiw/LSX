require(quanteda)
require(wordvector)

toks_test <- readRDS("../data/tokens_test.RDS")
feat_test <- head(char_context(toks_test, "america*", min_count = 1, p = 0.05), 100)
dfmt_test <- dfm(toks_test)
seed_test <- as.seedwords(data_dictionary_sentiment)

lss_test <- textmodel_lss(dfmt_test, seed_test, terms = feat_test, k = 300)
saveRDS(lss_test, "../data/lss_test.RDS")

w2v_test <- word2vec(head(toks, 10), min_count = 1)
saveRDS(w2v_test, "tests/data/word2vec_test.RDS")
