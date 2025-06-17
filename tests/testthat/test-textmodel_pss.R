require(quanteda)

# create and save test object
# corp_sent <- corpus_reshape(data_corpus_inaugural, "sentence")
# toks_test <- tokens(corp_sent, remove_punct = TRUE)
# saveRDS(toks_test, "tests/data/tokens.RDS")

toks_test <- readRDS("../data/tokens.RDS")
feat_test <- head(char_context(toks_test, "america*", min_count = 1, p = 0.05), 100)
seed <- as.seedwords(data_dictionary_sentiment)

test_that("textmodel_pss works", {

  skip_on_cran()

  # without data
  pss1 <- textmodel_pss(toks_test, seed, k = 10)

  expect_s3_class(pss1, "textmodel_lss")
  expect_equal(pss1$k, 10)
  expect_equal(names(pss1$beta),
               types(tokens_trim(tokens_tolower(toks_test), min_termfreq = 5)))
  expect_equal(pss1$concatenator, concatenator(toks_test))
  expect_equal(pss1$slice, NULL)
  expect_equal(pss1$embedding, NULL)
  expect_equal(pss1$data, NULL)
  expect_error(
    predict(pss1),
    "The model includes no data, use newdata to supply a dfm."
  )

  # with data
  pss2 <- textmodel_pss(toks_test, seed, k = 10, include_data = TRUE)

  expect_s3_class(pss2, "textmodel_lss")
  expect_equal(pss2$concatenator, concatenator(toks_test))
  expect_equal(docnames(pss2$data), docnames(toks_test))
  expect_equal(
    names(predict(pss2)),
    docnames(toks_test)
  )

  # with terms
  pss3 <- textmodel_pss(toks_test, seed, k = 10, terms = feat_test,
                        include_data = TRUE, group_data = TRUE)

  expect_s3_class(pss3, "textmodel_lss")
  expect_true(all(names(pss3$beta) %in% feat_test))
  expect_equal(
    names(predict(pss3)),
    docnames(tokens_group(toks_test))
  )

  # with tokens_xptr
  pss4 <- textmodel_pss(as.tokens_xptr(toks_test), seed, k = 10,
                        include_data = TRUE)

  expect_s3_class(pss4, "textmodel_lss")
  expect_equal(docnames(pss4$data), docnames(toks_test))

  # warning
  expect_warning(
    textmodel_pss(toks_test, seed, k = 10,
                  include_data = FALSE, group_data = TRUE),
    "group_data is ignored when include_data = FALSE"
  )

  # error
  expect_error(
    textmodel_pss(toks_test, k = -1),
    "The value of k must be between 2 and Inf"
  )
  expect_error(
    textmodel_pss(toks_test, k = c(10, 20)),
    "The length of k must be 1"
  )

})
