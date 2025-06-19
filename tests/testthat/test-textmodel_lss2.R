require(quanteda)

# create and save test object
# corp_sent <- corpus_reshape(data_corpus_inaugural, "sentence")
# toks_test <- tokens(corp_sent, remove_punct = TRUE)
# saveRDS(toks_test, "tests/data/tokens.RDS")

toks_test <- readRDS("../data/tokens.RDS")
feat_test <- head(char_context(toks_test, "america*", min_count = 1, p = 0.05), 100)
seed <- as.seedwords(data_dictionary_sentiment)

test_that("textmodel_lss works when spatial = TRUE", {

  skip_on_cran()

  # without data
  lss1 <- textmodel_lss(toks_test, seed, k = 10)

  expect_s3_class(lss1, "textmodel_lss")
  expect_equal(lss1$k, 10)
  expect_equal(names(lss1$beta),
               types(tokens_trim(tokens_tolower(toks_test), min_termfreq = 5)))
  expect_equal(lss1$concatenator, concatenator(toks_test))
  expect_equal(lss1$slice, 1:10)
  expect_equal(dim(lss1$embedding), c(10, length(lss1$beta)))
  expect_equal(lss1$data, NULL)
  expect_error(
    predict(lss1),
    "The model includes no data, use newdata to supply a dfm."
  )

  # with data
  lss2 <- textmodel_lss(toks_test, seed, k = 10, include_data = TRUE)

  expect_s3_class(lss2, "textmodel_lss")
  expect_equal(lss2$concatenator, concatenator(toks_test))
  expect_equal(docnames(lss2$data), docnames(toks_test))
  expect_equal(
    names(predict(lss2)),
    docnames(toks_test)
  )

  # with terms
  lss3 <- textmodel_lss(toks_test, seed, k = 10, terms = feat_test,
                         include_data = TRUE, group_data = TRUE)

  expect_s3_class(lss3, "textmodel_lss")
  expect_true(all(names(lss3$beta) %in% feat_test))
  expect_equal(
    names(predict(lss3)),
    docnames(tokens_group(toks_test))
  )

  # with tokens_xptr
  lss4 <- textmodel_lss(as.tokens_xptr(toks_test), seed, k = 10,
                         include_data = TRUE)

  expect_s3_class(lss4, "textmodel_lss")
  expect_equal(docnames(lss4$data), docnames(toks_test))

  # warning
  expect_warning(
    textmodel_lss(toks_test, seed, k = 10,
                   include_data = FALSE, group_data = TRUE),
    "group_data is ignored when include_data = FALSE"
  )

  # error
  expect_error(
    textmodel_lss(toks_test, k = -1),
    "The value of k must be between 2 and Inf"
  )
  expect_error(
    textmodel_lss(toks_test, k = c(10, 20)),
    "The length of k must be 1"
  )

})


test_that("textmodel_lss works when spatial = FALSE", {

  skip_on_cran()

  # without data
  lss1 <- textmodel_lss(toks_test, seed, k = 10, spatial = FALSE)

  expect_s3_class(lss1, "textmodel_lss")
  expect_equal(lss1$k, 10)
  expect_equal(names(lss1$beta),
               types(tokens_trim(tokens_tolower(toks_test), min_termfreq = 5)))
  expect_equal(lss1$concatenator, concatenator(toks_test))
  expect_equal(lss1$slice, NULL)
  expect_equal(lss1$embedding, NULL)
  expect_equal(lss1$data, NULL)
  expect_error(
    predict(lss1),
    "The model includes no data, use newdata to supply a dfm."
  )

  # with data
  lss2 <- textmodel_lss(toks_test, seed, k = 10, include_data = TRUE, spatial = FALSE)

  expect_s3_class(lss2, "textmodel_lss")
  expect_equal(lss2$concatenator, concatenator(toks_test))
  expect_equal(docnames(lss2$data), docnames(toks_test))
  expect_equal(
    names(predict(lss2)),
    docnames(toks_test)
  )

  # with terms
  lss3 <- textmodel_lss(toks_test, seed, k = 10, terms = feat_test,
                        include_data = TRUE, group_data = TRUE, spatial = FALSE)

  expect_s3_class(lss3, "textmodel_lss")
  expect_true(all(names(lss3$beta) %in% feat_test))
  expect_equal(
    names(predict(lss3)),
    docnames(tokens_group(toks_test))
  )

  # with tokens_xptr
  lss4 <- textmodel_lss(as.tokens_xptr(toks_test), seed, k = 10,
                        include_data = TRUE, spatial = FALSE)

  expect_s3_class(lss4, "textmodel_lss")
  expect_equal(docnames(lss4$data), docnames(toks_test))

  # warning
  expect_warning(
    textmodel_lss(toks_test, seed, k = 10,
                  include_data = FALSE, group_data = TRUE, spatial = FALSE),
    "group_data is ignored when include_data = FALSE"
  )

  # error
  expect_error(
    textmodel_lss(toks_test, k = -1, spatial = FALSE),
    "The value of k must be between 2 and Inf"
  )
  expect_error(
    textmodel_lss(toks_test, k = c(10, 20), spatial = FALSE),
    "The length of k must be 1"
  )

})

