
seed <- as.seedwords(data_dictionary_sentiment)

test_that("as.textmodel_lss works with textmodel_wordvector", {

  skip_if_not(utils::packageVersion("wordvector") >= "0.6.0")

  # spatial
  wdv <- readRDS("../data/word2vec.RDS")
  lss <- as.textmodel_lss(wdv, seed, spatial = TRUE)

  expect_equal(lss$beta_type, "similarity")
  expect_equal(lss$embedding, t(wdv$values))
  expect_identical(lss$frequency, wdv$frequency)
  expect_identical(
    names(lss$frequency),
    names(lss$frequency)
  )
  expect_identical(
    names(lss$beta),
    names(lss$frequency)
  )
  expect_error(
    as.textmodel_lss(wdv, seed, spatial = FALSE),
    "x must be trained with normalize = FALSE"
  )

  # probabilistic
  wdv2 <- readRDS("../data/word2vec-prob.RDS")
  lss2 <- as.textmodel_lss(wdv2, seed, spatial = FALSE)

  expect_equal(lss2$beta_type, "probability")
  expect_true(is.null(lss2$embedding))
  expect_identical(lss2$frequency, wdv2$frequency)
  expect_identical(
    names(lss2$frequency),
    names(wdv2$frequency)
  )
  expect_identical(
    names(lss2$beta),
    names(lss2$frequency)
  )

  # single seed
  lss3 <- as.textmodel_lss(wdv2, "good", spatial = FALSE)
  expect_true(is.null(lss3$embedding))
  expect_identical(lss3$frequency, wdv2$frequency)
  expect_identical(
    names(lss3$frequency),
    names(wdv2$frequency)
  )
  expect_identical(
    names(lss3$beta),
    names(lss3$frequency)
  )
})
