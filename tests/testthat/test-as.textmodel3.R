
dov_test <- readRDS("../data/doc2vec.RDS")

test_that("as.textmodel_lss is working", {

  skip_if_not(utils::packageVersion("wordvector") >= "0.6.0")

  seed <- as.seedwords(data_dictionary_sentiment)
  lss <- as.textmodel_lss(dov_test, seed)

  expect_equal(
    lss$seeds,
    seed
  )
  expect_equal(
    lss$seeds_weighted,
    c(good = 0.5, superior = 0.5,
      wrong = -0.5, inferior = -0.5)
  )
  expect_equal(
    lss$prob_mode,
    "mean"
  )
  expect_equal(
    lss$beta_type, "dummy"
  )
  expect_true(
    is.null(lss$embedding)
  )
  expect_identical(lss$frequency, dov_test$frequency)
  expect_identical(
    names(lss$frequency), names(dov_test$frequency)
  )
  expect_identical(
    names(lss$beta), names(dov_test$frequency)
  )
  expect_true(
    ggplot2::is_ggplot(textplot_terms(lss))
  )
  expect_true(
    all(predict(lss) == lss$alpha)
  )
  expect_false(
    all(predict(lss, min_n = 10) == lss$alpha)
  )
  expect_true(
    ggplot2::is_ggplot(textplot_terms(lss))
  )

  # errors
  expect_error(
    as.textmodel_lss(dov_test, c(1, 2)),
    "seeds must be a character or named-numeric vector"
  )
  expect_error(
    as.textmodel_lss(dov_test, prob_mode = "xxx"),
    "'arg' should be one of"
  )
  expect_error(
    as.textmodel_lss(dov_test, "xxxx"),
    "Seed words are not found in x"
  )

  # single seed
  lss2 <- as.textmodel_lss(dov_test, "good")
  expect_equal(
    lss2$prob_mode, "mean"
  )
  expect_equal(lss2$seeds, "good")
  expect_equal(lss2$seeds_weighted,
               c("good" = 1))

  # glob seeds
  seed <- c("america" = 1, "nation*" = 1, "foreign*" = -1)
  lss3 <- as.textmodel_lss(dov_test, seed, prob_mode = "mean")
  expect_equal(
    lss3$seeds,
    c("america" = 1, "nation*" = 1, "foreign*" = -1)
  )
  expect_equal(
    lss3$seeds_weighted,
    c("america" = 0.25, "nations" = 0.25, "nation" = 0.25,  "national" = 0.25,
      "foreign" = -1)
  )

  # prob_mode
  seed <- c("government" = 1, "citizen" = -1)
  lss4 <- as.textmodel_lss(dov_test, seed, prob_mode = "mean")
  lss5 <- as.textmodel_lss(dov_test, seed, prob_mode = "max")
  expect_true(
    any(predict(lss4) < 0)
  )
  expect_true(
    any(predict(lss5) < 0)
  )

  # doc2vec has no data
  dov_test_nd <- dov_test
  dov_test_nd$data <- NULL
  lss6 <- as.textmodel_lss(dov_test_nd, seed)

  expect_error(
    textplot_terms(lss6),
    "x must be trained with include_data = TRUE"
  )

  expect_true(
    all(predict(lss6) != predict(lss6, min_n = 1000))
  )

  # empty document
  dov_test_empty <- dov_test
  dov_test_empty$ntoken[c(10, 20, 30)] <- 0
  lss7 <- as.textmodel_lss(dov_test_empty, seed)

  expect_equal(
    predict(lss7)[c(10, 20, 30)],
    c("1789-Washington.10" = NA_real_,
      "1789-Washington.20" = NA_real_,
      "1797-Adams.3" = NA_real_)
  )

})
