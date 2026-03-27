
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
  expect_true(
    lss$max_prob
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
    as.textmodel_lss(dov_test, max_prob = c(TRUE, FALSE)),
    "The length of max_prob must be 1"
  )
  expect_error(
    as.textmodel_lss(dov_test, "xxxx"),
    "Seed words are not found in x"
  )

  # single seed
  lss2 <- as.textmodel_lss(dov_test, "good", max_prob = TRUE)
  expect_true(
    lss2$max_prob
  )
  expect_equal(lss2$seeds, "good")
  expect_equal(lss2$seeds_weighted,
               c("good" = 1))

  # glob seeds
  seed <- c("america" = 1, "nation*" = 1, "foreign*" = -1)
  lss3 <- as.textmodel_lss(dov_test, seed, max_prob = FALSE)
  expect_equal(
    lss3$seeds,
    c("america" = 1, "nation*" = 1, "foreign*" = -1)
  )
  expect_equal(
    lss3$seeds_weighted,
    c("america" = 0.25, "nations" = 0.25, "nation" = 0.25,  "national" = 0.25,
      "foreign" = -1)
  )

})
