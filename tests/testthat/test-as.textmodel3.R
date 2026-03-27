
dov_test <- readRDS("../data/doc2vec.RDS")

test_that("as.textmodel_lss is working", {

  # single seed
  lss1 <- as.textmodel_lss(dov_test, "good", max_prob = FALSE)
  expect_false(
    lss1$max_prob
  )
  expect_true(
    all(predict(lss1) == lss1$alpha)
  )
  expect_false(
    all(predict(lss1, min_n = 10) == lss1$alpha)
  )
  expect_true(
    ggplot2::is_ggplot(textplot_terms(lss1))
  )

  lss2 <- as.textmodel_lss(dov_test, "good", max_prob = TRUE)
  expect_true(
    lss2$max_prob
  )
  expect_true(
    all(predict(lss2) == lss2$alpha)
  )
  expect_false(
    all(predict(lss2, min_n = 10) == lss2$alpha)
  )
  expect_equal(lss2$seeds, "good")
  expect_equal(lss2$seeds_weighted,
               c("good" = 1))
  expect_identical(
    predict(lss1),
    predict(lss2)
  )

  # fixed seeds
  seed <- c("good" = 1, "superior" = 1, "wrong" = -1, "nasty" = -1)
  lss3 <- as.textmodel_lss(dov_test, seed, max_prob = FALSE)
  lss4 <- as.textmodel_lss(dov_test, seed, max_prob = TRUE)
  expect_gt(
    cor(predict(lss3), predict(lss4)),
    0.99
  )

  # glob seeds
  seed <- c("america" = 1, "nation*" = 1, "foreign*" = -1)
  lss5 <- as.textmodel_lss(dov_test, seed, max_prob = FALSE)
  expect_equal(
    lss5$seeds,
    c("america" = 1, "nation*" = 1, "foreign*" = -1)
  )
  expect_equal(
    lss5$seeds_weighted,
    c("america" = 0.25, "nations" = 0.25, "nation" = 0.25,  "national" = 0.25,
      "foreign" = -1)
  )
  expect_true(
    ggplot2::is_ggplot(textplot_terms(lss5))
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
})
