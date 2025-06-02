require(quanteda)
require(ggplot2)

lss_test <- readRDS("../data/lss_k300.RDS")

test_that("diagnosys works", {
    skip_on_travis()
    txt <- c("a b c d 0.2 . (xxx) \u2700", "a b_c 1st 2nd k100@gmail.com",
           "Aa bb cc. Xx yy zz.", "Aa bb cc. Xx yy zz.")
    dat <- data.frame(doc_id = paste0("text", 1:4),
                    number =  c(1, 3, 0, 0),
                    punct  =  c(4, 2, 2, 2),
                    symbol =  c(1, 0, 0, 0),
                    any    =  c(5, 4, 2, 2),
                    n_sent =  c(1, 1, 2, 2),
                    n_token = c(10, 5, 8, 8),
                    dupli = c(FALSE, FALSE, FALSE, TRUE),
                    noise = c(0.5, 0.8, 0.25, 0.25),
                    stringsAsFactors = FALSE)
    suppressWarnings(
        expect_equal(diagnosys(txt), dat)
    )
    expect_warning(diagnosys(txt),
                   "'diagnosys.corpus' is deprecated")

})

test_that("as.seedwords works", {
    lis1 <- list(c("a", "b", "c"), c("d", "e", "f"))
    expect_equal(as.seedwords(lis1),
                 c("a" = 1, "b" = 1, "c" = 1, "d" = -1, "e" = -1, "f" = -1))
    lis2 <- list(c("a", "b", "c"), c("d", "e", "f"))
    expect_equal(as.seedwords(lis2, upper = 2, lower = 1),
                 c("d" = 1, "e" = 1, "f" = 1, "a" = -1, "b" = -1, "c" = -1))
    lis3 <- list("pos" = c("a", "b", "c"), "neg" = c("d", "e", "f"))
    expect_equal(as.seedwords(lis3, upper = "pos", lower = "neg"),
                 c("a" = 1, "b" = 1, "c" = 1, "d" = -1, "e" = -1, "f" = -1))
    lis4 <- list("pos" = c("a", "a"), "neg" = c("b", "b"))
    expect_equal(as.seedwords(lis4, upper = "pos", lower = "neg"),
                 c("a" = 1, "b" = -1))
    lis5 <- list("pos1" = c("a", "b"), "pos2" = c("c"), "neg" = c("d", "e", "f"))
    expect_equal(as.seedwords(lis5, upper = c("pos1", "pos2"), lower = "neg"),
                 c("a" = 1, "b" = 1, "c" = 1, "d" = -1, "e" = -1, "f" = -1))

    dict1 <- dictionary(lis3)
    expect_equal(as.seedwords(dict1, upper = "pos", lower = "neg"),
                 c("a" = 1, "b" = 1, "c" = 1, "d" = -1, "e" = -1, "f" = -1))
    expect_error(as.seedwords(data.frame(1:3)), "x must be a list or dictionary object")

    dict2 <- dictionary(list("pos" = "very good", "neg" = "very bad"))
    expect_equal(as.seedwords(dict2),
                 c("very_good" = 1, "very_bad" = -1))
    expect_equal(as.seedwords(dict2, concatenator = "+"),
                 c("very+good" = 1, "very+bad" = -1))

})



test_that("smooth_lss works", {

  skip_on_cran() # takes to much time

  corp <- corpus_reshape(data_corpus_inaugural)
  toks <- tokens(corp)
  dfmt <- dfm(toks, remove_padding = TRUE) %>%
    dfm_subset(Party %in% c("Democratic", "Republican")) %>%
    dfm_trim()
  seed <- as.seedwords(data_dictionary_ideology)
  lss <- textmodel_lss(dfmt, seed, k = 150, include_data = TRUE,
                       group_data = TRUE)

  dat <- docvars(lss$data)
  dat$lss <- predict(lss)
  dat$date <- as.Date(paste0(dat$Year, "-01-20"))

  smo_le <- smooth_lss(dat, lss_var = "lss", by = "year",
                          span = 0.1, engine = "loess")
  expect_equal(colnames(smo_le),
               c("date", "time", "fit", "se.fit"))

  smo_lf <- smooth_lss(dat, lss_var = "lss", by = "year",
                       span = 0.1, engine = "locfit")
  expect_equal(colnames(smo_lf),
               c("date", "time", "fit", "se.fit"))

  expect_true(cor(smo_le$fit, smo_lf$fit, use = "pair") > 0.90)

  # group by variable
  smo_gr_le <- smooth_lss(dat, lss_var = "lss", by = "year",
                       span = 0.1, group = "Party", engine = "loess")
  expect_equal(colnames(smo_gr_le),
               c("date", "time", "fit", "se.fit", "Party"))
  expect_equal(levels(smo_gr_le$Party),
               c("Democratic", "Republican"))

  smo_gr_lf <- smooth_lss(dat, lss_var = "lss", by = "year",
                    span = 0.1, group = "Party", engine = "locfit")
  expect_equal(colnames(smo_gr_lf),
               c("date", "time", "fit", "se.fit", "Party"))
  expect_equal(levels(smo_gr_lf$Party),
               c("Democratic", "Republican"))

  expect_true(cor(smo_gr_le$fit, smo_gr_lf$fit, use = "pair") > 0.90)

  # check input values
  expect_error(
    smooth_lss(dat),
    "fit does not exist in x"
  )
  expect_error(
    smooth_lss(smooth_lss(dat, lss_var = "President")),
    "lss_var must be a numeric column"
  )
  expect_error(
    smooth_lss(dat, lss_var = "lss", date_var = "xxx"),
    "xxx does not exist in x"
  )
  expect_error(
    smooth_lss(dat, lss_var = "lss", date_var = "Year"),
    "date_var must be a date column"
  )
  expect_error(
    smooth_lss(dat, lss_var = "lss", group = "xxx"),
    "xxx does not exist in x"
  )
})

test_that("smooth_lss works with multiple grouping variables", {

  date <- seq(as.Date("2025-01-01"), as.Date("2025-01-31"), by = "1 day")
  n <- 1000
  dat <- data.frame(fit = rnorm(n),
                    date = sample(date, n, replace = TRUE),
                    class1 = factor(sample(letters[1:3], n, replace = TRUE)),
                    class2 = sample(LETTERS[1:2], n, replace = TRUE),
                    number = sample(1:10000, n))
  smo1 <- smooth_lss(dat)
  smo2 <- smooth_lss(dat, group = "class1")
  smo3 <- smooth_lss(dat, group = c("class1", "class2"))

  expect_equal(
    nrow(smo1), 31
  )
  expect_equal(
    sapply(smo1, class),
    c(date = "Date", time = "numeric", fit = "numeric", se.fit = "numeric")
  )

  expect_equal(
    sapply(smo2, class),
    c(date = "Date", time = "numeric", fit = "numeric", se.fit = "numeric",
      class1 = "factor")
  )
  expect_equal(
    nrow(smo2), 31 * 3,
  )

  expect_equal(
    sapply(smo3, class),
    c(date = "Date", time = "numeric", fit = "numeric", se.fit = "numeric",
      class1 = "factor", class2 = "character")
  )
  expect_equal(
    nrow(smo3),
    31 * 3 * 2
  )

  # error
  expect_error(
    smooth_lss(dat, group = c("class1", "xxxx")),
    "xxxx does not exist in x"
  )

  expect_error(
    smooth_lss(dat, group = c("class1", "number")),
    "columns for grouping must be a character or factor"
  )
})
