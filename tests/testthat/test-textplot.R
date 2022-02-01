
require(quanteda)
toks_test <- readRDS("../data/tokens_test.RDS")
test_toks <- tokens_remove(toks_test, stopwords())
dict <- dictionary(list("keywords" = c("positive", "bad", "xxxx")))

test_that("textplot_* works with SVD", {
    dfmt <- dfm(test_toks)
    seed <- c("nice*" = 1, "positive*" = 1, "bad*" = -1, "negative*" = -1)
    lss <- textmodel_lss(dfmt, seed, k = 10)
    expect_equal(class(textplot_simil(lss)), c("gg", "ggplot"))
    expect_equal(class(textplot_terms(lss, highlighted = dict$keywords)),
                 c("gg", "ggplot"))
    expect_equal(class(textplot_terms(lss, highlighted = dict$keywords, max_words = 2)),
                 c("gg", "ggplot"))
    expect_equal(class(textplot_terms(lss, highlighted = dict)),
                 c("gg", "ggplot"))
    expect_equal(class(textplot_terms(lss)), c("gg", "ggplot"))
})

test_that("textplot_* works with Glove", {
    fcmt <- fcm(test_toks)
    seed <- c("nice*" = 1, "positive*" = 1, "bad*" = -1, "negative*" = -1)
    lss <- textmodel_lss(fcmt, seed, w = 10)
    expect_equal(class(textplot_simil(lss)), c("gg", "ggplot"))
    expect_equal(class(textplot_terms(lss, highlighted = dict$keywords)),
                 c("gg", "ggplot"))
    expect_equal(class(textplot_terms(lss, highlighted = dict$keywords, max_words = 2)),
                 c("gg", "ggplot"))
    expect_equal(class(textplot_terms(lss, highlighted = dict)),
                 c("gg", "ggplot"))
    expect_equal(class(textplot_terms(lss)), c("gg", "ggplot"))
})

test_that("textplot_components() works", {

    seed <- c("nice*" = 1, "positive*" = 1, "bad*" = -1, "negative*" = -1)

    dfmt <- dfm(test_toks)
    lss_svd <- textmodel_lss(dfmt, seed, k = 10)
    fcmt <- fcm(test_toks)
    lss_glove <- textmodel_lss(fcmt, seed, w = 10)

    gg1 <- textplot_components(lss_svd, n = 5)
    expect_equal(length(levels(gg1$data$group)), 5)
    gg2 <- textplot_components(lss_svd, n = 3)
    expect_equal(length(levels(gg2$data$group)), 3)

    expect_equal(class(textplot_components(lss)), c("gg", "ggplot"))
    expect_error(textplot_components(lss_svd, n = 20), "n cannot be greater than k")
    expect_error(textplot_components(lss_glove), "SVD must be used to generate word vectors")
})

test_that("textplot_* raise error when attributes are missing", {
    dfmt <- dfm(test_toks)
    coef <- rnorm(100)
    names(coef) <- topfeatures(dfmt, 100)
    lss <- as.textmodel_lss(coef)
    expect_error(textplot_simil(lss),
                 "textplot_simil() does not work with dummy models", fixed = TRUE)
})
