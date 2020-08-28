context("test textplot_*")

corp_sent <- corpus_reshape(data_corpus_inaugural, "sentence")
test_toks <- tokens(corp_sent, remove_punct = TRUE) %>%
    tokens_remove(stopwords())

test_that("textplot_* works", {
    dfmt <- dfm(test_toks)
    seed <- c("nice*" = 1, "positive*" = 1, "bad*" = -1, "negative*" = -1)
    lss <- textmodel_lss(dfmt, seed, k = 10)
    expect_equal(class(textplot_simil(lss, group = TRUE)), c("gg", "ggplot"))
    expect_equal(class(textplot_simil(lss, group = FALSE)), c("gg", "ggplot"))
    expect_equal(class(textplot_factor(lss)), c("gg", "ggplot"))
    expect_equal(class(textplot_terms(lss, highlighted = c("positive", "bad", "xxxx"))),
                 c("gg", "ggplot"))
    expect_equal(class(textplot_scale1d(lss)), c("gg", "ggplot"))
    expect_error(textplot_scale1d(lss, margin = "document"),
                 "There is no document margin")
})

test_that("textplot_* raise error when attributes are missing", {
    dfmt <- dfm(test_toks)
    coef <- rnorm(100)
    names(coef) <- topfeatures(dfmt, 100)
    lss <- as.textmodel_lss(coef)
    expect_error(textplot_simil(lss, group = TRUE), "Invalid textmodel_lss object")
    expect_error(textplot_simil(lss, group = FALSE), "Invalid textmodel_lss object")
    expect_error(textplot_factor(lss), "Invalid textmodel_lss object")
})
