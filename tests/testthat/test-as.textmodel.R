context("test as.textmodel_lss")

mat_test <- readRDS("../data/matrix_embedding.RDS")
toks_test <- readRDS("../data/tokens_test.RDS")
feat_test <- head(char_keyness(toks_test, "america*", min_count = 1, p = 0.05), 100)
dfmt_test <- dfm_group(dfm(toks_test))

test_that("as.textmodel_lss works with matrix", {
    seed <- as.seedwords(data_dictionary_sentiment)
    term <- c("decision", "instance", "universal", "foundations", "the")
    lss <- as.textmodel_lss(mat_test, seed, term)
    expect_equal(names(lss), names(LSX:::build_lss()))
    expect_equal(dim(lss$embedding), c(100, 7))
    pred <- predict(lss, dfmt_test)
    expect_equal(names(pred), rownames(dfmt_test))
    expect_false(any(is.na(pred)))
})

test_that("as.textmodel_lss works with vector", {
    lss <- as.textmodel_lss(c("decision" = 0.1, "instance" = -0.1,
                              "foundations" = 0.3, "the" = 0))
    expect_equal(names(lss), names(LSX:::build_lss()))
    pred <- predict(lss, dfmt_test)
    expect_equal(names(pred), rownames(dfmt_test))
    expect_false(any(is.na(pred)))
})
