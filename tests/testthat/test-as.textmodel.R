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

test_that("as.textmodel_lss errors with invalid columns", {
    seed <- as.seedwords(data_dictionary_sentiment)
    mat_nocol <- mat_nacol <- mat_na <- mat_test
    colnames(mat_nocol) <- NULL
    expect_error(as.textmodel_lss(mat_nocol, seed),
                 "x must have column names for features")
    colnames(mat_nacol)[1] <- NA
    expect_error(as.textmodel_lss(mat_nacol, seed),
                 "x must not have NA in the column names")
    mat_na[1,1] <- NA
    expect_error(as.textmodel_lss(mat_na, seed),
                 "x must not have NA")
})

test_that("as.textmodel_lss works with vector", {
    weight <- c("decision" = 0.1, "instance" = -0.1,
                "foundations" = 0.3, "the" = 0)
    lss <- as.textmodel_lss(weight)
    expect_equal(names(lss), names(LSX:::build_lss()))
    pred <- predict(lss, dfmt_test)
    expect_equal(names(pred), rownames(dfmt_test))
    expect_false(any(is.na(pred)))
})

test_that("as.textmodel_lss errors with vector", {
    weight <- c("decision" = 0.1, "instance" = -0.1,
                "foundations" = 0.3, "the" = 0)
    weight_noname <- weight_naname <- weight_na <- weight
    names(weight_noname) <- NULL
    expect_error(as.textmodel_lss(weight_noname),
                 "x must have names for features")
    names(weight_naname)[1] <- NA
    expect_error(as.textmodel_lss(weight_naname),
                 "x must not have NA in the names")
    weight_na[1] <- NA
    expect_error(as.textmodel_lss(weight_na),
                 "x must not have NA")
})

