require(quanteda)

mat_test <- readRDS("../data/matrix_embedding.RDS")
toks_test <- readRDS("../data/tokens_test.RDS")
feat_test <- head(char_context(toks_test, "america*", min_count = 1, p = 0.05), 100)
dfmt_test <- dfm(toks_test)
seed <- as.seedwords(data_dictionary_sentiment)
lss_test <- textmodel_lss(dfmt_test, seed, terms = feat_test, k = 50,
                          include_data = FALSE)

test_that("as.textmodel_lss works with matrix", {

    term <- c("decision", "instance", "universal", "foundations", "the")

    # with terms
    lss1 <- as.textmodel_lss(mat_test, seed, term)
    expect_equal(names(lss1), names(LSX:::build_lss()))
    expect_equal(dim(lss1$embedding), c(100, 7))
    expect_false(any(duplicated(names(coef(lss1)))))
    pred1 <- predict(lss1, dfmt_test)
    expect_equal(names(pred1), rownames(dfmt_test))
    expect_equal(rowSums(dfmt_test[,names(lss1$beta)]) == 0,
                 is.na(pred1))

    # without terms
    lss2 <- as.textmodel_lss(mat_test, seed)
    expect_equal(names(lss2), names(LSX:::build_lss()))
    expect_equal(dim(lss2$embedding), dim(mat_test))
    expect_false(any(duplicated(names(coef(lss2)))))
    pred2 <- predict(lss2, dfmt_test)
    expect_equal(names(pred2), rownames(dfmt_test))
    expect_equal(rowSums(dfmt_test[,names(lss2$beta)]) == 0,
                 is.na(pred2))


    # with special features
    mat_special <- mat_test
    colnames(mat_special)[1:2] <- c("", "*")
    lss3 <- as.textmodel_lss(mat_special, seed)
    expect_equal(sum("" == names(coef(lss3))), 0)
    expect_equal(sum("*" == names(coef(lss3))), 1)

    # with slice
    lss4 <- as.textmodel_lss(mat_test, seed, slice = 50)
    expect_error(
        as.textmodel_lss(mat_test, seed, slice = 150),
        "The value of slice must be between 1 and 100"
    )
    expect_error(
        as.textmodel_lss(mat_test, seed, slice = 1:150),
        "The length of slice must be between 1 and 100"
    )
    expect_identical(coef(lss4),
                     coef(as.textmodel_lss(mat_test, seed, slice = 1:50)))
    expect_equal(dim(lss4$embedding), c(50, 1000))
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

test_that("as.textmodel_lss works with textmodel_lss", {

    lss <- as.textmodel_lss(lss_test, seed, slice = 10)
    expect_error(
        as.textmodel_lss(lss_test, seed, slice = 100),
        "The value of slice must be between 1 and 50"
    )
    expect_error(
        as.textmodel_lss(lss_test, seed, slice = 1:100),
        "The length of slice must be between 1 and 50"
    )
    expect_identical(coef(lss),
                     coef(as.textmodel_lss(lss_test, seed, slice = 1:10)))
    expect_equal(dim(lss$embedding), c(10, 112))
    expect_identical(lss_test$data, lss$data)
    expect_identical(lss_test$frequency, lss$frequency)

    # with dummy LSS
    weight <- c("decision" = 0.1, "instance" = -0.1,
                "foundations" = 0.3, "the" = 0)
    lss_dummy <- as.textmodel_lss(weight)
    expect_error(
        as.textmodel_lss(lss_dummy, seed),
        "x must be a valid textmodel_lss object"
    )
})

test_that("as.textmodel_lss works with vector", {
    weight <- c("decision" = 0.1, "instance" = -0.1,
                "foundations" = 0.3, "the" = 0)
    lss <- as.textmodel_lss(weight)
    expect_equal(names(lss), names(LSX:::build_lss()))
    pred <- predict(lss, dfmt_test)
    expect_equal(names(pred), rownames(dfmt_test))
    expect_equal(rowSums(dfmt_test[,names(lss$beta)]) == 0,
                 is.na(pred))
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

test_that("auto_weight is working", {
    skip_on_cran()

    lss1 <- as.textmodel_lss(mat_test, seed)
    lss2 <- as.textmodel_lss(mat_test, seed, auto_weight = TRUE)
    expect_true(
        all(lss1$seeds_weighted != lss2$seeds_weighted)
    )
    expect_true(
        all(sign(lss1$seeds_weighted) == sign(lss2$seeds_weighted))
    )
    expect_true(
        all(abs(lss2$beta[names(lss2$seeds_weighted)] - lss1$seeds_weighted) < 0.05)
    )
    expect_output(
        as.textmodel_lss(mat_test, seed, auto_weight = TRUE, verbose = TRUE),
        "Optimizing seed weights..."
    )
})

test_that("terms is working", {
    skip_on_cran()

    lss <- textmodel_lss(dfmt_test, seed, k = 50)

    # glob pattern
    lss1 <- as.textmodel_lss(lss, seed, terms = "poli*")
    expect_equal(sum(stringi::stri_startswith_fixed(names(lss1$beta), "poli")), 11)

    # numeric vector
    weight <- sample(1:10, length(lss1$beta), replace = TRUE) / 10
    names(weight) <- names(lss1$beta)
    lss2 <- as.textmodel_lss(lss, seed, terms = weight)
    expect_true(all(lss2$beta == lss1$beta * weight))
    expect_error(as.textmodel_lss(lss, seed, terms = c("polity" = 0.2, "politic" = -0.1)),
                 "terms must be positive values without NA")
    expect_error(as.textmodel_lss(lss, seed, terms = c("polity" = 0.2, "politic" = NA)),
                 "terms must be positive values without NA")
    expect_error(as.textmodel_lss(lss, seed, terms = c(01, 0.2)),
                 "terms must be named")

})
