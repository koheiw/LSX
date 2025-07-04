require(quanteda)

# create and save test object
# corp_sent <- corpus_reshape(data_corpus_inaugural, "sentence")
# toks_test <- tokens(corp_sent, remove_punct = TRUE)
# saveRDS(toks_test, "tests/data/tokens.RDS")

toks_test <- readRDS("../data/tokens.RDS")
feat_test <- head(char_context(toks_test, "america*", min_count = 1, p = 0.05), 100)
dfmt_test <- dfm(toks_test)
fcmt_test <- fcm(dfmt_test)

seed <- as.seedwords(data_dictionary_sentiment)
lss_test <- textmodel_lss(dfmt_test, seed, terms = feat_test, k = 300,
                          include_data = TRUE)
lss_test_nd <- textmodel_lss(dfmt_test, seed, terms = feat_test, k = 300,
                             include_data = FALSE)
lss_test_ss <- textmodel_lss(dfmt_test, seed[1], terms = feat_test, k = 300)
lss_test_fcm <- textmodel_lss(fcmt_test, seed, terms = feat_test, k = 50)

test_that("char_context is working", {

    expect_identical(length(feat_test), 100L)
    feat1 <- char_context(toks_test, "america.*", "regex", min_count = 1, p = 0.05)
    expect_identical(head(feat1, 100), feat_test)

    feat2 <- char_context(toks_test, "America*", case_insensitive = FALSE, min_count = 1, p = 0.05)
    expect_identical(head(feat2, 100), feat_test)

    feat3 <- char_context(toks_test, "america*", min_count = 1000, remove_pattern = TRUE)
    expect_identical(feat3, character())

    feat4 <- char_context(toks_test, "america*", min_count = 1000, remove_pattern = FALSE)
    expect_identical(feat4, character())

    expect_silent(char_context(toks_test, "xxxxx", min_count = 1, p = 0.05))
})

test_that("textmodel_lss has all the attributes", {

    expect_equal(
        names(lss_test),
        c("data", "beta", "beta_type", "k", "slice", "frequency", "terms", "seeds", "seeds_weighted",
          "embedding", "similarity", "concatenator", "type", "spatial", "call", "version")
    )

    expect_true(is.numeric(lss_test$beta))
    expect_true(is.numeric(lss_test$frequency))
    expect_identical(names(lss_test$beta), names(lss_test$frequency))
    expect_true(is.dfm(lss_test$data))
    expect_identical(lss_test$terms, feat_test)
    expect_identical(names(lss_test$seeds), names(seedwords("pos-neg")))

    expect_equal(
        names(lss_test_fcm),
        c("data", "beta", "beta_type", "k", "slice", "frequency", "terms", "seeds", "seeds_weighted",
          "embedding", "similarity", "concatenator", "type", "spatial", "call", "version")
    )

    expect_true(is.numeric(lss_test_fcm$beta))
    expect_true(is.numeric(lss_test_fcm$frequency))
    expect_identical(names(lss_test_fcm$beta), names(lss_test_fcm$frequency))
    expect_true(is.null(lss_test_fcm$data))
    expect_identical(lss_test_fcm$terms, feat_test)
    expect_identical(names(lss_test_fcm$seeds), names(seedwords("pos-neg")))

    expect_equal(
        names(lss_test_nd),
        c("data", "beta", "beta_type", "k", "slice", "frequency", "terms", "seeds", "seeds_weighted",
          "embedding", "similarity", "concatenator", "type", "spatial", "call", "version")
    )

})

test_that("summary.textmodel_lss is working", {

    expect_silent(summary(lss_test))
    expect_silent(summary(lss_test_nd))

})

test_that("predict.textmodel_lss is working", {

    expect_warning(predict(lss_test, xxx = TRUE),
                   "xxx argument is not used")
    expect_warning(predict(lss_test, se.fit = TRUE),
                   "'se.fit' is deprecated; use 'se_fit'")
    expect_error(predict(lss_test, newdata = matrix(1:10)),
                 "newdata must be a dfm")
    expect_error(predict(lss_test_nd),
                 "The model includes no data, use newdata to supply a dfm.")

    pred1 <- predict(lss_test)
    expect_equal(length(pred1), ndoc(dfmt_test))
    expect_identical(names(pred1), docnames(dfmt_test))
    expect_true(is.numeric(pred1))
    expect_equal(mean(pred1, na.rm = TRUE), 0)
    expect_equal(sd(pred1, na.rm = TRUE), 1)

    pred2 <- predict(lss_test, se_fit = TRUE)
    expect_equal(length(pred2$fit), ndoc(dfmt_test))
    expect_identical(names(pred2$fit), docnames(dfmt_test))
    expect_equal(length(pred2$se.fit), ndoc(dfmt_test))
    expect_equal(length(pred2$n), ndoc(dfmt_test))
    expect_null(names(pred2$se.fit))
    expect_null(names(pred2$n))

    pred3 <- predict(lss_test, density = TRUE)
    expect_equal(length(pred3$density), ndoc(toks_test))
    expect_null(names(pred3$density))

    pred4 <- predict(lss_test, rescale = FALSE)
    expect_identical(names(pred4), docnames(toks_test))
    expect_equal(as.numeric(scale(pred4)), unname(pred1))

    pred5 <- predict(lss_test, se_fit = TRUE, density = TRUE)
    expect_equal(names(pred5), c("fit", "se.fit", "n", "density"))

    pred6 <- predict(lss_test, rescale = FALSE, min_n = 2)
    expect_true(all(is.na(pred4) == is.na(pred6)))
    expect_true(all(abs(pred6[pred5$n == 1]) < abs(pred4[pred5$n == 1]), na.rm = TRUE))
    expect_true(all(abs(pred6[pred5$n >= 2]) == abs(pred4[pred5$n >= 2]), na.rm = TRUE))

    expect_error(
        predict(lss_test, rescale = FALSE, min_n = -1),
        "The value of min_n must be between 0 and Inf"
    )
    expect_error(
        predict(lss_test, rescale = FALSE, min_n = c(0, 1)),
        "The length of min_n must be 1"
    )
})

test_that("density is correct", {

    dfmt <- dfm_group(dfm(toks_test))
    pred <- predict(lss_test, newdata = dfmt, density = TRUE)

    expect_equal(
        pred$density,
        unname(rowSums(dfm_select(dfm_weight(dfmt, "prop"), feat_test)))
    )
})

test_that("predict.textmodel_lss works with newdata", {
    dfmt <- dfm_group(dfm(toks_test))
    pred <- predict(lss_test, newdata = dfmt)
    expect_equal(length(pred), ndoc(dfmt))
})

test_that("data object is valid", {
    sum <- summary(data_textmodel_lss_russianprotests)
    expect_equal(class(sum), c("summary.textmodel", "list"))
})

test_that("calculation of fit and se_fit are correct", {

    lss <- as.textmodel_lss(c("a" = 0.1, "b" = 0.1, "c" = 0.3))
    toks <- tokens(c("a a a", "a b", "a a b c c d e"))
    dfmt <- dfm(toks)
    pred <- predict(lss, newdata = dfmt, se_fit = TRUE, rescale = FALSE)

    expect_equal(pred$fit[1], c(text1 = 0.10))
    expect_equal(pred$fit[2], c(text2 = 0.10))
    expect_equal(pred$fit[3], c(text3 = 0.1 * (2 / 5) + 0.1 * (1 / 5) + 0.3 * (2 / 5)))

    beta <- lss$beta
    dfmt_sub <- dfm_select(dfmt, names(beta))
    dfmt_prop <- dfm_weight(dfmt_sub, "prop")

    expect_equal(pred$se.fit[1],
                 unname(sqrt(sum(as.numeric(dfmt_prop[1,]) * (pred$fit[1] - beta) ^ 2)) / sqrt(rowSums(dfmt_sub)[1])))
    expect_equal(pred$se.fit[2],
                 unname(sqrt(sum(as.numeric(dfmt_prop[2,]) * (pred$fit[2] - beta) ^ 2)) / sqrt(rowSums(dfmt_sub)[2])))
    expect_equal(pred$se.fit[3],
                 unname(sqrt(sum(as.numeric(dfmt_prop[3,]) * (pred$fit[3] - beta) ^ 2)) / sqrt(rowSums(dfmt_sub)[3])))

    expect_equal(pred$n[1], 3)
    expect_equal(pred$n[2], 2)
    expect_equal(pred$n[3], 5)

})

test_that("textmodel_lss works with only with single seed", {
    skip_on_cran()
    expect_silent(textmodel_lss(dfm(toks_test), seedwords("pos-neg")[1], terms = feat_test, k = 10))
    expect_silent(textmodel_lss(dfm(toks_test), seedwords("pos-neg")[1], terms = character(), k = 10))
    expect_silent(textmodel_lss(dfm(toks_test), seedwords("pos-neg")[1], k = 10))
})

test_that("textmodel_lss.fcm works with ...", {
    skip_on_cran()
    expect_warning(textmodel_lss(fcmt_test, seedwords("pos-neg"),
                                 terms = feat_test, learning_rate = 0.1), NA)
    expect_warning(textmodel_lss(fcmt_test, seedwords("pos-neg"),
                                 terms = feat_test, alpha = 1), NA)
})

test_that("terms is working", {
    skip_on_cran()

    # glob pattern
    lss1 <- textmodel_lss(dfmt_test, seed, terms = "poli*", k = 300)
    expect_true(all(stringi::stri_startswith_fixed(names(lss1$beta), "poli")))

    # numeric vector
    weight <- sample(1:10, length(lss1$beta), replace = TRUE) / 10
    names(weight) <- names(lss1$beta)
    lss2 <- textmodel_lss(dfmt_test, seed, terms = weight, k = 300)
    expect_true(all(lss2$beta == lss1$beta * weight))
    expect_error(textmodel_lss(dfmt_test, seed, terms = c("polity" = 0.2, "politic" = -0.1), k = 300),
                 "terms must be positive values without NA")
    expect_error(textmodel_lss(dfmt_test, seed, terms = c("polity" = 0.2, "politic" = NA), k = 300),
                 "terms must be positive values without NA")
    expect_error(textmodel_lss(dfmt_test, seed, terms = c(01, 0.2), k = 300),
                 "terms must be named")

})

test_that("terms work with numeric vector", {
    lss <- textmodel_lss(dfmt_test, seed, terms = "poli*", k = 300)
    expect_true(all(stringi::stri_startswith_fixed(names(coef(lss)), "poli")))
})

test_that("simil_method works", {

    lss_cos <- textmodel_lss(dfm(toks_test), seedwords("pos-neg")[1], terms = feat_test,
                             k = 10)
    lss_cor <- textmodel_lss(dfm(toks_test), seedwords("pos-neg")[1], terms = feat_test,
                             simil_method = "correlation", k = 10)

    expect_false(identical(lss_cos, lss_cor))
    expect_error(textmodel_lss(dfm(toks_test), seedwords("pos-neg")[1], terms = feat_test,
                               simil_method = "something", k = 10), "'arg' should be one of")
})


test_that("include_data is working", {

    skip_on_cran() # takes to much time

    dfmt <- dfm(toks_test)
    lss <- textmodel_lss(dfmt, seedwords("pos-neg"), k = 10, include_data = TRUE)
    lss_nd <- textmodel_lss(dfmt, seedwords("pos-neg"), k = 10, include_data = FALSE)
    expect_error(predict(lss_nd), "The model includes no data")
    expect_identical(predict(lss), predict(lss_nd, newdata = dfmt))

    lss_gd <- textmodel_lss(dfmt, seedwords("pos-neg"), k = 10,
                            include_data = TRUE, group_data = TRUE)
    expect_equal(names(predict(lss_gd)), docnames(dfm_group(dfmt)))
    expect_warning(
        textmodel_lss(dfmt, seedwords("pos-neg"), k = 10,
                      include_data = FALSE, group_data = TRUE),
        "group_data is ignored when include_data = FALSE"
    )
})

test_that("predict.textmodel_lss computes scores correctly", {

    dfmt <- dfm_group(dfm(toks_test))
    dfmt[c(3, 10),] <- 0
    dfmt <- as.dfm(dfmt)
    pred <- predict(lss_test, newdata = dfmt)
    expect_equal(length(pred), ndoc(dfmt))
    expect_equal(is.na(pred[c("1789-Washington", "1797-Adams", "1825-Adams")]),
                 c("1789-Washington" = FALSE, "1797-Adams" = TRUE, "1825-Adams" = TRUE))

    pred2 <- predict(lss_test, newdata = dfmt, se_fit = TRUE)
    expect_equal(is.na(pred2$fit[c("1789-Washington", "1797-Adams", "1825-Adams")]),
                 c("1789-Washington" = FALSE, "1797-Adams" = TRUE, "1825-Adams" = TRUE))
    expect_equal(is.na(pred2$se.fit[c(1, 3, 10)]), c(FALSE, TRUE, TRUE))
    expect_equal(pred2$n[c(1, 3, 10)] == 0, c(FALSE, TRUE, TRUE))

    pred3 <- predict(lss_test, newdata = dfmt, se_fit = TRUE, min_n = 2)
    expect_equal(is.na(pred3$fit[c("1789-Washington", "1797-Adams", "1825-Adams")]),
                 c("1789-Washington" = FALSE, "1797-Adams" = TRUE, "1825-Adams" = TRUE))
    expect_equal(is.na(pred3$se.fit[c(1, 3, 10)]), c(FALSE, TRUE, TRUE))
    expect_equal(pred3$n[c(1, 3, 10)] == 0, c(FALSE, TRUE, TRUE))

    load("../data/prediction_v0.99.RDA")
    expect_equal(pred, pred_v099, tolerance = 0.0001)
    expect_equal(pred2$fit, pred2_v099$fit, tolerance = 0.0001)
    expect_equal(pred2$se.fit, pred2_v099$se.fit, tolerance = 0.0001)
    expect_equal(pred2$n, pred2_v099$n)
})


test_that("textmodel_lss works with glob patterns", {
    dfmt <- dfm(toks_test)
    seed <- c("nice*" = 1, "positive*" = 1, "bad*" = -1, "negative*" = -1)
    lss <- textmodel_lss(dfmt, seed, k = 10)
    expect_equal(lss$seeds, seed)
    expect_equal(names(lss$seeds_weighted),
                 c("positive", "positively", "badge", "bad", "badly", "negative"))
})

test_that("textmodel_lss works with non-existent seeds", {

    seed1 <- c("good" = 1, "bad" = -1, "xyz" = -1)
    expect_silent(textmodel_lss(dfmt_test, seed1, k = 10))

    seed2 <- c("xyz", "xxx")
    expect_error(textmodel_lss(dfmt_test, seed2, k = 10),
                 "No seed word is found in the dfm")
})

test_that("rsvd and irlba work", {

    if (requireNamespace("irlba")) {
        expect_silent(textmodel_lss(dfmt_test, seedwords("pos-neg"), k = 10, engine = "irlba"))
    } else {
        expect_error(
            expect_silent(textmodel_lss(dfmt_test, seedwords("pos-neg"), k = 10, engine = "irlba")),
            "irlba package must be installed"
        )
    }

    if (requireNamespace("rsvd")) {
        expect_silent(textmodel_lss(dfmt_test, seedwords("pos-neg"), k = 10, engine = "rsvd"))
    } else {
        expect_error(
            expect_silent(textmodel_lss(dfmt_test, seedwords("pos-neg"), k = 10, engine = "rsvd")),
            "rsvd package must be installed"
        )
    }

})

test_that("text2vec works", {

    skip_on_cran() # takes to much time

    fcmt <- fcm(toks_test)
    lss <- textmodel_lss(fcmt, seedwords("pos-neg"), engine = "rsparse")
    expect_equal(
        names(predict(lss, dfmt_test)),
        docnames(dfmt_test)
    )
    expect_error(
        predict(lss),
        "The model includes no data"
    )
    expect_true(setequal(names(coef(lss)), colnames(fcmt)))
})

test_that("weight is working", {

    lss1 <- textmodel_lss(dfmt_test, seedwords("pos-neg"), k = 10, weight = "count")
    lss2 <- textmodel_lss(dfmt_test, seedwords("pos-neg"), k = 10, weight = "logcount")
    expect_false(identical(lss1, lss2))
    expect_error(
        textmodel_lss(dfmt_test, seedwords("pos-neg"), k = 10, weight = "xxx")
    )
})

test_that("slice argument is working", {
    expect_identical(
        dim(textmodel_lss(dfmt_test, seed, terms = feat_test, k = 300, slice = 100)$embedding),
        dim(textmodel_lss(dfmt_test, seed, terms = feat_test, k = 300, slice = 1:100)$embedding)
    )
    expect_silent(
        textmodel_lss(dfmt_test, seed, terms = feat_test, k = 300, slice = 1:100)
    )
    expect_error(
        textmodel_lss(dfmt_test, seed, terms = feat_test, k = 300, slice = 1:400),
        "The length of slice must be between 1 and 300"
    )
})

test_that("cohesion works", {
    coh <- cohesion(lss_test)
    expect_identical(names(coh), c("k", "raw", "smoothed"))
    expect_identical(nrow(coh), lss_test$k)
    expect_error(cohesion(list()), "x must be a textmodel_lss object")
})

test_that("weight_seeds() works", {
    expect_equal(
        LSX:::weight_seeds(c("a*" = 1, "b*" = -1), c("aa", "aaa", "bb", "bbb")),
        list("a*" = c("aa" = 0.5, "aaa" = 0.5),
             "b*" = c("bb" = -0.5, "bbb" = -0.5))
    )
    expect_equal(
        LSX:::weight_seeds(c("a*" = 1), c("aa", "aaa", "bb", "bbb")),
        list("a*" = c("aa" = 0.5, "aaa" = 0.5))
    )
    expect_equal(
        LSX:::weight_seeds(c("a*" = 1, "c*" = -1), c("aa", "aaa", "bb", "bbb")),
        list("a*" = c("aa" = 0.5, "aaa" = 0.5),
             "c*" = numeric())
    )
    expect_equal(
        LSX:::weight_seeds(c("a*" = 1, "b*" = 1), c("aa", "aaa", "bb", "bbb")),
        list("a*" = c("aa" = 0.25, "aaa" = 0.25),
             "b*" = c("bb" = 0.25, "bbb" = 0.25))
    )
    expect_equal(
        LSX:::weight_seeds(c("aa" = 1, "aaa" = 1, "bb" = 1), c("aa", "aaa", "bb", "bbb")),
        list("aa" = c("aa" = 0.333),
             "aaa" = c("aaa" = 0.333),
             "bb" = c("bb" = 0.333)),
        tolerance = 0.01
    )
    expect_equal(
        LSX:::weight_seeds(c("aa" = 1, "aaa" = 1, "bb" = -1), c("aa", "aaa", "bb", "bbb")),
        list("aa" = c("aa" = 0.5),
             "aaa" = c("aaa" = 0.5),
             "bb" = c("bb" = -1)),
    )
})

test_that("old argument still works", {

    skip_on_cran() # takes to much time

    suppressWarnings({
        lss <- textmodel_lss(dfmt_test, seed, features = feat_test, k = 300)
    })
    expect_equal(lss_test$terms, lss$terms)

    suppressWarnings({
        lss_fcm <- textmodel_lss(fcmt_test, seed, features = feat_test, w = 50)
    })
    expect_equal(lss_test$terms, lss_fcm$terms)
})

test_that("se_fit is working", {
    beta <- c(a = 0.2, b = 0.1, z = 0)
    lss <- as.textmodel_lss(beta)
    dfmt1 <- dfm(tokens(c("a a a b b", "")))
    dfmt2 <- dfm(tokens(c("a a a b b z z z z z", "")))
    pred1 <- predict(lss, newdata = dfmt1, rescale = FALSE, min_n = 10, se_fit = TRUE)
    pred2 <- predict(lss, newdata = dfmt2, rescale = FALSE, se_fit = TRUE)
    expect_identical(pred1, pred2)
})

test_that("cut is working", {

    skip_on_cran() # takes to much time

    p0 <- predict(lss_test, rescale = TRUE, min_n = 10)
    p1 <- predict(lss_test, cut = 0.5, rescale = TRUE)
    expect_true(min(p1, na.rm = TRUE) < -1)
    expect_true(max(p1, na.rm = TRUE) > 1)
    expect_equal(cor(p0, p1, use = "pair"), 0.59, tolerance = 0.01)

    p2 <- predict(lss_test, cut = 0.5, rescale = FALSE)
    expect_true(min(p2, na.rm = TRUE) >= -1)
    expect_true(max(p2, na.rm = TRUE) <= 1)
    expect_equal(cor(p0, p2, use = "pair"), 0.59, tolerance = 0.01)

    p3 <- predict(lss_test, cut = 0.5, rescale = FALSE, min_n = 10)
    expect_true(min(p3, na.rm = TRUE) >= -1)
    expect_true(max(p3, na.rm = TRUE) <= 1)
    expect_equal(cor(p0, p3, use = "pair"), 0.73, tolerance = 0.01)

    p4 <- predict(lss_test, cut = 0.75, rescale = FALSE, min_n = 10)
    expect_true(min(p4, na.rm = TRUE) >= -1)
    expect_true(max(p4, na.rm = TRUE) <= 1)
    expect_equal(cor(p0, p4, use = "pair"), 0.33, tolerance = 0.01)

    p5 <- predict(lss_test, cut = c(0.25, 0.75), rescale = FALSE, min_n = 10)
    expect_true(min(p5, na.rm = TRUE) >= -1)
    expect_true(max(p5, na.rm = TRUE) <= 1)
    expect_equal(cor(p0, p5, use = "pair"), 0.77, tolerance = 0.01)

    p6 <- predict(lss_test, cut = c(0.75, 0.25), rescale = FALSE, min_n = 10)
    expect_identical(p5, p6)

    expect_error(
        predict(lss_test, cut = 1.5),
        "The value of cut must be between 0 and 1"
    )
    expect_error(
        predict(lss_test, cut = -0.1),
        "The value of cut must be between 0 and 1"
    )
    expect_error(
        predict(lss_test, cut = c(0.1, 0.5, 0.9)),
        "The length of cut must be between 1 and 2"
    )

    expect_equal(
        LSX:::cut_beta(c(1.1, -1.2, 0.5, 0.3, -0.2, -0.5)),
        c(1, -1, 1, 1, -1, -1)
    )
    expect_equal(
        LSX:::cut_beta(c(1.1, -1.2, 0.5, 0.3, -0.2, -0.5), c(0.2, 0.8)),
        c(1, -1, 0, 0, 0, -1)
    )

    beta <- rnorm(nfeat(dfmt_test), sd = 0.1)
    names(beta) <- featnames(dfmt_test)
    beta2 <- LSX:::cut_beta(beta, c(0.2, 0.8))

    lss1 <- as.textmodel_lss(beta)
    lss2 <- as.textmodel_lss(beta2)
    expect_equal(names(lss1$beta), names(lss2$beta))

    pred0 <- predict(lss1, dfmt_test, se_fit = TRUE)
    pred1 <- predict(lss1, dfmt_test, cut = c(0.2, 0.8), se_fit = TRUE)
    pred2 <- predict(lss2, dfmt_test, se_fit = TRUE)

    expect_equal(pred0$n, pred1$n)
    expect_equal(pred0$n, pred2$n)
    expect_equal(pred1$fit, pred2$fit)
})

test_that("rescaling still works", {

    expect_warning({
        p1 <- predict(lss_test, rescaling = TRUE)
    })
    expect_silent({
        p2 <- predict(lss_test, rescale = TRUE)
    })
    expect_identical(p1, p2)
})

test_that("textmodel_lss print messages", {

  expect_output(
    textmodel_lss(dfmt_test, seed, k = 100, verbose = TRUE),
    "Performing SVD by RSpectra", fixed = TRUE
  )
  expect_warning(
    textmodel_lss(dfmt_test, seed, features = feat_test, k = 100),
    "'features' is deprecated; use 'terms'", fixed = TRUE
  )
  expect_warning(
    textmodel_lss(dfmt_test, seed, k = 100, auto_weight = TRUE),
    "'auto_weight' is deprecated", fixed = TRUE
  )

})
