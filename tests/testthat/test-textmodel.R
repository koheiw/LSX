context("test textmodel_lss")

corp_sent <- corpus_reshape(data_corpus_inaugural, "sentence")
toks_test <- tokens(corp_sent, remove_punct = TRUE)
feat_test <- head(char_keyness(toks_test, "america*", min_count = 1, p = 0.05), 100)
dfmt_test <- dfm(toks_test)
seed <- as.seedwords(data_dictionary_sentiment)
lss_test <- textmodel_lss(dfmt_test, seed, features = feat_test, k = 300)
lss_test_nd <- textmodel_lss(dfmt_test, seed, features = feat_test, k = 300,
                             include_data = FALSE)

test_that("char_keyness is working", {

    expect_identical(length(feat_test), 100L)
    feat1 <- char_keyness(toks_test, "america.*", "regex", min_count = 1, p = 0.05)
    expect_identical(head(feat1, 100), feat_test)

    feat2 <- char_keyness(toks_test, "America*", case_insensitive = FALSE, min_count = 1, p = 0.05)
    expect_identical(head(feat2, 100), feat_test)

    feat3 <- char_keyness(toks_test, "america*", min_count = 1000, remove_pattern = TRUE)
    expect_identical(feat3, character())

    feat4 <- char_keyness(toks_test, "america*", min_count = 1000, remove_pattern = FALSE)
    expect_identical(feat4, character())

    expect_error(char_keyness(toks_test, "xxxxx", min_count = 1, p = 0.05),
                 "xxxxx is not found")

})

test_that("char_keyness removes multi-word target", {

    feat_rp <- char_keyness(toks_test, phrase("united states"), "regex",
                            min_count = 1, p = 0.05)
    expect_identical(c("united", "states") %in% feat_rp,
                     c(FALSE, FALSE))

    feat_kp <- char_keyness(toks_test, phrase("united states"), "regex",
                            min_count = 1, p = 0.05, remove_pattern = FALSE)
    expect_identical(c("united", "states") %in% feat_kp,
                     c(TRUE, TRUE))
})

test_that("textmodel_lss has all the attributes", {

    expect_equal(
        names(lss_test),
        c("beta", "k", "s", "frequency", "features", "seeds", "seeds_weighted",
          "embedding", "similarity", "relevance", "importance", "call",  "data")
    )

    expect_true(is.numeric(lss_test$beta))
    expect_true(is.dfm(lss_test$data))
    expect_identical(lss_test$features, feat_test)
    expect_identical(names(lss_test$seeds), names(seedwords("pos-neg")))

    expect_equal(
        names(lss_test_nd),
        c("beta", "k", "s", "frequency", "features", "seeds", "seeds_weighted",
          "embedding", "similarity", "relevance", "importance", "call")
    )

})

test_that("summary.textmodel_lss is working", {

    expect_silent(summary(lss_test))
    expect_silent(summary(lss_test_nd))

})

test_that("predict.textmodel_lss is working", {

    pred1 <- predict(lss_test)
    expect_equal(length(pred1), ndoc(dfmt_test))
    expect_identical(names(pred1), docnames(dfmt_test))
    expect_true(is.numeric(pred1))
    expect_equal(mean(pred1, na.rm = TRUE), 0)
    expect_equal(sd(pred1, na.rm = TRUE), 1)

    pred2 <- predict(lss_test, se.fit = TRUE)
    expect_equal(length(pred2$fit), ndoc(dfmt_test))
    expect_identical(names(pred2$fit), docnames(dfmt_test))
    expect_equal(length(pred2$se.fit), ndoc(dfmt_test))
    expect_equal(length(pred2$n), ndoc(dfmt_test))
    expect_null(names(pred2$se.fit))
    expect_null(names(pred2$n))

    pred3 <- predict(lss_test, density = TRUE)
    expect_equal(length(pred3$density), ndoc(toks_test))
    expect_null(names(pred3$density))

    pred4 <- predict(lss_test, rescaling = FALSE)
    expect_identical(names(pred4), docnames(toks_test))
    expect_equal(as.numeric(scale(pred4)), unname(pred1))

    pred5 <- predict(lss_test, se.fit = TRUE, density = TRUE)
    expect_equal(names(pred5), c("fit", "se.fit", "n", "density"))

})

test_that("density is correct", {

    test_dfm <- dfm(data_corpus_inaugural)
    pred <- predict(lss_test, newdata = test_dfm, density = TRUE)

    expect_equal(
        pred$density,
        unname(rowSums(dfm_select(dfm_weight(test_dfm, "prop"), feat_test)))
    )
})

test_that("predict.textmodel_lss works with newdata", {

    pred <- predict(lss_test, newdata = dfm(data_corpus_inaugural))
    expect_equal(length(pred), ndoc(data_corpus_inaugural))

})

test_that("data object is valid", {
    sum <- summary(data_textmodel_lss_russianprotests)
    expect_equal(class(sum), c("summary.textmodel", "list"))
})

test_that("calculation of fit and se.fit are correct", {

    lss <- as.textmodel_lss(c("a" = 0.1, "b" = 0.1, "c" = 0.3))
    mt <- dfm(c("a a a", "a b", "a a b c c d e"))
    pred <- predict(lss, newdata = mt, se.fit = TRUE, rescaling = FALSE)

    expect_equal(pred$fit[1], c(text1 = 0.10))
    expect_equal(pred$fit[2], c(text2 = 0.10))
    expect_equal(pred$fit[3], c(text3 = 0.1 * (2 / 5) + 0.1 * (1 / 5) + 0.3 * (2 / 5)))

    beta <- coef(lss)
    mt_sub <- dfm_select(mt, names(beta))
    mt_prop <- dfm_weight(mt_sub, "prop")

    expect_equal(pred$se.fit[1],
                 unname(sqrt(sum(as.numeric(mt_prop[1,]) * (pred$fit[1] - beta) ^ 2)) / sqrt(rowSums(mt_sub)[1])))
    expect_equal(pred$se.fit[2],
                 unname(sqrt(sum(as.numeric(mt_prop[2,]) * (pred$fit[2] - beta) ^ 2)) / sqrt(rowSums(mt_sub)[2])))
    expect_equal(pred$se.fit[3],
                 unname(sqrt(sum(as.numeric(mt_prop[3,]) * (pred$fit[3] - beta) ^ 2)) / sqrt(rowSums(mt_sub)[3])))

    expect_equal(pred$n[1], 3)
    expect_equal(pred$n[2], 2)
    expect_equal(pred$n[3], 5)

})

test_that("as.textmodel_lss works with only with single seed", {
    expect_silent(textmodel_lss(dfm(toks_test), seedwords("pos-neg")[1], features = feat_test, k = 10))
    expect_silent(textmodel_lss(dfm(toks_test), seedwords("pos-neg")[1], features = character(), k = 10))
    expect_silent(textmodel_lss(dfm(toks_test), seedwords("pos-neg")[1], k = 10))
})


test_that("simil_method works", {

    lss_cos <- textmodel_lss(dfm(toks_test), seedwords("pos-neg")[1], features = feat_test,
                             k = 10)
    lss_cor <- textmodel_lss(dfm(toks_test), seedwords("pos-neg")[1], features = feat_test,
                             simil_method = "correlation", k = 10)

    expect_false(identical(lss_cos, lss_cor))
    expect_error(textmodel_lss(dfm(toks_test), seedwords("pos-neg")[1], features = feat_test,
                               simil_method = "something", k = 10), "'arg' should be one of")
})


test_that("include_data is working", {
    mt <- dfm(toks_test)
    lss <- textmodel_lss(mt, seedwords("pos-neg"), include_data = TRUE, k = 10)
    lss_nd <- textmodel_lss(mt, seedwords("pos-neg"), include_data = FALSE, k = 10)
    expect_error(predict(lss_nd), "LSS model includes no data")
    expect_identical(predict(lss), predict(lss_nd, newdata = mt))
})

test_that("predict.textmodel_lss retuns NA for empty documents", {

    mt <- dfm(data_corpus_inaugural)
    mt[c(3, 10),] <- 0
    pred <- predict(lss_test, newdata = as.dfm(mt))
    expect_equal(length(pred), ndoc(data_corpus_inaugural))
    expect_equal(pred[c("1789-Washington", "1797-Adams", "1825-Adams")],
                      c("1789-Washington" = -0.762, "1797-Adams" = NA, "1825-Adams" = NA),
                 tolerance = 0.01)

    pred2 <- predict(lss_test, newdata = as.dfm(mt), se.fit = TRUE)
    expect_equal(pred2$fit[c("1789-Washington", "1797-Adams", "1825-Adams")],
                 c("1789-Washington" = -0.762, "1797-Adams" = NA, "1825-Adams" = NA),
                 tolerance = 0.01)
    expect_equal(pred2$se.fit[c(1, 3, 10)], c(0.931129, NA, NA), tolerance = 0.01)
    expect_equal(pred2$n[c(1, 3, 10)], c(33, 0, 0))
})


test_that("textmodel_lss works with glob patterns", {
    mt <- dfm(toks_test)
    seed <- c("nice*" = 1, "positive*" = 1, "bad*" = -1, "negative*" = -1)
    lss <- textmodel_lss(mt, seed, k = 10)
    expect_equal(names(lss$seeds_weighted), names(seed))
    expect_equal(lengths(lss$seeds_weighted),
                 c("nice*" = 0, "positive*" = 2, "bad*" = 3, "negative*" = 1))
})

test_that("textmodel_lss works with non-existent seeds", {

    seed1 <- c("good" = 1, "bad" = -1, "xyz" = -1)
    expect_silent(textmodel_lss(dfmt_test, seed1, k = 10))

    seed2 <- c("xyz", "xxx")
    expect_error(textmodel_lss(dfmt_test, seed2, k = 10),
                 "No seed word is found in the dfm")
})

test_that("RSpectra and irlba work", {

    expect_silent(textmodel_lss(dfmt_test, seedwords("pos-neg"), k = 10, engine = "RSpectra"))
    expect_silent(textmodel_lss(dfmt_test, seedwords("pos-neg"), k = 10, engine = "irlba"))

    fcmt <- fcm(dfmt_test)
    expect_silent(textmodel_lss(fcmt, seedwords("pos-neg"), k = 10, engine = "RSpectra"))
    expect_silent(textmodel_lss(fcmt, seedwords("pos-neg"), k = 10, engine = "irlba"))
})

test_that("text2vec works", {
    dfmt <- dfm(toks_test)
    expect_error(textmodel_lss(dfmt, seedwords("pos-neg"), engine = "text2vec"),
                 "x must be a fcm for text2vec")
    fcmat <- fcm(toks_test)
    suppressMessages({
        lss <- textmodel_lss(fcmat, seedwords("pos-neg"), engine = "text2vec")
    })
    expect_equal(
        names(predict(lss, dfmt)),
        docnames(dfmt)
    )
    expect_error(
        predict(lss),
        "LSS model includes no data"
    )
    expect_true(setequal(names(coef(lss)), colnames(fcmat)))
})

test_that("d is working", {
    dfmt <- dfm(toks_test)
    lss1 <- textmodel_lss(dfmt, seedwords("pos-neg"), k = 10, d = 0)
    lss2 <- textmodel_lss(dfmt, seedwords("pos-neg"), k = 10, d = 1.0)
    expect_false(identical(lss1, lss2))
})

test_that("weight is working", {
    dfmt <- dfm(toks_test)

    lss1 <- textmodel_lss(dfmt, seedwords("pos-neg"), k = 10, weight = "count")
    lss2 <- textmodel_lss(dfmt, seedwords("pos-neg"), k = 10, weight = "logcount")
    expect_false(identical(lss1, lss2))
    expect_error(
        textmodel_lss(dfmt, seedwords("pos-neg"), k = 10, weight = "xxx")
    )
})

test_that("utils are working", {
    expect_equal(names(divergence(lss_test)),
                 c("within", "between", "diff"))
    expect_equal(names(discrimination(lss_test)),
                 c("document", "term"))
})

test_that("s argument is working", {
    expect_identical(
        dim(textmodel_lss(dfmt_test, seed, features = feat_test, k = 300, s = 100)$embedding),
        dim(textmodel_lss(dfmt_test, seed, features = feat_test, k = 300, s = 1:100)$embedding)
    )
    expect_silent(
        textmodel_lss(dfmt_test, seed, features = feat_test, k = 300, s = 1:100)
    )
    expect_error(
        textmodel_lss(dfmt_test, seed, features = feat_test, k = 300, s = 1:400),
        "s must be between 1 and k"
    )
})
