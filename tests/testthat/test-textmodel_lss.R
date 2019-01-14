
corp_sent <- corpus_reshape(data_corpus_inaugural, "sentence")
test_toks <- tokens(corp_sent, remove_punct = TRUE)
test_feat <- head(char_keyness(test_toks, "america*", min_count = 1, p = 0.05), 100)
test_mt <- dfm(test_toks)
test_lss <- textmodel_lss(test_mt, seedwords("pos-neg"), features = test_feat, k = 300)
test_lss_nd <- textmodel_lss(test_mt, seedwords("pos-neg"), features = test_feat, k = 300,
                             include_data = FALSE)

test_that("char_keyness is working", {

    expect_identical(length(test_feat), 100L)
    feat1 <- char_keyness(test_toks, "america.*", "regex", min_count = 1, p = 0.05)
    expect_identical(head(feat1, 100), test_feat)

    feat2 <- char_keyness(test_toks, "America*", case_insensitive = FALSE, min_count = 1, p = 0.05)
    expect_identical(head(feat2, 100), test_feat)

    feat3 <- char_keyness(test_toks, "america*", min_count = 1000, remove_pattern = TRUE)
    expect_identical(feat3, character())

    feat4 <- char_keyness(test_toks, "america*", min_count = 1000, remove_pattern = FALSE)
    expect_identical(feat4, character())

    expect_error(char_keyness(test_toks, "xxxxx", min_count = 1, p = 0.05),
                 "xxxxx is not found")

})

test_that("char_keyness removes multi-word target", {

    feat_rp <- char_keyness(test_toks, phrase("united states"), "regex",
                            min_count = 1, p = 0.05)
    expect_identical(c("united", "states") %in% feat_rp,
                     c(FALSE, FALSE))

    feat_kp <- char_keyness(test_toks, phrase("united states"), "regex",
                            min_count = 1, p = 0.05, remove_pattern = FALSE)
    expect_identical(c("united", "states") %in% feat_kp,
                     c(TRUE, TRUE))
})

test_that("textmodel_lss has all the attributes", {

    expect_equal(
        names(test_lss),
        c("beta", "features", "seeds", "seeds_weighted", "seeds_distance",
          "correlation", "call", "data")
    )

    expect_true(is.numeric(test_lss$beta))
    expect_true(is.dfm(test_lss$data))
    expect_identical(test_lss$features, test_feat)
    expect_identical(names(test_lss$seeds), names(seedwords("pos-neg")))

    expect_equal(
        names(test_lss_nd),
        c("beta", "features", "seeds", "seeds_weighted", "seeds_distance",
          "correlation", "call")
    )

})

test_that("summary.textmodel_lss is working", {

    expect_silent(summary(test_lss))
    expect_silent(summary(test_lss_nd))

})



test_that("predict.textmodel_lss is working", {

    pred1 <- predict(test_lss)
    expect_equal(length(pred1), ndoc(test_mt))
    expect_identical(names(pred1), docnames(test_mt))
    expect_true(is.numeric(pred1))
    expect_equal(mean(pred1, na.rm = TRUE), 0)
    expect_equal(sd(pred1, na.rm = TRUE), 1)

    pred2 <- predict(test_lss, se.fit = TRUE)
    expect_equal(length(pred2$fit), ndoc(test_mt))
    expect_identical(names(pred2$fit), docnames(test_mt))
    expect_equal(length(pred2$se.fit), ndoc(test_mt))
    expect_equal(length(pred2$n), ndoc(test_mt))
    expect_null(names(pred2$se.fit))
    expect_null(names(pred2$n))

    pred3 <- predict(test_lss, density = TRUE)
    expect_equal(length(pred3$density), ndoc(test_toks))
    expect_null(names(pred3$density))

    pred4 <- predict(test_lss, rescaling = FALSE)
    expect_identical(names(pred4), docnames(test_toks))
    expect_equal(as.numeric(scale(pred4)), unname(pred1))

    pred5 <- predict(test_lss, se.fit = TRUE, density = TRUE)
    expect_equal(names(pred5), c("fit", "se.fit", "n", "density"))

})

test_that("density is correct", {

    test_dfm <- dfm(data_corpus_inaugural)
    pred <- predict(test_lss, newdata = test_dfm, density = TRUE)

    expect_equal(
        pred$density,
        unname(rowSums(dfm_select(dfm_weight(test_dfm, "prop"), test_feat)))
    )
})

test_that("predict.textmodel_lss works with newdata", {

    pred <- predict(test_lss, newdata = dfm(data_corpus_inaugural))
    expect_equal(length(pred), ndoc(data_corpus_inaugural))

})

test_that("data object is valid", {
    sum <- summary(data_textmodel_lss_russianprotests)
    expect_equal(class(sum), c("summary.textmodel", "list"))
})

test_that("calculation of fit and se.fit are correct", {

    lss <- LSS:::as.textmodel_lss(c("a" = 0.1, "b" = 0.1, "c" = 0.3))
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

    expect_silent(textmodel_lss(dfm(test_toks), seedwords("pos-neg")[1], features = test_feat, k = 300))
    expect_silent(textmodel_lss(dfm(test_toks), seedwords("pos-neg")[1], features = character(), k = 300))
    expect_silent(textmodel_lss(dfm(test_toks), seedwords("pos-neg")[1], k = 300))
})


test_that("simil_method works", {

    lss_cos <- textmodel_lss(dfm(test_toks), seedwords("pos-neg")[1], features = test_feat)
    lss_cor <- textmodel_lss(dfm(test_toks), seedwords("pos-neg")[1], features = test_feat,
                             simil_method = "correlation")

    expect_false(identical(lss_cos, lss_cor))
    expect_error(textmodel_lss(dfm(test_toks), seedwords("pos-neg")[1], features = test_feat,
                               simil_method = "something"), "'arg' should be one of")
})


test_that("include_data is working", {
    mt <- dfm(test_toks)
    lss <- textmodel_lss(mt, seedwords("pos-neg"), include_data = TRUE)
    lss_nd <- textmodel_lss(mt, seedwords("pos-neg"), include_data = FALSE)
    expect_error(predict(lss_nd), "LSS model includes no data")
    expect_identical(predict(lss), predict(lss_nd, newdata = mt))
})

test_that("predict.textmodel_lss retuns NA for empty documents", {

    mt <- dfm(data_corpus_inaugural)
    mt[c(3, 10),] <- 0
    pred <- predict(test_lss, newdata = as.dfm(mt))
    expect_equal(length(pred), ndoc(data_corpus_inaugural))
    expect_equal(pred[c("1789-Washington", "1797-Adams", "1825-Adams")],
                      c("1789-Washington" = 0.42207451, "1797-Adams" = NA, "1825-Adams" = NA),
                 tolerance = 0.01)

    pred2 <- predict(test_lss, newdata = as.dfm(mt), se.fit = TRUE)
    expect_equal(pred2$fit[c("1789-Washington", "1797-Adams", "1825-Adams")],
                 c("1789-Washington" = 0.42207451, "1797-Adams" = NA, "1825-Adams" = NA))
    expect_equal(pred2$se.fit[c(1, 3, 10)], c(0.6426934, NA, NA), tolerance = 0.01)
    expect_equal(pred2$n[c(1, 3, 10)], c(33, 0, 0))
})


test_that("textmodel_lss works with glob patterns", {
    mt <- dfm(test_toks)
    seed <- c("nice*" = 1, "positive*" = 1, "bad*" = -1, "negative*" = -1)
    lss <- textmodel_lss(mt, seed, k = 300)
    expect_equal(names(lss$seeds_weighted), names(seed))
    expect_equal(lengths(lss$seeds_weighted),
                 c("nice*" = 0, "positive*" = 2, "bad*" = 3, "negative*" = 1))
    expect_equal(class(textplot_proxy(lss)), c("gg", "ggplot"))
    expect_equal(class(textplot_proxy(lss, group = FALSE)), c("gg", "ggplot"))
})
