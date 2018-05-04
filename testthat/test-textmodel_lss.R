require(quanteda)

corp_sent <- corpus_reshape(data_corpus_inaugural, "sentence")
toks <- tokens(corp_sent, remove_punct = TRUE)
feat <- head(char_keyness(toks, "america*", min_count = 1, p = 0.01), 100)
test_lss <- textmodel_lss(dfm(toks), seedwords("pos-neg"), features = feat, k = 300)

test_that("char_keyness is working", {

    expect_identical(length(feat), 100L)
    expect_error(char_keyness(toks, "xxxxx", min_count = 1, p = 0.01),
                 "xxxxx is not found")
    expect_identical(char_keyness(toks, "america*", min_count = 100, remove_pattern = TRUE),
                     character())
    expect_warning(char_keyness(toks, "america*", min_count = 100, remove_pattern = FALSE),
                   character())
})

test_that("textmodel_lss has all the attributes", {

    expect_equal(
        names(test_lss),
        c("beta", "data", "features", "seeds", "seeds_weighted", "call")
    )

    expect_true(is.numeric(test_lss$beta))
    expect_true(is.dfm(test_lss$data))
    expect_identical(test_lss$features, feat)
    expect_identical(names(test_lss$seeds), names(seedwords("pos-neg")))

})

test_that("summary.textmodel_lss is working", {

    expect_silent(summary(test_lss))

})



test_that("predict.textmodel_lss is working", {

    pred1 <- predict(test_lss)
    expect_equal(length(pred1), ndoc(toks))
    expect_identical(names(pred1), docnames(toks))
    expect_true(is.numeric(pred1))
    expect_equal(sd(pred1), 1)

    pred2 <- predict(test_lss, se.fit = TRUE)
    expect_equal(length(pred2$fit), ndoc(toks))
    expect_equal(length(pred2$se.fit), ndoc(toks))
    expect_equal(length(pred2$n), ndoc(toks))
    expect_null(names(pred3$se.fit))
    expect_null(names(pred3$n))

    pred3 <- predict(test_lss, density = TRUE)
    expect_equal(length(pred3$density), ndoc(toks))
    expect_null(names(pred3$density))

    pred4 <- predict(test_lss, rescaling = FALSE)
    expect_identical(names(pred4), docnames(toks))
    expect_equal(as.numeric(scale(pred4)), unname(pred1))

    pred5 <- predict(test_lss, se.fit = TRUE, density = TRUE)
    expect_equal(names(pred5), c("fit", "se.fit", "n", "density"))

})

test_that("density is correct", {

    test_dfm <- dfm(data_corpus_inaugural)
    pred <- predict(test_lss, newdata = test_dfm, density = TRUE)

    expect_equal(
        pred$density,
        unname(rowSums(dfm_select(dfm_weight(test_dfm, "prop"), feat)))
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
                 sqrt(sum(as.numeric(mt_prop[1,]) * (pred$fit[1] - beta) ^ 2)) / sqrt(rowSums(mt_sub)[1]))
    expect_equal(pred$se.fit[2],
                 sqrt(sum(as.numeric(mt_prop[2,]) * (pred$fit[2] - beta) ^ 2)) / sqrt(rowSums(mt_sub)[2]))
    expect_equal(pred$se.fit[3],
                 sqrt(sum(as.numeric(mt_prop[3,]) * (pred$fit[3] - beta) ^ 2)) / sqrt(rowSums(mt_sub)[3]))

    expect_equal(pred$n[1], 3)
    expect_equal(pred$n[2], 2)
    expect_equal(pred$n[3], 5)

})


test_that("as.textmodel_lss works with only with single seed", {

    expect_silent(textmodel_lss(dfm(toks), seedwords("pos-neg")[1], features = feat, k = 300))
    expect_silent(textmodel_lss(dfm(toks), seedwords("pos-neg")[1], features = character(), k = 300))
    expect_silent(textmodel_lss(dfm(toks), seedwords("pos-neg")[1], k = 300))
})


test_that("simil_method words", {

    lss_cos <- textmodel_lss(dfm(toks), seedwords("pos-neg")[1], features = feat)
    lss_cor <- textmodel_lss(dfm(toks), seedwords("pos-neg")[1], features = feat,
                             simil_method = "correlation")

    expect_false(identical(lss_cos, lss_cor))
    expect_error(textmodel_lss(dfm(toks), seedwords("pos-neg")[1], features = feat,
                               simil_method = "something"))
})
