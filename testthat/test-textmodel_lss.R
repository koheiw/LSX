require(quanteda)

corp_sent <- corpus_reshape(data_corpus_inaugural, 'sentence')
toks <- tokens(corp_sent, remove_punct = TRUE)
feat <- head(char_keyness(toks, 'america*', min_count = 1, p = 0.01), 100)
test_lss <- textmodel_lss(dfm(toks), seedwords('pos-neg'), features = feat, k = 300)

test_that("char_keyness is working", {

    expect_equal(length(feat), 100)

})

test_that("textmodel_lss has all the attributes", {

    expect_equal(
        names(test_lss),
        c("beta", "data", "features", "seeds", "seeds_weighted", "call")
    )

    expect_true(is.numeric(test_lss$beta))
    expect_true(is.dfm(test_lss$data))
    expect_identical(test_lss$features, feat)
    expect_identical(names(test_lss$seeds), names(seedwords('pos-neg')))

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

    pred2 <- predict(test_lss, fit.se = TRUE)
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

    pred5 <- predict(test_lss, fit.se = TRUE, density = TRUE)
    expect_equal(names(pred5), c('fit', 'se.fit', 'n', 'density'))

})

test_that("density is correct", {

    test_dfm <- dfm(data_corpus_inaugural)
    pred <- predict(test_lss, newdata = test_dfm, density = TRUE)

    expect_equal(
        pred$density,
        unname(rowSums(dfm_select(dfm_weight(test_dfm, 'prop'), feat)))
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
