
require(quanteda)
toks_test <- readRDS("../data/tokens_test.RDS")

test_that("textstat_context works", {

    char <- char_context(toks_test, phrase("united states"),
                         min_count = 1, window = 10)
    dat <- textstat_context(toks_test, phrase("united states"),
                            min_count = 1, window = 10)

    expect_identical(head(dat$feature, 10), head(char, 10))
    expect_identical(names(dat), c("feature", "chi2", "p", "n_inside", "n_outside"))
})

test_that("char_context removes multi-word target", {

    key_rp <- textstat_context(toks_test, phrase("united states"),
                               min_count = 1, window = 0)
    expect_equal(nrow(key_rp), 0)
    suppressWarnings({
        feat_rp <- char_context(toks_test, phrase("united states"),
                                min_count = 1, p = 0.05, window = 0)
    })
    expect_equal(length(feat_rp), 0)

    key_kp <- textstat_context(toks_test, phrase("united states"),
                               min_count = 1, window = 0, remove_pattern = FALSE)
    expect_equal(nrow(key_kp), 2)

    feat_kp <- char_context(toks_test, phrase("united states"),
                            min_count = 1, p = 0.05, window = 0, remove_pattern = FALSE)
    expect_identical(feat_kp, c("united", "states"))
})

test_that("char_context removes multi-word target", {

    # unigram
    txt <- "a a b b z b c c d d"
    toks <- tokens(txt)
    cont_uni <- textstat_context(toks, "z", window = 2, min_count = 0)
    dfmt_uni <- tokens(c(inside = "b b b c", outside = "a a c d d")) %>% dfm()
    key_uni <- textstat_keyness(dfmt_uni)
    expect_equivalent(cont_uni, key_uni)

    # bigram
    cont_bi <- textstat_context(toks, "z", window = 2, min_count = 0, n = 2)
    dfmt_bi <- tokens(c(inside = "b b b c", outside = "a a c d d")) %>%
        tokens_ngrams(n = 2) %>%
        dfm()
    key_bi <- textstat_keyness(dfmt_bi)
    expect_equivalent(cont_bi, cont_bi)
})
