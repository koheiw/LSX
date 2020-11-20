context("test textstat_context")

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

    feat_kp <- char_keyness(toks_test, phrase("united states"),
                            min_count = 1, p = 0.05, window = 0, remove_pattern = FALSE)
    expect_identical(feat_kp, "united")
})
