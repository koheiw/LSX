require(quanteda)

lss_test <- readRDS("../data/lss_test.RDS")

test_that("bootstrap_lss works", {

    bs1 <- bootstrap_lss(lss_test, what = "k", by = 300)
    expect_equal(names(bs1), c("k", "beta", "terms", "strength"))

    expect_error(
        bootstrap_lss(lss_test, what = "k", by = 1000),
        "The value of by must be between 1 and 300"
    )
    bs2 <- bootstrap_lss(lss_test, what = "slice", n = 1)
    expect_equal(names(bs2), c("slice", "beta", "terms", "strength"))
    expect_error(
        bootstrap_lss(lss_test, what = "slice", size = 500),
        "The value of size must be between 2 and 300"
    )

    expect_error(
        bootstrap_lss(lss_test, what = "slice", n = 0),
        "The value of n must be between 1 and Inf"
    )

})
