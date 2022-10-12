require(quanteda)

lss_test <- readRDS("../data/lss_test.RDS")

test_that("bootstrap_lss works for k", {

    bs1 <- bootstrap_lss(lss_test, what = "k")
    expect_equal(names(bs1), c("k", "beta"))
    expect_equal(ncol(bs1$beta), 10)
    expect_equal(bs1$k, seq(30, 300, by = 30))

    bs2 <- bootstrap_lss(lss_test, what = "k", param = c(100, 150, 200))
    expect_equal(names(bs2), c("k", "beta"))
    expect_equal(colnames(bs2$beta), c("100", "150", "200"))
    expect_equal(bs2$k, c(100, 150, 200))

    expect_error(
        bootstrap_lss(lss_test, what = "k", n = 500),
        "The value of n must be between 1 and 300"
    )

})


test_that("bootstrap_lss works for slice", {

    bs2 <- bootstrap_lss(lss_test, what = "slice", n = 10)
    expect_equal(names(bs2), c("slice", "beta"))
    expect_error(
        bootstrap_lss(lss_test, what = "slice", n = 0),
        "The value of size must be between 2 and 300"
    )

    expect_error(
        bootstrap_lss(lss_test, what = "slice", n = 0),
        "The value of n must be between 1 and Inf"
    )

})

