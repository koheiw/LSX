require(quanteda)

lss_test <- readRDS("../data/lss_test.RDS")

test_that("bootstrap_lss works", {
    bs1 <- bootstrap_lss(lss_test, "seeds")
    expect_true(is.character(as.vector(bs1)))
    expect_equal(class(as.vector(bs1)), "character")
    expect_equal(ncol(bs1), 12)
    expect_equal(nrow(bs1), length(lss_test$beta))
    expect_equal(attr(bs1, "values"), names(lss_test$seeds_weighted))

    bs2 <- bootstrap_lss(lss_test, what = "k")
    expect_equal(class(as.vector(bs2)), "character")
    expect_equal(ncol(bs2), 6)
    expect_equal(nrow(bs2), length(lss_test$beta))
    expect_equal(attr(bs2, "values"), seq(50, 300, 50))

    bs3 <- bootstrap_lss(lss_test, mode = "coef")
    expect_equal(class(as.vector(bs3)), "numeric")
    expect_equal(ncol(bs3), 12)
    expect_equal(nrow(bs3), length(lss_test$beta))
    expect_equal(attr(bs3, "values"), names(lss_test$seeds_weighted))

    bs4 <- bootstrap_lss(lss_test, what = "k", by = 10)
    expect_equal(ncol(bs4), 26)
    expect_equal(attr(bs4, "values"), seq(50, 300, 10))

    bs5 <- bootstrap_lss(lss_test, what = "k", from = 100, to = 200, by = 10)
    expect_equal(ncol(bs5), 11)
    expect_equal(attr(bs5, "values"), seq(100, 200, 10))

    expect_error(bootstrap_lss(lss_test, what = "k", from = 0),
                 "The value of from must be between 1 and 300")
    expect_error(bootstrap_lss(lss_test, what = "k", to = 0),
                 "The value of to must be between 1 and 300")
    expect_error(bootstrap_lss(lss_test, what = "k", by = -1),
                 "The value of by must be between 1 and 250")
    expect_error(bootstrap_lss(lss_test, what = "k", by = 1000),
                 "The value of by must be between 1 and 250")

})

test_that("bootstrap_lss show messages", {

    expect_silent(
        bootstrap_lss(lss_test, "k", verbose = FALSE)
    )
    expect_output(
        bootstrap_lss(lss_test, "seeds", verbose = TRUE),
        "Fitting textmodel_lss with a different hyper-parameter.*"
    )
    expect_output(
        bootstrap_lss(lss_test, "k", verbose = TRUE),
        "Fitting textmodel_lss with a different hyper-parameter.*"
    )
})

