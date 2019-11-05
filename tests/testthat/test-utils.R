context("test utilities")

test_that("diagnosys works", {
    skip_on_travis()
    txt <- c("a b c d 0.2 . (xxx) \u2700", "a b_c 1st 2nd k100@gmail.com",
           "Aa bb cc. Xx yy zz.", "Aa bb cc. Xx yy zz.")
    dat <- data.frame(document = paste0("text", 1:4),
                    number =  c(1, 3, 0, 0),
                    punct  =  c(4, 2, 2, 2),
                    symbol =  c(1, 0, 0, 0),
                    any    =  c(5, 4, 2, 2),
                    n_sent =  c(1, 1, 2, 2),
                    n_token = c(10, 5, 8, 8),
                    dupli = c(FALSE, FALSE, FALSE, TRUE),
                    noise = c(0.5, 0.8, 0.25, 0.25),
                    stringsAsFactors = FALSE)
    expect_equal(diagnosys(txt), dat)

})

test_that("as.seedwords works", {
    lis1 <- list(c("a", "b", "c"), c("d", "e", "f"))
    expect_equal(as.seedwords(lis1),
                 c("a" = 1, "b" = 1, "c" = 1, "d" = -1, "e" = -1, "f" = -1))
    lis2 <- list(c("a", "b", "c"), c("d", "e", "f"))
    expect_equal(as.seedwords(lis2, upper = 2, lower = 1),
                 c("d" = 1, "e" = 1, "f" = 1, "a" = -1, "b" = -1, "c" = -1))
    lis3 <- list("pos" = c("a", "b", "c"), "neg" = c("d", "e", "f"))
    expect_equal(as.seedwords(lis3, upper = "pos", lower = "neg"),
                 c("a" = 1, "b" = 1, "c" = 1, "d" = -1, "e" = -1, "f" = -1))
})

