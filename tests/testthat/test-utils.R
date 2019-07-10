test_that("diagnosys works", {
    skip_on_travis()
    txt <- c("a b c d 123 . (xxx) \u2700", "a b_c 1st 2nd xxx@gmail.com",
           "Aa bb cc. Xx yy zz.", "Aa bb cc. Xx yy zz.")
    dat <- data.frame(document = paste0("text", 1:4),
                    number = c(1, 2, 0, 0),
                    punct = c(3, 2, 2, 2),
                    symbol = c(1, 0, 0, 0),
                    n_sent = c(1, 1, 2, 2),
                    n_token = c(10, 5, 8, 8),
                    dupli = c(FALSE, FALSE, FALSE, TRUE),
                    noise = c(0.5, 0.8, 0.25, 0.25),
                    stringsAsFactors = FALSE)
    expect_equal(diagnosys(txt), dat)

})
