
toks_test <- readRDS("../data/tokens_test.RDS")
feat_test <- head(char_keyness(toks_test, "america*", min_count = 1, p = 0.05), 100)
dfmt_test <- dfm(toks_test)
seed_test <- as.seedwords(data_dictionary_sentiment)
lss_test <- textmodel_lss(dfmt_test, seed_test, terms = feat_test, k = 300)

test_that("diagnosys works", {
    skip_on_travis()
    txt <- c("a b c d 0.2 . (xxx) \u2700", "a b_c 1st 2nd k100@gmail.com",
           "Aa bb cc. Xx yy zz.", "Aa bb cc. Xx yy zz.")
    dat <- data.frame(doc_id = paste0("text", 1:4),
                    number =  c(1, 3, 0, 0),
                    punct  =  c(4, 2, 2, 2),
                    symbol =  c(1, 0, 0, 0),
                    any    =  c(5, 4, 2, 2),
                    n_sent =  c(1, 1, 2, 2),
                    n_token = c(10, 5, 8, 8),
                    dupli = c(FALSE, FALSE, FALSE, TRUE),
                    noise = c(0.5, 0.8, 0.25, 0.25),
                    stringsAsFactors = FALSE)
    suppressWarnings(
        expect_equal(diagnosys(txt), dat)
    )
    expect_warning(diagnosys(txt),
                   "'diagnosys.corpus' is deprecated")

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
    lis4 <- list("pos" = c("a", "a"), "neg" = c("b", "b"))
    expect_equal(as.seedwords(lis4, upper = "pos", lower = "neg"),
                 c("a" = 1, "b" = -1))
    lis5 <- list("pos1" = c("a", "b"), "pos2" = c("c"), "neg" = c("d", "e", "f"))
    expect_equal(as.seedwords(lis5, upper = c("pos1", "pos2"), lower = "neg"),
                 c("a" = 1, "b" = 1, "c" = 1, "d" = -1, "e" = -1, "f" = -1))

    dict1 <- dictionary(lis3)
    expect_equal(as.seedwords(dict1, upper = "pos", lower = "neg"),
                 c("a" = 1, "b" = 1, "c" = 1, "d" = -1, "e" = -1, "f" = -1))
    expect_error(as.seedwords(data.frame(1:3)), "x must be a list or dictionary object")

    dict2 <- dictionary(list("pos" = "very good", "neg" = "very bad"))
    expect_equal(as.seedwords(dict2),
                 c("very_good" = 1, "very_bad" = -1))
    expect_equal(as.seedwords(dict2, concatenator = "+"),
                 c("very+good" = 1, "very+bad" = -1))

})

test_that("cohesion works", {
    coh <- cohesion(lss_test)
    expect_identical(names(coh), c("k", "raw", "smoothed"))
    expect_identical(nrow(coh), lss_test$k)
})

test_that("strength works", {
    lis <- strength(lss_test)
    expect_identical(names(lis), c("overall", "element"))
    expect_identical(names(lis$element), c("seed", "selected", "all"))
    expect_identical(nrow(lis$element), length(unlist(lss_test$seeds_weighted)))

    lss1 <- textmodel_lss(dfmt_test, seed_test, terms = feat_test, k = 300, slice = 100)
    lss2 <- textmodel_lss(dfmt_test, seed_test, terms = feat_test, k = 300)
    expect_true(all(strength(lss1)$overall != strength(lss2)$overall))
})

