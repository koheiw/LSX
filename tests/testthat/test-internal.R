test_that("expand_seeds works", {

  seed <- c("a*" = 1, "b*" = 1, "c*" = -1, "d*" = -1)
  seed2 <- c("a*" = 2, "b*" = 1, "c*" = -2, "d*" = -1)
  type <- c("apple", "ball", "bag", "chair", "car", "cat", "dog")

  # bipolar
  expect_equal(LSX:::expand_seeds(seed, type),
              list("a*" = c("apple" = 0.5),
                   "b*" = c("ball" = 0.25, "bag" = 0.25),
                   "c*" = c("chair" = -0.166, "car" = -0.166, "cat" = -0.166),
                   "d*" = c("dog" = -0.5)),
              tolerance = 0.01)

  expect_equal(LSX:::expand_seeds(seed, type, adjust_weight = FALSE),
               list("a*" = c("apple" = 0.333),
                    "b*" = c("ball" = 0.333, "bag" = 0.333),
                    "c*" = c("chair" = -0.25, "car" = -0.25, "cat" = -0.25),
                    "d*" = c("dog" = -0.25)),
               tolerance = 0.01)

  expect_equal(LSX:::expand_seeds(seed2, type, adjust_weight = FALSE),
               list("a*" = c("apple" = 0.666),
                    "b*" = c("ball" = 0.333, "bag" = 0.333),
                    "c*" = c("chair" = -0.5, "car" = -0.5, "cat" = -0.5),
                    "d*" = c("dog" = -0.25)),
               tolerance = 0.01)

  # unipolar
  expect_equal(LSX:::expand_seeds(seed[1:2], type),
               list("a*" = c("apple" = 0.5),
                    "b*" = c("ball" = 0.25, "bag" = 0.25)),
               tolerance = 0.01)

  expect_equal(LSX:::expand_seeds(seed[1:2], type, adjust_weight = FALSE),
               list("a*" = c("apple" = 0.333),
                    "b*" = c("ball" = 0.333, "bag" = 0.333)),
               tolerance = 0.01)

  expect_equal(LSX:::expand_seeds(seed2[1:2], type, adjust_weight = FALSE),
               list("a*" = c("apple" = 0.666),
                    "b*" = c("ball" = 0.333, "bag" = 0.333)),
               tolerance = 0.01)

})
