test_that("same salt + same object => same draws", {
  evt1 <- list(type = "infection", from = 1, to = 2, time = 3.14)
  evt2 <- list(type = "infection", from = 1, to = 2, time = 3.14)
  salt <- 8675309
  hash_seed(salt, evt1$type, evt1$from, evt1$to, as.integer(evt1$time))
  draw1 <- runif(10)
  hash_seed(salt, evt2$type, evt2$from, evt2$to, as.integer(evt2$time))
  draw2 <- runif(10)
  expect_identical(draw1, draw2)
})

test_that("diff salt + same object => diff draws", {
  evt1 <- list(type = "infection", from = 1, to = 2, time = 3.14)
  evt2 <- list(type = "infection", from = 1, to = 2, time = 3.14)
  salt <- 8675309
  hash_seed(salt, evt1$type, evt1$from, evt1$to, as.integer(evt1$time))
  draw1 <- runif(10)
  salt <- 42
  hash_seed(salt, evt2$type, evt2$from, evt2$to, as.integer(evt2$time))
  draw2 <- runif(10)
  expect_false(all(draw1==draw2))
})

test_that("same salt + diff object => diff draws", {
  evt1 <- list(type = "infection", from = 1, to = 2, time = 3.14)
  evt2 <- list(type = "infection", from = 1, to = 2, time = 4.14)
  salt <- 8675309
  hash_seed(salt, evt1$type, evt1$from, evt1$to, as.integer(evt1$time))
  draw1 <- runif(10)
  hash_seed(salt, evt2$type, evt2$from, evt2$to, as.integer(evt2$time))
  draw2 <- runif(10)
  expect_false(all(draw1==draw2))
})
