test_that("set.salt establishes a locally scoped salt", {
  salt <- 8675309
  set.salt(salt)
  expect_identical(salt, .hash.salt) # the same when in scope
  set.salt(42)
  expect_false(salt == .hash.salt) # not after change w/in scope
  (function() {
    set.salt(salt)
    expect_identical(salt, .hash.salt) # same in scope
  })()
  expect_false(salt == .hash.salt) # not after change out of scope
})


test_that("same salt + same object => same draws", {

  salt <- 8675309
  set.salt(salt)
  expect_identical(salt, .hash.salt)

  evt1 <- list(type = 1L, from = 1L, to = 2L, time = 3.14)
  evt2 <- list(type = 1L, from = 1L, to = 2L, time = 3.14)

  expect_no_error(set.hash(evt1, .hash.salt))

  draw1 <- runif(5, hash = evt1)
  draw2 <- runif(5, hash = evt2)

  expect_identical(draw1, draw2)
})

test_that("diff salt + same object => diff draws", {
  evt1 <- list(type = 1L, from = 1L, to = 2L, time = 3.14)
  set.salt(8675309)
  draw1 <- runif(5, hash = evt1)
  set.salt(42)
  draw2 <- runif(5, hash = evt1)
  expect_false(all(draw1 == draw2))
})

test_that("same salt + diff object => diff draws", {
  evt1 <- list(type = 1L, from = 1L, to = 2L, time = 3.14)
  evt2 <- list(type = 1L, from = 1L, to = 2L, time = 4.14)
  set.salt(8675309)
  draw1 <- runif(5, hash = evt1)
  draw2 <- runif(5, hash = evt2)
  expect_false(all(draw1 == draw2))
})
