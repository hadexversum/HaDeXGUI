test_that("first works", {
  expect_equal(first(1:5), 1)
  expect_equal(first(LETTERS), "A")

  # edge cases
  expect_error(first(integer(0)), "subscript out of bounds")
  expect_equal(first(NULL), NULL)
})

test_that("last works", {
  expect_equal(last(1:5), 5)
  expect_equal(last(LETTERS), "Z")

  # edge cases
  expect_error(last(integer(0)), "attempt to select less than one element")
  expect_equal(last(NULL), NULL)
})

test_that("middle works", {
  expect_equal(middle(1:6), 3)
  expect_equal(middle(1:5), 3)
  expect_equal(middle(LETTERS), "M")

  # edge cases
  expect_error(middle(integer(0)), "attempt to select less than one element")
  expect_equal(middle(NULL), NULL)

})

test_that("not_null works", {
  expect_true(not_null(1))
  expect_false(not_null(NULL))
})

test_that("%?>% works", {
  expect_equal(
    NULL %?>% exp,
    NULL
  )
  expect_equal(
    1 %?>% exp,
    exp(1)
  )
})

