test_that("capitalize works", {
  expect_equal(
    capitalize("qwerty"),
    "Qwerty"
  )
  expect_equal(
    capitalize("Qwerty"),
    "Qwerty"
  )
})

test_that("decapitalize works", {
  expect_equal(
    decapitalize("Qwerty"),
    "qwerty"
  )
  expect_equal(
    decapitalize("qwerty"),
    "qwerty"
  )
})

test_that("idize works", {
  expect_equal(
    idize("a short sentence"),
    "a_short_sentence"
  )
})
