context("utils")

test_that("starts_with() and ends_with() work as expected", {

  expect_true(starts_with("abc", ""))
  expect_true(starts_with("abc", "a"))
  expect_true(starts_with("abc", "ab"))
  expect_true(starts_with("abc", "abc"))
  expect_false(starts_with("abc", "abcd"))

  expect_false(ends_with("abc", "abcd"))
  expect_true(ends_with("abc", "abc"))
  expect_true(ends_with("abc", "bc"))
  expect_true(ends_with("abc", "c"))
  expect_true(ends_with("abc", ""))

})
