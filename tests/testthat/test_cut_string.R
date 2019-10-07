context("Cut String function")
library(packexplorer)

test_that("NULL of (0) text gets empty string", {
  expect_equal(cut_string(NULL), '')
  expect_equal(cut_string(character(0)), '')
  expect_equal(cut_string(numeric(0)), '')
  expect_equal(cut_string(NA), '')
})


test_that("0 nwords get same text", {
  expect_equal(cut_string('This is a string',0), 'This is a string')
  expect_equal(cut_string('This is a string',10), 'This is a string')
})

test_that("this breaks are the same",{
  expect_equal(cut_string('This is a string',1), "<br/>This<br/>is<br/>a<br/>string<br/></p>")
  expect_equal(cut_string('This is a string',2), "<br/>This is<br/>a string<br/></p>")
  expect_equal(cut_string('This is a string',3), "<br/>This is a<br/>string</p>")
})