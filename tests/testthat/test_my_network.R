context("My Network function")
library(packexplorer)

test_that("all packages are mine", {
  expect_equal(length(setdiff(V(my_network()[['Suggests']])$name,my_packages())), 0)
  expect_equal(length(setdiff(V(my_network()[['Depends']])$name,my_packages())), 0)
  expect_equal(length(setdiff(V(my_network()[['Imports']])$name,my_packages())), 0)
  expect_equal(length(setdiff(V(my_network()[['Enhances']])$name,my_packages())), 0)
})

