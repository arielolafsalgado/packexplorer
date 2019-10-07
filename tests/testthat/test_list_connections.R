context("List connections function")
library(packexplorer)

test_that("number of rows is the same as 3*number of edges", {
  require(igraph)
  g = make_star(n=10)
  V(g)$name = letters[1:10]
  expect_equal(nrow(list_connections(g,layout_nicely(g))),3*ecount(g))
})
