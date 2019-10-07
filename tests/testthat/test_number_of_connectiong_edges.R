context("Number of connecting edges function")
library(packexplorer)

test_that("the number of connecting edges is the same", {
  require(igraph)
  g = make_empty_graph(n=2)
  V(g)$name = letters[1:2]
  expect_equal(number_of_connecting_edges(g,'a','b'),c('a'=0))
  g = add_edges(g,letters[c(1,2)])
  expect_equal(number_of_connecting_edges(g,'a','b'),c('a'=1))
  g = add_vertices(g,nv=1,name=letters[3])
  g = add_edges(g,letters[c(1,3)])
  expect_equal(number_of_connecting_edges(g,letters[c(1,3)],letters[2]),c('a'=1,'c'=0))
  g = add_edges(g,letters[c(2,3)])
  expect_equal(number_of_connecting_edges(g,letters[c(1,3)],letters[2]),c('a'=1,'c'=1))
})

