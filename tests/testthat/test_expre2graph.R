context("Expression to graph function")
library(packexplorer)

test_that("if not asked, no output is returned", {
  expect_equal(expre2graph('info',return.map=F,plot.it=F),NULL)
})

test_that("numbers of packages in graph is same as mentions", {
    e2g = expre2graph('info',return.map=T,plot.it=F)
    expect_equal(vcount(e2g$igraph.graph),e2g$nmentions)
    expect_equal(vcount(e2g$igraph.graph),nrow(leaflet::getMapData(e2g$leaflet.graph)))
})
