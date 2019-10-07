context("Who are you function")
library(packexplorer)

test_that("if not asked, no output is returned", {
  expect_equal(who_are_you('leaflet',return.map=F,plot.it=F),NULL)
})


test_that("numbers of packages in graph is same as in leaflet", {
    way = who_are_you('leaflet',return.map=T,plot.it=F)
    expect_equal(vcount(way$igraph.graph),nrow(leaflet::getMapData(way$leaflet.graph)))
})
