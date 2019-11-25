context("Plot neighbors function")
library(packexplorer)

test_that("if not asked, no output is returned", {
  expect_equal(plot_neighbors(return.map=F,plot.it=F),NULL)
})


test_that("numbers of packages in graph is same my_packages plus recommended packages", {
    pn = plot_neighbors(return.map=T,plot.it=F)
    g = pn$igraph.graph
    my_packs = my_packages()
    reco.me = recommend_me()
    expect_equal(length(setdiff(V(g)$name,c(my_packs,names(reco.me)))),0)
})
