#' Your own R-Network
#'
#' This function gives you your own package network, based on the avaiable general networks.
#' @return A list with igraph objects, one for each input graph.
#' @export
#' @examples
#' g = my_network()
#' plot(g)
#' importFrom igraph induced_subgraph V

my_network <- function(){
	data('dependsGraph')
	data('suggestsGraph')	
	data('importsGraph')
	data('enhancesGraph')	
	my_packs = rownames(installed.packages())
	return(list(
		'Sug'= induced_subgraph(gs,V(gs)[is.element(V(gs)$name,my_packs)]),
		'Dep' = induced_subgraph(gd,V(gd)[is.element(V(gd)$name,my_packs)]),
		'Imp'= induced_subgraph(gi,V(gi)[is.element(V(gi)$name,my_packs)]),
		'Enh' = induced_subgraph(ge,V(ge)[is.element(V(ge)$name,my_packs)])))

}

