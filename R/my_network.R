#' Your own R-Network
#'
#' This function gives you your own package network, based on the avaiable general networks.
#' @return A list with igraph objects, one for each input graph.
#' @export
#' @examples
#' g_list = my_network()
#' plot(g_list[['Suggests']])
#' @importFrom igraph induced_subgraph V

my_network <- function(){
	my_packs = rownames(utils::installed.packages())
	return(list(
		'Suggests'= induced_subgraph(gs,V(gs)[is.element(V(gs)$name,my_packs)]),
		'Depends' = induced_subgraph(gd,V(gd)[is.element(V(gd)$name,my_packs)]),
		'Imports'= induced_subgraph(gi,V(gi)[is.element(V(gi)$name,my_packs)]),
		'Enhances' = induced_subgraph(ge,V(ge)[is.element(V(ge)$name,my_packs)])))

}

