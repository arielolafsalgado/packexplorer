#' Get the number of conecting edges between two groups
#'
#' It calculates the number of edges from group1 to group2
#' @param g The graph object
#' @param group1 The departure set of nodes
#' @param group2 The arrival set of nodes
#' @param mode The direction of the edges to be considered. If 'in', the edges considered will be incidents over group1, if 'out', will be incident over group2, if 'all', all edges are considered.
#' @export
#' importFrom igraph neighbors

number_of_conecting_edges <- function(g,group1,group2,mode='all'){
	aux = rep(NA,length(group1))
	names(aux) = group1
	for(g1 in group1){
		neighs = neighbors(g,g1,mode=mode)$name
		aux[g1] = length(intersect(neighs,group2))
	}
	return(aux)
}
