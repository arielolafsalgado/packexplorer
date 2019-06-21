#' Your own R-Network, plus its first neighbors
#'
#' This function gives you your own package network, based on the avaiable general networks, plus its first neighbots
#' @param kind.of The direction considered for the neighborhood. If you want packages pointing to yours, use 'to_packages', if you want packages pointed by yours, use 'from_packages'. If you want them all, use 'all'
#' @return A list with igraph objects, one for each input graph.
#' @export
#' @examples
#' g = one_step2my_net(gd,gs)$Sug
#' plot(g)

one_step2my_net <- function(kind.of='all'){
	data('dependsGraph')
	data('suggestsGraph')	
	data('importsGraph')
	data('enhancesGraph')	
	my_net = my_network()
	my_dep = my_net$Dep
	my_sug = my_net$Sug
	my_imp = my_net$Imp
	my_enh = my_net$Enh
	dir = switch(kind.of,'to_packages'='in','from_packages'='out','all'='all')
	my_sug2 = unique(names(unlist(igraph::neighborhood(gs,mode=dir,nodes = V(my_sug)$name,order=1))))
	new_sug = igraph::induced_subgraph(gs,V(gs)[is.element(V(gs)$name,my_sug2)])
	my_dep2 = unique(names(unlist(igraph::neighborhood(gd,mode=dir,nodes = V(my_dep)$name,order=1))))
	new_dep = igraph::induced_subgraph(gd,V(gd)[is.element(V(gd)$name,my_dep2)])
	my_imp2 = unique(names(unlist(igraph::neighborhood(gi,mode=dir,nodes = V(my_imp)$name,order=1))))
	new_imp = igraph::induced_subgraph(gi,V(gi)[is.element(V(gi)$name,my_imp2)])
	my_enh2 = unique(names(unlist(igraph::neighborhood(ge,mode=dir,nodes = V(my_enh)$name,order=1))))
	new_enh = igraph::induced_subgraph(ge,V(ge)[is.element(V(ge)$name,my_enh2)])
	V(new_dep)$color = 'black'
	V(new_dep)$color[is.element(V(new_dep)$name,V(my_dep)$name)]='white'
	V(new_sug)$color = 'black'
	V(new_sug)$color[is.element(V(new_sug)$name,V(my_sug)$name)]='white'
	V(new_enh)$color = 'black'
	V(new_enh)$color[is.element(V(new_enh)$name,V(my_enh)$name)]='white'
	V(new_imp)$color = 'black'
	V(new_imp)$color[is.element(V(new_imp)$name,V(my_imp)$name)]='white'
	return(list('Dep'=new_dep,'Sug'=new_sug,'Imp'=new_dep,'Enh'=new_sug))	
}

