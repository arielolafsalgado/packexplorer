#' Discover new packages, related to the ones you hace
#'
#' This function provides you with recommendations of new packages to install.
#' @param gs The package graph, as an igraph object. Usually the Suggests graph. 
#' @param my_packs The packages that you are interested to use as reference. Default is your installed packages
#' @param kind.of The direction of the relations. If you want packages recommending my_packs, it should be "to_packages", and if you want packages recommended by my_packs, it should be "from_packages". It can also be "all".
#' @return A list with packages and scores for each one.
#' @export
#' @examples
#' pack_list = recommend.me(graph.dep,graph.sug)
#' install.packages(names(pack_list)[1])


recommend.meOLD <- function(gs,my_packs = rownames(installed.packages()),kind.of='to_packages'){
	my_packs = intersect(my_packs,V(gs)$name)
	if(kind.of == 'to_packages'){
		dir_ = 'out'
		dir = 'in'
	}
	else{
		if(kind.of == 'from_packages'){
			dir_ = 'in'
			dir = 'out'
		}
		else{
			dir_ = 'all'
			dir = 'all'
		}
	}
	mis_suggesteds = unique(names(unlist(neighborhood(gs,mode=dir,nodes = my_packs,order=1))))
	mi_gs_suggested = igraph::induced_subgraph(gs,V(gs)[is.element(V(gs)$name,mis_suggesteds)])
	mis_suggesteds = setdiff(mis_suggesteds,my_packs)
	X = igraph::as_adjacency_matrix(mi_gs_suggested)
	if(dir_=='in'){
		X = X[my_packs,mis_suggesteds]
		score = Matrix::colSums(X)
	}
	else{
		if(dir_=='out'){
			X = X[mis_suggesteds,my_packs]
			score = Matrix::rowSums(X) 
		}
		else{
			score = Matrix::rowSums(X[mis_suggesteds,my_packs]) + Matrix::colSums(X[my_packs,mis_suggesteds])
		}
	}
	output = sort(score,decreasing=TRUE)
	return(output)
}

