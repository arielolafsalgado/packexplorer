#' Matrix construction, used to rank packages
#'
#' This function creates the voting matrix from your packages and the package graph.
#' @param G The package graph, as an igraph object. Usually the Suggests graph. 
#' @param my_packs The packages that you are interested to use as reference. Default is your installed packages
#' @param kind.of The direction of the relations. If you want packages recommending my_packs, it should be "to_packages", and if you want packages recommended by my_packs, it should be "from_packages". It can also be "all".
#' @return A list with packages and scores for each one.
#' @import methods
#' @importFrom Matrix Diagonal
#' @importMethodsFrom Matrix %*%
#' @export
#' @examples
#' gs = my_network()[['Suggests']]
#' pack = my_packages()[1]
#' X = voting_matrix(gs,my_packs=pack)
#' @importFrom igraph as_adjacency_matrix induced_subgraph neighborhood V as.undirected
#' @importFrom Matrix t

voting_matrix = function(G,my_packs=row.names(utils::installed.packages()),kind.of='to_packages'){
  if(any(!is.element(my_packs,V(G)$name))) return('Some packages are missing in the graph')
	dir = switch(kind.of,'to_packages'='in','from_packages'='out','all'='all')
	my_packs = intersect(my_packs,V(G)$name)
	packs = unique(names(unlist(neighborhood(G,mode=dir,nodes = my_packs,order=1))))
	g = induced_subgraph(G,packs)
	if(dir=='all') g = as.undirected(g)
	X = as_adjacency_matrix(g)
	for(p in setdiff(packs,my_packs)){
		if(dir=='in'){
			X[,p] = 0 #Hago que el grado de entrada sea cero
		}else{
			if(dir=='out'){
				X[p,] = 0 #Hago que el grado de salida sea cero
			}else{
				X[p,] = 0
			}
		}
		X[p,p] = 1
	}
	if(dir=='in') X = t(X)
	for(i in 1:nrow(X)){
		X[i,] = X[i,]/max(sum(X[i,]),1)
	}
	return(X)
}
