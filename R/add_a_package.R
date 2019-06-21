#' Discover new packages, related to the ones you have
#'
#' This function gives you an igraph network with your packages and the first one that recommend.me gives you.
#' @param relationship The kind of relationship you use to recommend. It can be 'suggests','imports','enhances' or 'depends'. 
#' @param my_packs The packages that you are interested in to use as reference. Default is your installed packages.
#' @param kind.of The direction of the relations. If you want packages recommending my_packs, it should be "to_packages", and if you want packages recommended by my_packs, it should be "from_packages". It can also be "all".
#' @param neighbors.neighbors If it is TRUE, only the neighbors of the new package are included in the output.
#' @return An igraph object.
#' @export
#' @examples
#' g = add.a.package(gs)
#' plot(g)

add.a.package <-function(relationship='suggests',my_packs = rownames(installed.packages()),kind.of='to_packages',neighbors.neighbors=FALSE,nAdd=1){
	data('dependsGraph')
	data('suggestsGraph')	
	data('importsGraph')
	data('enhancesGraph')
	agrega_este = names(recommend.me(relationship=relationship,my_packs = my_packs,kind.of=kind.of)[1:nAdd])
	agrega_estos = NULL
	g = switch(relationship,'suggests'=gs,'depends'=gd,'imports'=gi,'enhances'=ge)
	if(neighbors.neighbors){
	for(v in agrega_este){
		dir = switch(kind.of,'to_packages'='in','from_packages'='out','all'='all')
		agrega_estos = c(agrega_estos,igraph::neighbors(graph=g,v=v,mode=dir)$name)
		 
	}
	agrega_estos = setdiff(agrega_estos,c(my_packs,agrega_este))
	}
	g = igraph::induced_subgraph(graph = g,vids = c(intersect(my_packs,V(g)$name),agrega_este,agrega_estos))
	vecinos = NULL
	for(p in agrega_este){
		vecinos = c(vecinos,neighbors(g,p,'all')$name)
	}
	vecinos = intersect(vecinos,my_packs)
	neighbors.neighbors = agrega_estos
	edge_ids = as_ids(E(g))
	E(g)$recien.suggested.col = 'gray'

	
	E(g)$recien.suggested.col[grep(pattern = paste(paste('\\|',agrega_este,sep=''),collapse='|'), x = edge_ids)] = 'orange'
	E(g)$recien.suggested.col[grep(pattern = paste(paste(agrega_este,'\\|',sep=''),collapse='|'), x = edge_ids)] = 'magenta'
	V(g)$suggested.col = 'brown'
	V(g)$suggested.col[is.element(V(g)$name,vecinos)] = 'white'
	V(g)$suggested.col[is.element(V(g)$name,neighbors.neighbors)] = 'green'
	V(g)$suggested.col[is.element(V(g)$name,agrega_este)] = 'black'
	V(g)$suggested.shape = 'circle'
	V(g)$suggested.shape[is.element(V(g)$name,vecinos)] = 'square'
	V(g)$suggested.shape[is.element(V(g)$name,agrega_este)] = 'square'
	V(g)$ranking = 0
	for(i in 1:nAdd){
		V(g)$ranking[is.element(V(g)$name,agrega_este[i])] = i
	}
	return(g)
}

