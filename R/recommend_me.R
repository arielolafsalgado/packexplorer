#' Rank available packages to be installed
#'
#' This function provides you with recommendations of new packages to install.
#' @param relationship The kind of relationship you use to recommend. It can be 'suggests','imports','enhances' or 'depends'. 
#' @param my_packs The packages that you are interested to use as reference. Default is your installed packages
#' @param kind.of The direction of the relations. If you want packages recommending my_packs, it should be "to_packages", and if you want packages recommended by my_packs, it should be "from_packages". It can also be "all".
#' @param apply.degree.filter If TRUE, the score of each package is multiplied by the fraction of its edges that conect to my_packs.
#' @param niter The number of interations done to calculate the score.
#' @param nMax The max number of elements to be retrieved. Default is the full list.
#' @return A list with packages and scores for each one.
#' @import methods
#' @importFrom Matrix diag
#' @importMethodsFrom Matrix %*%
#' @export
#' @examples
#' \dontrun{
#' pack_list = recommend_me()
#' install.packages(names(pack_list)[1])
#' }
#' @importFrom igraph V induced_subgraph degree

recommend_me = function(relationship='suggests',my_packs = rownames(utils::installed.packages()),kind.of='to_packages',apply.degree.filter=F,niter=50,nMax=NULL){
	G = switch(relationship,'suggests'=gs,'depends'=gd,'imports'=gi,'enhances'=ge)	
	not_considered_packs = setdiff(my_packs,V(G)$name)
	if(length(not_considered_packs)>0) print(paste('Packages not available in database:',paste(not_considered_packs,collapse=',')))
	my_packs = intersect(my_packs,V(G)$name)
	if(length(my_packs)>0){
	  if(length(not_considered_packs)>0) print(paste('Packages not available in database:',paste(not_considered_packs,collapse=',')))
	}else{
	  return('Requested packages are not available')
	}
	VM = voting_matrix(G,my_packs,kind.of)
	p = 1-diag(VM)
	names(p) = row.names(VM)
	for(i in 1:niter){
	VM = VM%*%VM
	}
	p = p%*%VM	
	p = p[1,]
	score = p[setdiff(names(p),my_packs)]
	score = score/sum(score)
	if(apply.degree.filter){
		g = induced_subgraph(G,row.names(VM))
		coef = degree(g,v=names(score))/degree(G,v=names(score))
		score = score*coef
		score = score/sum(score)
	}
	output = sort(score,decreasing=TRUE)
	return(output[1:min(nMax,length(output))])
}

