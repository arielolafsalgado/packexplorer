#' Discover new packages, related to the ones you have
#'
#' Shortcome for recommend.me. Its a good choice from the list it provides.
#' @param relationship The kind of relationship you use to recommend. It can be 'suggests','imports','enhances' or 'depends'. 
#' @param my_packs The packages that you are interested to use as reference. Default is your installed packages
#' @param kind.of The direction of the relations. If you want packages recommending my_packs, it should be "to_packages", and if you want packages recommended by my_packs, it should be "from_packages". It can also be "all".
#' @param deterministic If TRUE, the function returns the first one of the list generated from recommend.me. If FALSE, it samples from the list generated from recommend.me, using the scores as sampling probabilities.
#' @param apply.degree.filter If TRUE, the score of each package is multiplied by the fraction of its edges that conect to my_packs.
#' @return A recommended package.
#' @export
#' @examples
#' pack = give.me.one(gs)
#' install.packages(pack)

give.me.one <-function(relationship='suggests',my_packs = rownames(installed.packages()),kind.of='to_packages',deterministic=F,apply.degree.filter=F){
	data('dependsGraph')
	data('suggestsGraph')	
	data('importsGraph')
	data('enhancesGraph')	
	if(deterministic){
		return(names(recommend.me(relationship=relationship,my_packs = my_packs,kind.of=kind.of,apply.degree.filter=apply.degree.filter)[1]))
	}else{
		scores = recommend.me(relationship=relationship,my_packs = my_packs,kind.of=kind.of,apply.degree.filter=apply.degree.filter)
		if(any(is.na(scores))){
			return(NA)
		}else{
			return(sample(x=names(scores),size=1,prob=scores))
		}
	}
}

