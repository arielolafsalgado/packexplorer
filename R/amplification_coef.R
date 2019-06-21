#' Discover new packages, related to the ones you have
#'
#' A coefficient that measures how much your network horizons will expand after adding a package.
#' @param pack The package of interest 
#' @param gs The package graph, as an igraph object. Usually the Suggests graph. 
#' @param gd The package graph, as an igraph object. Usually the Depends graph. 
#' @param internals The set of packages to be considered. By default, your packages. 
#' @return A data.frame with the different coefficients.
#' @export
#' @examples
#' pack = 'fuzzywuzzyR'
#' amplification.coefficient(pack,gs,gd)

amplification.coefficient <- function(pack,internals = rownames(installed.packages())){
	data('dependsGraph')
	data('suggestsGraph')	
	if(length(pack)==1){
		sugieren.a.pack = neighbors(graph=gs,v=pack,mode='out')$name
		pack.sugiere = neighbors(graph=gs,v=pack,mode='in')$name
		n.i = length(intersect(internals,sugieren.a.pack))
		n.o = length(setdiff(pack.sugiere,internals))
		coeficiente.suggested = n.o/n.i

		n.i = length(intersect(internals,pack.sugiere))
		n.o = length(setdiff(sugieren.a.pack,internals))
		coeficiente.sugerente = n.o/n.i

		pack.depende.de = neighbors(graph=gd,v=pack,mode='in')$name
		dependen.de.pack = neighbors(graph=gs,v=pack,mode='out')$name
		n.i = length(intersect(internals,sugieren.a.pack))
		n.o = length(setdiff(pack.sugiere,internals))
		coeficiente.dependiente = n.o/n.i	
		return(c('coeficiente.suggested'=coeficiente.suggested,'coeficiente.sugerente'=coeficiente.sugerente,
	'coeficiente.dependiente'=coeficiente.dependiente))
	}
	else{
		output = NULL
		for(p in pack){
			sugieren.a.pack = neighbors(graph=gs,v=pack,mode='out')$name
			pack.sugiere = neighbors(graph=gs,v=pack,mode='in')$name
			n.i = length(intersect(internals,sugieren.a.pack))
			n.o = length(setdiff(pack.sugiere,internals))
			coeficiente.suggested = n.o/n.i

			n.i = length(intersect(internals,pack.sugiere))
			n.o = length(setdiff(sugieren.a.pack,internals))
			coeficiente.sugerente = n.o/n.i

			pack.depende.de = neighbors(graph=gd,v=pack,mode='in')$name
			dependen.de.pack = neighbors(graph=gs,v=pack,mode='out')$name
			n.i = length(intersect(internals,sugieren.a.pack))
			n.o = length(setdiff(pack.sugiere,internals))
			coeficiente.dependiente = n.o/n.i	
			output = rbind(output,c('coeficiente.suggested'=coeficiente.suggested,'coeficiente.sugerente'=coeficiente.sugerente,
		'coeficiente.dependiente'=coeficiente.dependiente))	
		}
		rownames(output) = pack
		return(output)
	}
}

