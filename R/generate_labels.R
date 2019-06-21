#' Auxiliary functions for the leaflet plotting
#'
#' This functions help manage the text in the leaflet plotting
#' @param graph The graph to be plotted.
#' @param layout The layout of the graph 
#' @param texto The text to be parsed.
#' @param nwords The number of words per line.
#' @param pack The package of interest
#' @return Multiple values depending on function
#' 
#' @export
#' @examples

generate_labels <- function(graph,nwords=15){
	data('des')
	data('cats')
	labs <- lapply(1:length(V(graph)), function(i) {
		pack = V(graph)$name[i]
		desci = as.character(desc$Description[is.element(desc$Package,pack)])
		catsi = ifelse(!is.na(cats$Category[is.element(cats$Package,pack)]),yes=as.character(cats$Category[is.element(cats$Package,pack)]),no='No AT')
		desci = cut_string(desci,nwords=nwords)
		paste0( '<p><font size = "6">', pack, '</font></p>','<font size = "3">', 
		  desci,'</font>','<p>',catsi,'</p>') 
	})
	return(unlist(labs))
}
