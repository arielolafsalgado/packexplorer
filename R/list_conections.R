#' Auxiliary functions for the leaflet plotting
#'
#' This functions help manage the text in the leaflet plotting
#' @param graph The graph to be plotted.
#' @param layout The layout of the graph 
#' @param des The descriptions dataframe.
#' @param cats The categories dataframe.
#' @param texto The text to be parsed.
#' @param nwords The number of words per line.
#' @param pack The package of interest
#' @return Multiple values depending on function
#' 
#' @export
#' @examples


list_conections <- function(graph,layout){
	lista = NULL
	lista_ejes = as_edgelist(graph=graph)
	for(k in 1:nrow(lista_ejes)){
		v1 = lista_ejes[k,1]
		v2 = lista_ejes[k,2]
		lv1 = is.element(V(graph)$name,v1)
		lv2 = is.element(V(graph)$name,v2)
		lista = rbind(lista,layout[lv1,])
		lista = rbind(lista,layout[lv2,])
		lista = rbind(lista,c(NA,NA))
	}
	colnames(lista) = c("x","y")
	return(as.data.frame(lista))

}

