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
#' importFrom igraph as_edgelist V

list_conections <- function(graph,layout){
	connList = NULL
	edgeList = as_edgelist(graph=graph)
	for(k in 1:nrow(edgeList)){
		v1 = edgeList[k,1]
		v2 = edgeList[k,2]
		lv1 = is.element(V(graph)$name,v1)
		lv2 = is.element(V(graph)$name,v2)
		connList = rbind(connList,layout[lv1,])
		connList = rbind(connList,layout[lv2,])
		connList = rbind(connList,c(NA,NA))
	}
	colnames(connList) = c("x","y")
	return(as.data.frame(connList))

}

