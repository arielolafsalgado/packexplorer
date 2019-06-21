#' Auxiliary functions for the leaflet plotting
#'
#' This functions help to manage the text in the leaflet plotting
#' @param graph The graph to be plotted.
#' @param layout The graph layout  
#' @param texto The text to be parsed.
#' @param nwords The number of words per line.
#' @param pack The package of interest
#' @return Multiple values depending on the function
#' 
#' @export
#' @examples

description <- function(pack){
	data('des')
	return(gsub(pattern='<p>|</p>',replacement='',x=as.character(desc$Description[is.element(desc$Package,pack)])))
}

