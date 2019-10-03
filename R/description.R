#' Auxiliary functions for the leaflet plotting
#'
#' This functions help to manage the text in the leaflet plotting
#' @param pack The package of interest
#' @return Multiple values depending on the function
#' 
#' @export
#' @examples

description <- function(pack){
#	utils::data('des',envir=environment())
	return(gsub(pattern='<p>|</p>',replacement='',x=as.character(desc$Description[is.element(desc$Package,pack)])))
}

