#' Auxiliary functions for the leaflet plotting
#'
#' These functions help to manage the text in the leaflet plotting
#' @param graph The graph to be plotted.
#' @param layout The layout of the graph 
#' @param texto The text to be parsed.
#' @param nwords The number of words per line.
#' @param pack The package of interest
#' @return Multiple values depending on the function
#' 
#' @export
#' @examples

cut_string <- function(texto,nwords=15){
	data('des')
	data('cats')
	data('downloads')
	if(is.null(texto)) texto = ''
	if(length(texto)==0) texto = ''
	sst <- stringr::str_split(texto, " ")[[1]]
	sst = gsub(pattern='<p>|</p>',replacement = '', x= sst)
	n = length(sst)
	if(n<nwords){
		return(texto)
	}
	q = as.integer(n/nwords) + 1	
	begin= 1
	end = nwords
	final.word=''
	for(i in 1:q){
		word <- paste(na.exclude(sst[begin:end]),collapse=' ')
		final.word = paste(final.word,word,sep='<br/>')
		begin = begin + nwords
		end = end + nwords
	}
	final.word = sub(pattern='</p>',replacement='',final.word)	
	final.word = paste(final.word,'</p>',sep='')
	return(final.word)
}


