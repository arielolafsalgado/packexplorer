#' Auxiliary functions for the leaflet plotting
#'
#' These functions help to manage the text in the leaflet plotting
#' @param graph The graph to be plotted.
#' @param layout The layout of the graph 
#' @param text The text to be parsed.
#' @param nwords The number of words per line.
#' @param pack The package of interest
#' @return Multiple values depending on the function
#' 
#' @export
#' @examples
#' @importFrom stringr str_split
cut_string <- function(text,nwords=15){
	data('des')
	data('cats')
	data('downloads')
	if(is.null(text)) text = ''
	if(length(text)==0) text = ''
	sst <- str_split(text, " ")[[1]]
	sst = gsub(pattern='<p>|</p>',replacement = '', x= sst)
	n = length(sst)
	if(n<nwords){
		return(text)
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


