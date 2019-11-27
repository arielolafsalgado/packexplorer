#' Auxiliary functions for the leaflet plotting
#'
#' These functions help to manage the text in the leaflet plotting
#' @param text The text to be parsed.
#' @param nwords The number of words per line.
#' @return Multiple values depending on the function
#' 
#' @export
#' @examples
#' cut_string('Hello world',1)
#' @importFrom stringr str_split
cut_string = function(text,nwords=15){
	if(is.null(text)) text = ''
	if(length(text)==0) text = ''
	if(any(is.na(text))) text = ''
	sst <- str_split(text, " ")[[1]]
	sst = gsub(pattern='<p>|</p>',replacement = '', x= sst)
	sst = sst[sst!='']
	if(nwords<1 | nwords>length(sst)) return(text)
	n = length(sst)
	q = as.integer(n/nwords) + 1	
	begin= 1
	end = nwords
	final.word=''
	for(i in 1:q){
		word <- paste(stats::na.exclude(sst[begin:end]),collapse=' ')
		final.word = paste(final.word,word,sep='<br/>')
		begin = begin + nwords
		end = end + nwords
	}
	final.word = sub(pattern='</p>',replacement='',final.word)	
	final.word = paste(final.word,'</p>',sep='')
	return(final.word)
}


