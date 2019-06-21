#' Get the equivalent network with other relations
#'
#' This function gives you the equivalent network in another set of relationships.
#' @param relationship The new relation to be used. It can be 'suggests' or 'depends'. 
#' @param sgs Your own package network
#' @return An igraph object.
#' @export
#' @examples
#' sgd = graph2graph('depends',sgs)
#' plot(sgd)


graph2graph <-function(relationship,sgs){
	data('dependsGraph')
	data('suggestsGraph')	
	data('importsGraph')
	data('enhancesGraph')	
	G = switch(relationship,'suggests'=gs,'depends'=gd,'imports'=gs,'enhances'=gd)	

    sG = induced_subgraph(graph = G,vids = V(G)$name[is.element(V(G)$name,V(sgs)$name)])
    if(!is.null(V(sgs)$suggesters.shape)) V(sG)$suggesters.shape = V(sgs)$suggesters.shape[is.element(V(sgs)$name,V(sG)$name)]
    if(!is.null(V(sgs)$suggesters.col)){ 
	V(sG)$suggesters.col = V(sgs)$suggesters.col[is.element(V(sgs)$name,V(sG)$name)]
        agrega_este = V(sgs)$name[grep(pattern = 'black',x=V(sgs)$suggesters.col)]
    	
	    if(length(agrega_este)){
		agrega_este = paste(agrega_este,collapse='|')
		    edge_ids = as_ids(E(sG))
		    E(sG)$last.suggest.col = 'gray'
		    E(sG)$last.suggest.col[grep(pattern = agrega_este, x = edge_ids)] = 'blue'
	    }
    }
    return(sG)
}
