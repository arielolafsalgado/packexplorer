#' Discover new packages, related to the ones you have
#'
#' Obtain a graph induced by the packages containing certain expression in their description. The expression is matched using agrep.
#' @param expression The expression of interest 
#' @param plot.it Should the graph be plotted? Default is FALSE
#' @param first.neighbors Should the first neighbors be added to the graph? Default is FALSE
#' @param return_map Should graph objects be retrieved? Default is TRUE
#' @param point.size The packages attribute which defines their size. If 'score', their size is defines based on their score. If 'downloads', their size is based on their daily downloads. Otherwise it is the same size for all of them.
#' @param min.point.size The min point size, passed to leaflet. Default is 15
#' @param max.point.size The max point size, passed to leaflet. Default is 30
#' @param nwords Number of words per line in the description of the package
#' @return Graph plot in leaflet, and an igraph object.
#' @export
#' @examples
#' expression = 'information'
#' expre2graph(expression)
#' @importFrom igraph induced_subgraph layout_nicely V "%<-%" "V<-"
#' @importFrom leaflet leaflet addPolylines addCircleMarkers addLegend "%>%"
#' @importFrom stringr str_trim str_split
#' @importFrom htmltools HTML

expre2graph <- function(expression,plot.it=FALSE,first.neighbors=FALSE,return.map=TRUE,point.size='downloads',min.point.size=15,max.point.size=30,nwords=5){
	the.desc = tolower(as.character(desc$Description))
	which.packs = grep(expression, the.desc,ignore.case = T)
	if(length(which.packs)==0) return('No package found')
	nodes_expre = unique(desc$Package[which.packs])
	output = list()
	output$nmentions = length(nodes_expre)
	if(!first.neighbors){
		g = induced_subgraph(graph = igraph::union(igraph::union(igraph::union(gs,gd),gi),ge), vids = nodes_expre)
		V(g)$color = 'black'
		V(g)$color[is.element(V(g)$name,row.names(utils::installed.packages()))] = 'white'

	}else{
		nodes_neigh_gd = NULL 
		nodes_neigh_gs = NULL 
		nodes_neigh_ge = NULL 
		nodes_neigh_gi = NULL 
		for(idx in which.packs){
			nodes_neigh_gd = c(nodes_neigh_gd,neighbors(graph = gd, v = desc$Package[idx],mode='all'))
			nodes_neigh_ge = c(nodes_neigh_ge,neighbors(graph = ge, v = desc$Package[idx],mode='all'))
			nodes_neigh_gi = c(nodes_neigh_gi,neighbors(graph = gi, v = desc$Package[idx],mode='all'))
			nodes_neigh_gs = c(nodes_neigh_gs,neighbors(graph = gs, v = desc$Package[idx],mode='all'))
		}
		nodes_neigh_gd = unique(names(nodes_neigh_gd)) 
		nodes_neigh_ge = unique(names(nodes_neigh_ge)) 
		nodes_neigh_gi = unique(names(nodes_neigh_gi)) 
		nodes_neigh_gs = unique(names(nodes_neigh_gs)) 
		gs_neighborhood = induced_subgraph(graph = gs, vids = union(nodes_expre,nodes_neigh_gs))
		gd_neighborhood = induced_subgraph(graph = gd, vids = union(nodes_expre,nodes_neigh_gd))
		ge_neighborhood = induced_subgraph(graph = ge, vids = union(nodes_expre,nodes_neigh_ge))
		gi_neighborhood = induced_subgraph(graph = gi, vids = union(nodes_expre,nodes_neigh_gi))

		g = induced_subgraph(graph = igraph::union(igraph::union(igraph::union(gs,gd),gi),ge), vids = c(nodes_expre,nodes_neigh_gd,nodes_neigh_gs,nodes_neigh_ge,nodes_neigh_gi))

		V(g)$color = 'black'
		V(g)$color[is.element(V(g)$name,nodes_neigh_gs) & !is.element(V(g)$name,nodes_expre)] = 'red'
		V(g)$color[is.element(V(g)$name,nodes_neigh_gd) & !is.element(V(g)$name,nodes_expre)] = 'blue'
		V(g)$color[is.element(V(g)$name,nodes_neigh_ge) & !is.element(V(g)$name,nodes_expre)] = 'orange'
		V(g)$color[is.element(V(g)$name,nodes_neigh_gi) & !is.element(V(g)$name,nodes_expre)] = 'green'
		V(g)$color[is.element(V(g)$name,row.names(utils::installed.packages()))] = 'white'
	}
	categorias_g = cats$Category[is.element(cats$Package,V(g)$name)]
	output$cat_pres = table(str_trim(unlist(str_split(categorias_g,','))))



	l = scale(layout_nicely(g))

	listConnections = list_connections(graph = g,layout = l)

	labs = generate_labels(graph=g,nwords=nwords)

	if(point.size=='score'){
		g1 = V(g)$name
		g2 = row.names(utils::installed.packages())
		scores = number_of_connecting_edges(g,g1,g2)
		scores[is.element(names(scores),g2)]=0
		radii = min.point.size+(max.point.size-min.point.size)*as.numeric(ifelse(is.na(scores),0,scores)/ifelse(is.finite(max(scores,na.rm=T)),max(scores,na.rm=T),1))
		labels = lapply(1:length(V(g)), function(i){paste(labs[[i]],' <p>','Score: ',scores[i],'</p>',sep='')})
		labels = lapply(labels, HTML)
	}else{
		if(point.size=='downloads'){
			desc.rate = downloads$mean.desc[match(V(g)$name,as.character(downloads$paq))]
			radii = min.point.size + (max.point.size-min.point.size)*desc.rate/max(desc.rate,na.rm=T)
			radii[is.na(radii)] = min.point.size 
			labels = lapply(1:length(V(g)), function(i){paste(labs[[i]],' <p>','Downloads rate: ',round(desc.rate[i],3),'</p>',sep='')})
			labels = lapply(labels, HTML)
		}else{
			radii = min.point.size
			labels = lapply(labs, HTML)
		}
	}


	coloresMapV = c('white','black')
	labelsMapV = c('Your own packages','Other packages')

	output$leaflet.graph = leaflet(data=l) %>% addPolylines(data=listConnections,lng=~x,lat = ~y,weight=2,color='blue') %>% addCircleMarkers(lng=l[,1],lat = l[,2],label=labels,color=V(g)$color,radius=radii,opacity=1,fillOpacity=1) %>% addLegend(colors=coloresMapV,labels=labelsMapV)
	output$igraph.graph = g
	if(plot.it){
		print(output$grafo.leaflet)
	}
	if(return.map){
		return(output)
	}
}
