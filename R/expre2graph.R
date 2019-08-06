#' Discover new packages, related to the ones you have
#'
#' Obtain a graph induced by the packages containing certain expression in their description. The expression is matched using agrep.
#' @param expression The expression of interest 
#' @param plot.it Should the graph be plotted? Default is FALSE
#' @param max.distance passed to agrep
#' @param ignore.case passed to agrep
#' @param fixed passed to agrep
#' @param first.neighbors Should the first neighbors be added to the graph? Default is FALSE
#' @param generate_output Should graph objects be retrieved? Default is TRUE
#' @param point.size The packages attribute which defines their size. If 'score', their size is defines based on their score. If 'downloads', their size is based on their daily downloads. Otherwise it is the same size for all of them.
#' @param min.point.size The min point size, passed to leaflet. Default is 15
#' @param max.point.size The max point size, passed to leaflet. Default is 30
#' @return Graph plot in leaflet, and an igraph object.
#' @export
#' @examples
#' expression = 'information'
#' expre2graph(expression)

expre2graph <- function(expression,plot.it=FALSE,first.neighbors=FALSE,generate_output=TRUE,point.size='downloads',nwords=5,min.point.size=15,max.point.size=30){
	data('des')
	data('cats')
	data('downloads')
	data('dependsGraph')
	data('suggestsGraph')	
	data('importsGraph')
	data('enhancesGraph')
	desc = desc[is.element(desc$Package,V(gd)$name),]
	cats = cats[is.element(cats$Package,V(gd)$name),]
	downloads = downloads[is.element(downloads$paq,V(gd)$name),]
	ladesc <- tolower(as.character(desc$Description))
	cuales <- grep(tolower(expression), ladesc,ignore.case = T)
	if(length(cuales)==0) return('No package found')
	nodos_expre <- desc$Package[cuales]
	nodos_vecinos_gd <- NULL 
	nodos_vecinos_gs <- NULL 
	nodos_vecinos_ge <- NULL 
	nodos_vecinos_gi <- NULL 
	for(idx in cuales){
		nodos_vecinos_gd = c(nodos_vecinos_gd,neighbors(graph = gd, v = desc$Package[idx],mode='all'))
		nodos_vecinos_ge = c(nodos_vecinos_ge,neighbors(graph = ge, v = desc$Package[idx],mode='all'))
		nodos_vecinos_gi = c(nodos_vecinos_gi,neighbors(graph = gi, v = desc$Package[idx],mode='all'))
		nodos_vecinos_gs = c(nodos_vecinos_gs,neighbors(graph = gs, v = desc$Package[idx],mode='all'))
	}

	nodos_vecinos_gd <- unique(names(nodos_vecinos_gd)) 
	nodos_vecinos_ge <- unique(names(nodos_vecinos_ge)) 
	nodos_vecinos_gi <- unique(names(nodos_vecinos_gi)) 
	nodos_vecinos_gs <- unique(names(nodos_vecinos_gs)) 
	output = list()
	output$nmenciones = length(cuales)
	if(!first.neighbors){
		gs_vecindario = induced_subgraph(graph = gs, vids = desc$Package[cuales])
		gd_vecindario = induced_subgraph(graph = gd, vids = desc$Package[cuales])
		ge_vecindario = induced_subgraph(graph = ge, vids = desc$Package[cuales])
		gi_vecindario = induced_subgraph(graph = gi, vids = desc$Package[cuales])

		g = igraph::induced_subgraph(graph = igraph::union(igraph::union(igraph::union(gs,gd),gi),ge), vids = nodos_expre)
		V(g)$color = 'black'
		V(g)$color[is.element(V(g)$name,row.names(installed.packages()))] = 'white'

	}
	else{
		gs_vecindario = induced_subgraph(graph = gs, vids = union(nodos_expre,nodos_vecinos_gs))
		gd_vecindario = induced_subgraph(graph = gd, vids = union(nodos_expre,nodos_vecinos_gd))
		ge_vecindario = induced_subgraph(graph = ge, vids = union(nodos_expre,nodos_vecinos_ge))
		gi_vecindario = induced_subgraph(graph = gi, vids = union(nodos_expre,nodos_vecinos_gi))

		g = induced_subgraph(graph = igraph::union(igraph::union(igraph::union(gs,gd),gi),ge), vids = c(nodos_expre,nodos_vecinos_gd,nodos_vecinos_gs,nodos_vecinos_ge,nodos_vecinos_gi))

		V(g)$color = 'black'
		V(g)$color[is.element(V(g)$name,nodos_vecinos_gs) & !is.element(V(g)$name,nodos_expre)] = 'red'
		V(g)$color[is.element(V(g)$name,nodos_vecinos_gd) & !is.element(V(g)$name,nodos_expre)] = 'blue'
		V(g)$color[is.element(V(g)$name,nodos_vecinos_ge) & !is.element(V(g)$name,nodos_expre)] = 'orange'
		V(g)$color[is.element(V(g)$name,nodos_vecinos_gi) & !is.element(V(g)$name,nodos_expre)] = 'green'
		V(g)$color[is.element(V(g)$name,row.names(installed.packages()))] = 'white'
	}
	categorias_g = cats$Category[is.element(cats$Package,V(g)$name)]
	output$cat_pres = table(str_trim(unlist(str_split(categorias_g,','))))



	l = scale(layout_nicely(g))

	listConections = list_conections(graph = g,layout = l)

	labs = generate_labels(graph=g,nwords=nwords)

	if(point.size=='score'){
		g1 = V(g)$name
		g2 = row.names(installed.packages())
		scores = number_of_conecting_edges(g,g1,g2)
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

	output$grafo.leaflet <- leaflet() %>% addPolylines(data=listConections,lng=~x,lat = ~y,weight=2,color='blue') %>% addCircleMarkers(lng=l[,1],lat = l[,2],label=labels,color=V(g)$color,radius=radii,opacity=1,fillOpacity=1) %>% addLegend(colors=coloresMapV,labels=labelsMapV)
	output$grafo.igraph = g
	if(plot.it){
		print(output$grafo.leaflet)
	}
	if(generate_output){
		return(output)
	}
}
