#' Find nearest packages to a selected group
#'
#' Plot your network, plus its firts neighbors, using leaflet.
#' @param relationship The kind of relationship you use to recommend. It can be 'suggests','imports','enhances' or 'depends'. 
#' @param my_packs The packages that you are interested to use as reference. Default is your installed packages
#' @param kind.of The direction of the relations. If you want packages recommending my_packs, it should be "to_packages", and if you want packages recommended by my_packs, it should be "from_packages". It can also be "all".
#' @param order The order of the neighborhood. It has to be equal or bigger than zero. 
#' @param return.map Should the leaflet and igraph object be returned?
#' @param plot.it Should the leaflet object be printed?
#' @param nwords The number of words appearing in the description of the package, per row.
#' @param point.size The aspect of the packages giving their size. If 'score', their size is given based in their score. If 'downloads', their size is base in ther daily downloads. Else its the same for all of them.
#' @param min.point.size The min point size, passed to leaflet. Default is 15
#' @param max.point.size The max point size, passed to leaflet. Default is 30
#' @param apply.degree.filter If TRUE, the score of each package is multiplied by the fraction of its edges that conect to my_packs.
#' @return A list with packages and scores for each one.
#' @export
#' @examples
#' plot_neighbors()
#' @importFrom igraph V neighborhood induced_subgraph layout_nicely "V<-"
#' @importFrom htmltools HTML
#' @importFrom leaflet leaflet addCircleMarkers addPolylines addLegend

plot_neighbors = function(relationship='suggests',my_packs = rownames(utils::installed.packages()),kind.of='to_packages',order=1,point.size='score',return.map =T, plot.it = F,apply.degree.filter=F,nwords=5,min.point.size=15,max.point.size=30){
	desc = desc[is.element(desc$Package,V(gd)$name),]
	cats = cats[is.element(cats$Package,V(gd)$name),]
	downloads = downloads[is.element(downloads$paq,V(gd)$name),]
	dir = switch(kind.of,'to_packages'='in','from_packages'='out','all'='all')
	G = switch(relationship,'suggests'=gs,'depends'=gd,'imports'=gi,'enhances'=ge)
	not_considered_packs = setdiff(my_packges,V(G)$name)
	my_packs = intersect(my_packs,V(G)$name)
	if(length(my_packs)>0){
  	if(length(not_considered_packs)>0) print(paste('Packages not available in database:',paste(not_considered_packs,collapse=',')))
	}else{
	  return('Requested packages are not available')
	}
	neighs = unique(names(unlist(neighborhood(G,mode=dir,nodes = my_packs,order=order))))
	g = induced_subgraph(G,neighs)
	scores = recommend_me(relationship=relationship,my_packs=my_packs,kind.of=kind.of,apply.degree.filter=apply.degree.filter)
	V(g)$color = 'black'
	V(g)$color[is.element(V(g)$name,my_packs)] = 'white'
	V(g)$score = 0
	if(any(is.element(names(scores),V(g)$name))){
		V(g)$score[match(names(scores),V(g)$name)] = scores
	}
	l = scale(layout_nicely(g))
	listConnections = list_connections(graph = g,layout = l)
	labs = generate_labels(graph=g,nwords=nwords)

	if(point.size=='score'){
		radii = min.point.size+ifelse(is.na(V(g)$score),0,V(g)$score)/max(V(g)$score,na.rm=T)*(max.point.size-min.point.size)
		labels = lapply(1:length(V(g)), function(i){paste(labs[[i]],' <p>','Score: ',V(g)$score[i],'</p>',sep='')})
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
	labelsMapV = c('Your own packages','Neighbor packages')
	map <- leaflet(data=l) %>% addPolylines(data=listConnections,lng=~x,lat = ~y,weight=2,color='gray') %>% addCircleMarkers(lng=l[,1],lat = l[,2],label=labels,color=V(g)$color,radius=radii,opacity=1,fillOpacity=1) %>% addLegend(colors=coloresMapV,labels=labelsMapV)
	output = list()
	output$leaflet.graph = map
	output$igraph.graph = g
	if(plot.it) print(map)
	if(return.map ) return(output)

}

