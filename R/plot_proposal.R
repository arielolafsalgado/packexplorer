#' Discover new packages, related to the ones you have
#'
#' Plots your network plus a recommended package added.
#' @param relationship The kind of relationship you use to recommend. It can be 'suggests','imports','enhances' or 'depends'. 
#' @param my_packs The packages that you are interested to use as reference. Default is your installed packages
#' @param kind.of The direction of the relations. If you want packages recommending my_packs, it should be "to_packages", and if you want packages recommended by my_packs, it should be "from_packages". It can also be "all".
#' @param on.leaflet The plot should be done on leaflet?
#' @param neighbors.neighbors If it is TRUE, only the neighbors of the new package are considered.
#' @param point.size The aspect of the packages giving their size. If 'score', their size is given based in their score. If 'downloads', their size is base in ther daily downloads. Else its the same for all of them.
#' @param min.point.size The min point size, passed to leaflet. Default is 15
#' @param max.point.size The max point size, passed to leaflet. Default is 30
#' @param ... Igraph plotting parameters
#' @return A list with packages and scores for each one.
#' @export
#' @examples
#' plot_proposal(gs,on.leaflet=T,downloads=downloads)


plot_proposal <- function(relationship='suggests',my_packs = rownames(installed.packages()),vertex.label.dist=-1,vertex.label.degree=pi/2,vertex.size=3,
arrow.size=0,kind.of='to_packages',new.label=FALSE,neighbors.label=FALSE,others.label=FALSE,neighbors.neighbors.label=FALSE,only.neighbors=FALSE,
neighbors.neighbors=FALSE,label.cex=1,label.color='black',layout=NULL,on.leaflet=FALSE,nAdd=1,recenter.suggested=!on.leaflet,nwords=5,point.size='downloads',min.point.size=15,max.point.size=30){
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

	g = add.a.package(relationship=relationship,my_packs=my_packs,kind.of=kind.of,neighbors.neighbors=neighbors.neighbors,nAdd=nAdd)
	if(only.neighbors){
		g = delete_vertices(graph = g,v = V(g)$name[grep(pattern='brown',x=V(g)$suggested.col)])
	}
	if(!is.null(gd)){
	sgd = graph2graph(relationship=setdiff(c('suggests','depends'),relationship),sgs = g) 
	}
	else{
	sgd = NULL
	}
	if(is.null(layout)){	
		l = layout_suger(gs = g,gd = sgd, suggested = V(g)$name[grep(pattern='black',x=V(g)$suggested.col)],recenter.suggested=recenter.suggested)
	}
	else{
		l = layout
	}
	v.labels = rep(NA,length(V(g)))
	if(new.label){
	v.labels[grep(pattern = 'black',x=V(g)$suggested.col)] = V(g)$name[grep(pattern = 'black',x=V(g)$suggested.col)]
	}
	if(neighbors.label){
	v.labels[grep(pattern = 'white',x=V(g)$suggested.col)] = V(g)$name[grep(pattern = 'white',x=V(g)$suggested.col)]
	}
	if(others.label){
	v.labels[grep(pattern = 'brown',x=V(g)$suggested.col)] = V(g)$name[grep(pattern = 'brown',x=V(g)$suggested.col)]
	}
	if(neighbors.neighbors.label & neighbors.neighbors){
	v.labels[grep(pattern = 'green',x=V(g)$suggested.col)] = V(g)$name[grep(pattern = 'green',x=V(g)$suggested.col)]
	}
	logicsuggested = grepl(pattern='black',x=V(g)$suggested.col)
	label.font = rep(1,length(V(g)))
	label.font[logicsuggested] = 3
	vertex.size = rep(vertex.size,length(V(g)))
	vertex.size[logicsuggested] = 3*vertex.size[logicsuggested]
	if(neighbors.neighbors) vertex.size[grep(pattern='green',x=V(g)$suggested.col)] = 2*min(vertex.size,na.rm=TRUE)
	label.color = rep(label.color,length(V(g)))
	label.color[logicsuggested] = 'red'
	vertex.label.dist = rep(vertex.label.dist,length(V(g)))
	vertex.label.dist[logicsuggested] = 1.2*vertex.label.dist[logicsuggested]
	if(!on.leaflet){	
		plot(g,vertex.label=v.labels,vertex.label.dist=vertex.label.dist,vertex.label.degree=vertex.label.degree,vertex.size=vertex.size,vertex.label.font=label.font,
	 vertex.shapes=V(g)$suggested.shape,vertex.color=V(g)$suggested.col,edge.arrow.size=arrow.size,
	 edge.color = E(g)$recien.suggested.col,layout=l,vertex.label.cex=label.cex,vertex.label.color=label.color)
	}
	else{
		listConections = list_conections(graph = g,layout = l)
		labs = generate_labels(graph=g,nwords=nwords)

		if(point.size=='score'){
			g1 = V(g)$name
			g2 = row.names(installed.packages())
			scores = number_of_conecting_edges(g,g1,g2)
			scores[is.element(names(scores),g2)]=0
			radii = min.point.size+ (max.point.size-min.point.size)*as.numeric(ifelse(is.na(scores),0,scores)/ifelse(is.finite(max(scores,na.rm=T)),max(scores,na.rm=T),1))
			labels = lapply(1:length(V(g)), function(i){paste(labs[[i]],' <p>','Score: ',scores[i],'</p>',sep='')})
			labels = lapply(labels, HTML)
		}else{
			if(point.size=='downloads'){
				desc.rate = downloads$mean.desc[match(V(g)$name,as.character(downloads$paq))]
				radii = min.point.size+(max.point.size-min.point.size)*desc.rate/max(desc.rate,na.rm=T)
				radii[is.na(radii)] = min.point.size 
				labels = lapply(1:length(V(g)), function(i){paste(labs[[i]],' <p>','Downloads rate: ',round(desc.rate[i],3),'</p>',sep='')})
				labels = lapply(labels, HTML)
			}else{
				radii = min.point.size
				labels = lapply(labs, HTML)
			}
		}
		coloresMapV = c('brown','white','black')
		labelsMapV = c('Your own packages, not related to the proposal','Your own packages, related to the proposal','Proposed package')
		map <- leaflet() %>% addPolylines(data=listConections,lng=~x,lat = ~y,weight=2,color=E(g)$recien.suggested.col) %>% addCircleMarkers(lng=l[,1],lat = l[,2],label=labels,color=V(g)$suggested.col,radius=radii,opacity=1,fillOpacity=1) %>% addLegend(colors=coloresMapV,labels=labelsMapV)
		print(map)
		return(map)

	}

}

