#' Discover new packages, related to the ones you have
#'
#' This function gives you an ego network of a package (the package and its surrounders).
#' @param pack The package of interest.
#' @param plot.it Should the network be plotted? Default is TRUE
#' @param facts Should the output contain facts about the package? Default is FALSE
#' @param point.size The aspect of the packages giving their size. If 'score', their size is given based in their score. If 'downloads', their size is base in ther daily downloads. Else its the same for all of them. If you want to specify a size use min.point.size
#' @param add_my_packs Should your packages be added to the plot? Default is FALSE
#' @param my_packs_neighbors If add_my_packs is TRUE, should only keep the surrounders of pack? Default is FALSE
#' @param min.point.size The min point size, passed to leaflet. Default is 15
#' @param max.point.size The max point size, passed to leaflet. Default is 30
#' @return A dataframe and also a plot.
#' @export
#' @examples
#' who.are.you('ggplot2')
#' importFrom igraph V neighbors induced_subgraph E %<-% as_ids layout_nicely
#' importFrom leaflet leaflet addCircleMarkers addPolylines addLegend addLabelOnlyMarkers
who.are.you <- function(pack,plot.it = TRUE,nwords=5,add_my_packs=FALSE,my_packs_neighbors=FALSE,point.size='downloads',min.point.size=15,max.point.size=30,...){
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
	pack_depends = neighbors(graph=gd,v=pack,mode='in')
	pack_revdepends = neighbors(graph=gd,v=pack,mode='out')
	pack_revsuggests = neighbors(graph=gs,v=pack,mode='out')
	pack_suggests = neighbors(graph=gs,v=pack,mode='in')
	pack_imports = neighbors(graph=gi,v=pack,mode='in')
	pack_revimports = neighbors(graph=gi,v=pack,mode='out')
	pack_revenhances = neighbors(graph=ge,v=pack,mode='out')
	pack_enhances = neighbors(graph=ge,v=pack,mode='in')
	gs_neighborhood = induced_subgraph(graph = gs, vids = c(pack,(igraph::union(pack_revsuggests,pack_suggests))$name))
	gd_neighborhood = induced_subgraph(graph = gd, vids = c(pack,(igraph::union(pack_depends,pack_revdepends))$name))
	ge_neighborhood = induced_subgraph(graph = ge, vids = c(pack,(igraph::union(pack_enhances,pack_revenhances))$name))
	gi_neighborhood = induced_subgraph(graph = gi, vids = c(pack,(igraph::union(pack_imports,pack_revimports))$name))
	packlist = c(pack,(igraph::union(pack_revsuggests,pack_suggests))$name,(igraph::union(pack_depends,pack_revdepends))$name,(igraph::union(pack_revimports,pack_imports))$name,(igraph::union(pack_enhances,pack_revenhances))$name)
	if(add_my_packs){
		my_packs = rownames(installed.packages())
		if(my_packs_neighbors){
			g_full = igraph::union(gs,igraph::union(gd,igraph::union(gi,ge)))
			my_packs = intersect(my_packs,neighbors(graph=g_full,v=pack, mode = 'all'))
		}
		packlist = c(packlist,my_packs)
		packlist = intersect(packlist,union(V(gs)$name,V(gd)$name))
	}
	g = induced_subgraph(graph = igraph::union(igraph::union(igraph::union(gs,gd),gi),ge), vids = packlist)
	V(g)$color = 'black'
	V(g)$color[is.element(V(g)$name,V(gs_neighborhood)$name)] = 'red'
	V(g)$color[is.element(V(g)$name,V(gd_neighborhood)$name)] = 'blue'
	V(g)$color[is.element(V(g)$name,V(gi_neighborhood)$name)] = 'green'
	V(g)$color[is.element(V(g)$name,V(ge_neighborhood)$name)] = 'orange'
	V(g)$color[is.element(V(g)$name,V(gs_neighborhood)$name) & is.element(V(g)$name,V(gd_neighborhood)$name)] = 'violet'
	V(g)$color[is.element(V(g)$name,rownames(installed.packages()))] = 'white'
	V(g)$color[is.element(V(g)$name,pack)] = 'black'
	Eg = as_ids(E(g))
	Egs = as_ids(E(gs_neighborhood))
	Egd = as_ids(E(gd_neighborhood))
	Egi = as_ids(E(gi_neighborhood))
	Ege = as_ids(E(ge_neighborhood))
	E(g)$color = 'gray'
	E(g)$color[is.element(Eg,Egs) & is.element(tail_of(graph = g, es = E(g))$name, pack)] = 'red'
	E(g)$color[is.element(Eg,Egs) & is.element(head_of(graph = g, es = E(g))$name, pack)] = 'orange'
	E(g)$color[is.element(Eg,Egd) & is.element(tail_of(graph = g, es = E(g))$name, pack)] = 'blue'
	E(g)$color[is.element(Eg,Egd) & is.element(head_of(graph = g, es = E(g))$name, pack)] = 'green'
	E(g)$color[is.element(Eg,Egi) & is.element(tail_of(graph = g, es = E(g))$name, pack)] = 'pink'
	E(g)$color[is.element(Eg,Egi) & is.element(head_of(graph = g, es = E(g))$name, pack)] = 'brown'
	E(g)$color[is.element(Eg,Ege) & is.element(tail_of(graph = g, es = E(g))$name, pack)] = 'yellow'
	E(g)$color[is.element(Eg,Ege) & is.element(head_of(graph = g, es = E(g))$name, pack)] = 'violet'
	output = list()
	output$graph = g
	if(plot.it){
		l = scale(layout_nicely(g))
		listConections = list_conections(graph = g,layout = l)
		labs = generate_labels(graph=g,nwords=nwords)

		if(point.size=='score'){
			g1 = V(g)$name
			g2 = row.names(installed.packages())
			scores = number_of_conecting_edges(g,g1,g2)
			scores[is.element(names(scores),g2)]=0
			radii = min.point.size+(max.point.size-min.point.size)*as.numeric(ifelse(is.na(scores),0,scores)/max(c(scores,1),na.rm=T))
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
		coloresMapV = c('black','white','red','blue','green','orange')
		labelsMapV = c(pack,'My packs','Via Suggests','Via Depends','Via Imports','Via Enhances')
		if(is.element('violet',V(g)$color)){
			coloresMapV = c(coloresMapV,'violet')
			labelsMapV = c(labelsMapV,'Via Suggests & Depends')
		}
		coloresMapE = c('red','orange','blue','green','pink','brown','yellow','violet','gray')
		labelsMapE = c(paste('suggests to',pack),paste(pack,'suggests to'),paste('deppends from',pack),paste(pack,'deppends from'),paste('imports',pack),paste(pack,'imports'),paste('enhances',pack),paste(pack,'enhances'),'other')
		map <- leaflet() %>% addPolylines(data=listConections,lng=~x,lat = ~y,weight=2,color=E(g)$color) %>% addCircleMarkers(lng=l[,1],lat = l[,2],label=labels,color=V(g)$color,radius=radii,opacity=1,fillOpacity=1) %>%
		addLegend(colors=coloresMapV,labels=labelsMapV) %>% 
addLegend(colors=coloresMapE,labels=labelsMapE) %>%
		addLabelOnlyMarkers(lng=l[,1],lat = l[,2],label=V(g)$name,labelOptions=labelOptions(textsize='8px'))

		output$leafletGraph = map
		}
	}
	return(output)
}

