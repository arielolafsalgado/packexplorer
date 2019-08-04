#' Discover new packages, related to the ones you have
#'
#' This function gives you an ego network of a package (the package and its surrounders).
#' @param pack The package(s) of interest.
#' @param plot.it Should the network be plotted? Default is TRUE
#' @param on.leaflet Should the plot be done in leaflet? Default is TRUE
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


who.are.you <- function(pack,plot.it = TRUE,facts=FALSE,on.leaflet=TRUE,nwords=5,add_my_packs=FALSE,my_packs_neighbors=FALSE,point.size='downloads',min.point.size=15,max.point.size=30,...){
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

	if(length(pack)==1){
		grado_sug = degree(graph=gs,v=pack,mode='all')
		grado_dep = degree(graph=gd,v=pack,mode='all')
		Nlo_sugieren = degree(graph=gs,v=pack,mode='out')
		Nsugiere_a = degree(graph=gs,v=pack,mode='in')
		Ndepende_de = degree(graph=gd,v=pack,mode='in')
		Ndependen_de_el = degree(graph=gd,v=pack,mode='out')
		depende_de = neighbors(graph=gd,v=pack,mode='in')
		dependen_de_el = neighbors(graph=gd,v=pack,mode='out')
		lo_sugieren = neighbors(graph=gs,v=pack,mode='out')
		sugiere_a = neighbors(graph=gs,v=pack,mode='in')
		importa_a = neighbors(graph=gi,v=pack,mode='in')
		importan_a_el = neighbors(graph=gi,v=pack,mode='out')
		lo_mejoran = neighbors(graph=ge,v=pack,mode='out')
		mejora_a = neighbors(graph=ge,v=pack,mode='in')
		gs_vecindario = induced_subgraph(graph = gs, vids = c(pack,(igraph::union(lo_sugieren,sugiere_a))$name))
		gd_vecindario = induced_subgraph(graph = gd, vids = c(pack,(igraph::union(depende_de,dependen_de_el))$name))
		ge_vecindario = induced_subgraph(graph = ge, vids = c(pack,(igraph::union(mejora_a,lo_mejoran))$name))
		gi_vecindario = induced_subgraph(graph = gi, vids = c(pack,(igraph::union(importa_a,importan_a_el))$name))
		packlist = c(pack,(igraph::union(lo_sugieren,sugiere_a))$name,(igraph::union(depende_de,dependen_de_el))$name,(igraph::union(importan_a_el,importa_a))$name,(igraph::union(mejora_a,lo_mejoran))$name)
		if(add_my_packs){
			my_packs = rownames(installed.packages())
			if(my_packs_neighbors){
				g_full = igraph::union(gs,igraph::union(gd,igraph::union(gi,ge)))
				my_packs = intersect(my_packs,neighbors(graph=g_full,v=pack, mode = 'all'))
			}
			packlist = c(packlist,my_packs)
			packlist = intersect(packlist,union(V(gs)$name,V(gd)$name))
		}
		g = igraph::induced_subgraph(graph = igraph::union(igraph::union(igraph::union(gs,gd),gi),ge), vids = packlist)
		V(g)$color = 'black'
		V(g)$color[is.element(V(g)$name,V(gs_vecindario)$name)] = 'red'
		V(g)$color[is.element(V(g)$name,V(gd_vecindario)$name)] = 'blue'
		V(g)$color[is.element(V(g)$name,V(gi_vecindario)$name)] = 'green'
		V(g)$color[is.element(V(g)$name,V(ge_vecindario)$name)] = 'orange'
		V(g)$color[is.element(V(g)$name,V(gs_vecindario)$name) & is.element(V(g)$name,V(gd_vecindario)$name)] = 'violet'
		V(g)$color[is.element(V(g)$name,rownames(installed.packages()))] = 'white'
		V(g)$color[is.element(V(g)$name,pack)] = 'black'
		Eg = as_ids(E(g))
		Egs = as_ids(E(gs_vecindario))
		Egd = as_ids(E(gd_vecindario))
		Egi = as_ids(E(gi_vecindario))
		Ege = as_ids(E(ge_vecindario))
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
		output$grado.dep = grado_dep
		output$grado.sug = grado_sug
		output$lo.sugieren = lo_sugieren$name
		output$sugiere.a = sugiere_a$name
		output$depende.de = depende_de$name
		output$dependen.de.el = dependen_de_el$name
		output$num.lo.sugieren = Nlo_sugieren
		output$num.sugiere.a = Nsugiere_a
		output$num.depende.de = Ndepende_de
		output$num.dependen.de.el = Ndependen_de_el
		output$grafo = g
		if(plot.it){
			if(on.leaflet){
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

				return(map)
			}
			else{
				plot(g,...)
			}
		}
		if(facts){
			return(output)
		}
	}
	if(length(pack)>1){
		output = list()

		for(p in pack){
			grado_sug = degree(graph=gs,v=p,mode='all')
			grado_dep = degree(graph=gd,v=p,mode='all')
			Nlo_sugieren = degree(graph=gs,v=p,mode='out')
			Nsugiere_a = degree(graph=gs,v=p,mode='in')
			Ndepende_de = degree(graph=gd,v=p,mode='in')
			Ndependen_de_el = degree(graph=gd,v=p,mode='out')
			depende_de = neighbors(graph=gd,v=p,mode='in')
			dependen_de_el = neighbors(graph=gd,v=p,mode='out')
			lo_sugieren = neighbors(graph=gs,v=p,mode='out')
			sugiere_a = neighbors(graph=gs,v=p,mode='in')
			gs_vecindario = induced_subgraph(graph = gs, vids = c(p,igraph::union(lo_sugieren,sugiere_a)$name))
			gd_vecindario = induced_subgraph(graph = gd, vids = c(p,igraph::union(depende_de,dependen_de_el)$name))
			packlist = c(p,igraph::union(lo_sugieren,sugiere_a)$name,igraph::union(depende_de,dependen_de_el)$name)
			if(add_my_packs){
				my_packs = rownames(installed.packages())
				if(my_packs_neighbors){
					my_packs = intersect(my_packs,neighbors(graph=igraph::union(gs,gd),v=p, mode = 'all'))
				}
				packlist = c(packlist,my_packs)
				packlist = intersect(packlist,union(V(gs)$name,V(gd)$name))
			}
			g = induced_subgraph(graph = igraph::union(gs,gd), vids = packlist)

			V(g)$color = 'black'
			V(g)$color[is.element(V(g)$name,V(gs_vecindario)$name)] = 'red'
			V(g)$color[is.element(V(g)$name,V(gd_vecindario)$name)] = 'blue'
			V(g)$color[is.element(V(g)$name,V(gs_vecindario)$name) & is.element(V(g)$name,V(gd_vecindario)$name)] = 'violet'
			V(g)$color[is.element(V(g)$name,rownames(installed.packages()))] = 'white'
			V(g)$color[is.element(V(g)$name,pack)] = 'black'
			Eg = as_ids(E(g))
			Egs = as_ids(E(gs_vecindario))
			Egd = as_ids(E(gd_vecindario))
			E(g)$color = 'gray'
			E(g)$color[is.element(Eg,Egs) & is.element(tail_of(graph = g, es = E(g))$name, p)] = 'pink'
			E(g)$color[is.element(Eg,Egs) & is.element(head_of(graph = g, es = E(g))$name, p)] = 'red'
			E(g)$color[is.element(Eg,Egd) & is.element(tail_of(graph = g, es = E(g))$name, p)] = 'green'
			E(g)$color[is.element(Eg,Egd) & is.element(head_of(graph = g, es = E(g))$name, p)] = 'blue'
			output$grado.dep = rbind(output$grado.dep,grado_dep)

			output$grado.sug = rbind(output$grado.sug,grado_sug)
			output$lo.sugieren = c(output$lo.sugieren,list(p=lo_sugieren$name))
			output$sugiere.a = c(output$sugiere.a,list(p=sugiere_a$name))
			output$depende.de = c(output$depende.de,list(p=depende_de$name))
			output$dependen.de.el = c(output$dependen.de.el,list(p=dependen_de_el$name))
			output$num.lo.sugieren = rbind(output$num.lo.sugieren,Nlo_sugieren)
			output$num.sugiere.a = rbind(output$num.sugiere.a,Nsugiere_a)
			output$num.depende.de = rbind(output$num.depende.de,Ndepende_de)
			output$num.dependen.de.el = rbind(output$num.dependen.de.el,Ndependen_de_el)
			output$grafo = c(output$grafo,list(p=g))
		}	
		
		colnames(output$grado.dep) = 'grado.dep'
		rownames(output$grado.dep) = pack
		colnames(output$grado.sug) = 'grado.sug'
		rownames(output$grado.sug) = pack
		colnames(output$num.lo.sugieren) = 'lo.sugieren'
		rownames(output$num.lo.sugieren) = pack
		colnames(output$num.sugiere.a) = 'sugiere.a'
		rownames(output$num.sugiere.a) = pack
		colnames(output$num.dependen.de.el) = 'dependen.de.el'
		rownames(output$num.dependen.de.el) = pack
		colnames(output$num.depende.de) = 'depende.de'
		rownames(output$num.depende.de) = pack
		names(output$lo.sugieren) = pack
		names(output$sugiere.a) = pack
		names(output$depende.de) = pack
		names(output$dependen.de.el) = pack
		names(output$grafo) = pack
		en.total = list()
		dependencias_compartidas = matrix(NA,nrow=length(pack),ncol=length(pack))
		rownames(dependencias_compartidas) = pack
		colnames(dependencias_compartidas) = pack
		sugerencias_compartidas = matrix(NA,nrow=length(pack),ncol=length(pack))
		rownames(sugerencias_compartidas) = pack
		colnames(sugerencias_compartidas) = pack
		sugerentes_compartidos = matrix(NA,nrow=length(pack),ncol=length(pack))
		rownames(sugerentes_compartidos) = pack
		colnames(sugerentes_compartidos) = pack
		dependientes_compartidos = matrix(NA,nrow=length(pack),ncol=length(pack))
		rownames(dependientes_compartidos) = pack
		colnames(dependientes_compartidos) = pack
		suggesteds_compartidos = matrix(NA,nrow=length(pack),ncol=length(pack))
		rownames(suggesteds_compartidos) = pack
		colnames(suggesteds_compartidos) = pack

		for(p in pack){
			for(q in pack){
				dependencias_compartidas[p,q] = length(intersect(c(output$depende.de[[p]],output$dependen.de.el[[p]]),c(output$depende.de[[q]],output$dependen.de.el[[q]])))
				sugerencias_compartidas[p,q] = length(intersect(c(output$sugiere.a[[p]],output$lo.sugieren[[p]]),c(output$sugiere.a[[q]],output$lo.sugieren[[q]])))
				sugerentes_compartidos[p,q] = length(intersect(c(output$lo.sugieren[[p]]),c(output$lo.sugieren[[q]])))
				suggesteds_compartidos[p,q] = length(intersect(c(output$sugiere.a[[p]]),c(output$sugiere.a[[q]])))
				dependientes_compartidos[p,q] = length(intersect(c(output$dependen.de.el[[p]]),c(output$dependen.de.el[[q]])))
			}
		}
		en.total$dependencias.compartidas = dependencias_compartidas
		en.total$sugerencias.compartidas = sugerencias_compartidas
		en.total$sugerentes.compartidos = sugerentes_compartidos
		en.total$suggesteds.compartidos = suggesteds_compartidos
		en.total$dependientes.compartidos = dependientes_compartidos
		ug = output$grafo[[1]]
		for(grafo in output$grafo[2:length(pack)]){
			ug = igraph::union(ug,grafo,byname=TRUE)
		}
		colatt = grep('color',names(vertex.attributes(ug)),value=TRUE)
		color_ = vertex.attributes(ug)[colatt]
		color_ = as.data.frame(color_)
		color = rep(NA,length(V(ug)))
		for(i in 1:length(V(ug))){
			col_ = color_[i,]
#			if(sum(is.na(col_))==(length(pack)-1)){
#				color[i] = as.character(col_[!is.na(col_)])
#			}
			if(length(unique(col_[!is.na(col_)]))==1){
				color[i] = unique(col_[!is.na(col_)])
			}
			else{
				color[i] = 'brown'
			}
		}
		V(ug)$color = color

		ecolatt = grep('color',names(edge.attributes(ug)),value=TRUE)
		ecolor_ = edge.attributes(ug)[ecolatt]
		ecolor_ = as.data.frame(ecolor_)
		ecolor = rep(NA,length(E(ug)))
		for(i in 1:length(E(ug))){
			col_ = ecolor_[i,]
#			if(sum(is.na(col_))==(length(pack)-1)){
#				color[i] = as.character(col_[!is.na(col_)])
#			}
			if(length(unique(col_[!is.na(col_)]))==1){
				ecolor[i] = unique(col_[!is.na(col_)])
			}
			else{
				ecolor[i] = 'brown'
			}
		}
		E(ug)$color = ecolor
		en.total$grafo = ug
		output$compartidos = en.total
		if(plot.it){
			if(on.leaflet){
				l = scale(layout_nicely(ug))
				listConections = list_conections(ug,l)
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
						labels = lapply(1:length(V(g)), function(i){paste(labs[[i]],' <p>','Downloads rate: ',round(desc.rate[i],3),'</p>',sep='')})
						labels = lapply(labels, HTML)
					}else{
						radii = min.point.size
						labels = lapply(labs, HTML)
					}
				}

				map <- leaflet()  %>% addPolylines(data=listConections,lng=~x,lat = ~y,weight=2,color=E(ug)$color) %>% addCircleMarkers(lng=l[,1],lat = l[,2],label=labels,color=V(ug)$color,radius=radii,opacity=1,fillOpacity=1)
				return(map)
			}
			else{
				plot(ug,...)
			}
		}
		if(facts){
			return(output)
		}
	}
}

