#' Makes a layout to highlight the new suggested package
#'
#' It is useful to understand to wich packages the new one is related
#' @param gs The package's graph, as an igraph object. Usually the Suggests graph. 
#' @param suggested The suggested package.
#' @param gd The package's graph, as an igraph object. Usually the Depends graph. Default is NULL. If not, the two kinds of relations are used to make the layout.
#' @param recenter.suggested If TRUE, the new package is recentered
#' @return A matrix to be used as layout.
#' @export
#' @examples
#' sgs = add.a.package(gs)
#' pack = give.me.one(gs)
#' layout = layout_suger(gs,pack)
#' plot(sgs,layout=layout)
#' importsFrom igraph layout_nicely degree

layout_suger <- function(gs,suggested,gd=NULL,recenter.suggested=TRUE){
    if(is.null(gd)){
        g = gs
        l0 = layout_nicely(g)
	if(!recenter.suggested){
		return(l0)
	}
        isolates = degree(g)==0
        xs = mean(l0[isolates,1],na.rm=TRUE)
        ys = mean(l0[isolates,2],na.rm=TRUE)
        if(is.na(xs)) xs = 0
        if(is.na(ys)) ys = 0
        xc = mean(l0[!isolates,1],na.rm=TRUE)
        yc = mean(l0[!isolates,2],na.rm=TRUE)
        l = matrix(NA,nrow=sum(isolates),ncol=2) 
        vs = max(var(l0[isolates,1]),l0[isolates,2])
        vc = max(var(l0[!isolates,1]),l0[!isolates,2])
        if(sum(isolates)>0){
            for(i in 1:sum(isolates)){
                r = runif(n = 1,min = 0,max = min(vs,vc,na.rm=TRUE)/10)
                phi = runif(n=1,min=0,max=2*pi)
                l[i,1] = r*cos(phi)+xs
                l[i,2] = r*sin(phi)+ys
            }
        }
        l0[isolates,] = l
        xcM = max(l0[!isolates,1])
        ycM = max(l0[!isolates,2])
        xcm = min(l0[!isolates,1])
        ycm = min(l0[!isolates,2])
        if(xc<xs & yc<ys){
            l0[grep(pattern=suggested,x=V(g)$name),] = matrix(c(xcm,ycm),nrow=1,ncol=2)        
        }
        else{
            if(xc>xs & yc<ys){
                l0[grep(pattern=suggested,x=V(g)$name),] = matrix(c(xcM,ycm),nrow=1,ncol=2)        
            }
            else{
                if(xc>xs & yc>ys){
                    l0[grep(pattern=suggested,x=V(g)$name),] = matrix(c(xcM,ycM),nrow=1,ncol=2)        
                }
                else{
                    l0[grep(pattern=suggested,x=V(g)$name),] = matrix(c(xcm,ycM),nrow=1,ncol=2)        
                }
            }
        }
        return(l0)
    }
    else{
        g = igraph::union(gs,gd)
        l0 = layout_nicely(g)
        if(!recenter.suggested){
            return(l0)
        }
        isolates = degree(g)==0
        xs = mean(l0[isolates,1],na.rm=TRUE)
        ys = mean(l0[isolates,2],na.rm=TRUE)
        if(is.na(xs)) xs = 0
        if(is.na(ys)) ys = 0
        xc = mean(l0[!isolates,1],na.rm=TRUE)
        yc = mean(l0[!isolates,2],na.rm=TRUE)
        l = matrix(NA,nrow=sum(isolates),ncol=2) 
        vs = max(var(l0[isolates,1]),l0[isolates,2])
        vc = max(var(l0[!isolates,1]),l0[!isolates,2])
        if(sum(isolates)>0){
            for(i in 1:sum(isolates)){
                r = runif(n = 1,min = 0,max = min(vs,vc,na.rm=TRUE)/10)
                phi = runif(n=1,min=0,max=2*pi)
                l[i,1] = r*cos(phi)+xs
                l[i,2] = r*sin(phi)+ys
            }
        }
        l0[isolates,] = l
        xcM = max(l0[!isolates,1])
        ycM = max(l0[!isolates,2])
        xcm = min(l0[!isolates,1])
        ycm = min(l0[!isolates,2])
        if(xc<xs & yc<ys){
            l0[grep(pattern=suggested,x=V(g)$name),] = matrix(c(xcm,ycm),nrow=1,ncol=2)        
        }
        else{
            if(xc>xs & yc<ys){
                l0[grep(pattern=suggested,x=V(g)$name),] = matrix(c(xcM,ycm),nrow=1,ncol=2)        
            }
            else{
                if(xc>xs & yc>ys){
                    l0[grep(pattern=suggested,x=V(g)$name),] = matrix(c(xcM,ycM),nrow=1,ncol=2)        
                }
                else{
                    l0[grep(pattern=suggested,x=V(g)$name),] = matrix(c(xcm,ycM),nrow=1,ncol=2)        
                }
            }
        }
        return(l0)

    }
}

