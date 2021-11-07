#' Plot an individual response
#'
#' This function plots an individual response for easier visual inspection. The observation can be identified by one of the following methods: observation number (obs), row name (rowname), or value of the ID variable (id, if defined in the rp.object). Only one of these identifiers should be specified. Using this function requires that the data are stored in the ResponsePatterns object.
#'
#' @param rp.object A ResponsePatterns object.
#' @param obs An integer. The number of observation to plot.
#' @param rowname A string. The row name of the observation to plot.
#' @param id A string. The value of the ID variable (if defined in the ResponsePatterns object).
#' @param plot A logical scalar. Should a the data be plotted?
#' @param text.output A logical scalar. Should a the data be printed to the console?
#' @param groups A list of vectors. Defines groups of items that should be plotted using the same color.
#' @param page.breaks A vector. Draws a vertical line after the items (useful if you want to display the pagination of the questionnaire in the plot).
#' @param bw A logical scalar. Should the plot be printed in black and white?
#'
#' @return Plots a graph.
#' @export
#'
#' @seealso \code{\link{rp.acors}}, \code{\link{rp.patterns}}, \code{\link{rp.plots2pdf}}
#'
#' @examples
#' rp <- rp.acors(rp.simdata, id.var="optional_ID")
#' rp.plot(rp, obs=1)
#' rp.plot(rp, rowname="12", groups=list(c(1:10),c(11:20)))
#' rp.plot(rp, id="Natalya", page.breaks=c(5,10,15))
rp.plot <- function(rp.object,
                    obs=NULL,
                    rowname=NULL,
                    id=NULL,
                    plot=TRUE,
                    text.output=FALSE,
                    groups=NULL,
                    page.breaks=NULL,
                    bw=FALSE
){

  #Check rp.object
  if(!methods::is(rp.object,"ResponsePatterns"))
    stop("The object is not of class ResponsePatterns")
  if(nrow(rp.object@data)==0 | ncol(rp.object@data)==0)
    stop("Use store.data=TRUE in rp.acors")
  #Resolve obs vs rowname
  if(is.null(obs) & is.null(rowname) & is.null(id))
    stop("One of the following identifiers must be specified: obs, rowname, id")
  #if(!is.null(obs) & !is.null(rowname))
  #  message("Both obs and rowname were specified, rowname was used")
  if(!is.null(obs)){
    obs <- suppressWarnings(as.integer(obs))
    if(is.na(obs))
      stop("obs must be an integer")
  }
  #If rowname is specified, rowname is used
  if(!is.null(rowname)){
    if(!rowname %in% rownames(rp.object@data))
      stop("rowname not found in data set")
    obs <- which(rownames(rp.object@data)==rowname)
  }
  #If id is specified, id is used
  if(!is.null(id)){
    if(!id %in% rp.object@id)
      stop("id not found in data set")
    obs <- which(rp.object@id==id)
  }
  #Validate obs
  if(obs > rp.object@n.obs)
    stop("obs is higher than the number of rows in the data")
  #Validate the groups parameter
  if(!is.null(groups)) {
    if(!is.list(groups))
      stop("The groups parameter must be a list")
    for(group in groups)
      for(g in group)
        if(!g %in% c(1:rp.object@n.vars))
          stop("Groups contain nonexisting item number(s)")
  }
  #Check the page.breaks parameter
  if(!is.null(page.breaks))
    if(any(page.breaks > rp.object@n.vars))
      warning("page.breaks contains number(s) exceeding the number of items")
  #Transform groups to colors
  colors <- matrix(rep(1,rp.object@n.vars),nrow=1,ncol=rp.object@n.vars)
  i <- 1
  for(group in groups) {
    i <- i + 1
    colors[group] <- i
  }

  scale.levels <- levels(as.factor(unlist(rp.object@data)))

  if(text.output==TRUE) {
    cat("Obs = ", obs, ", rowname = ", rownames(rp.object@data[obs,]), "\n", sep="")
    for(i in 1:rp.object@n.vars) {
      if(!is.na(rp.object@data[obs,i]))
        cat(i,".\t", rep(" ",(2*which(scale.levels==rp.object@data[obs,i]))-1), rp.object@data[obs,i],"\n", sep="")
      else
        cat(i,".\t", rep(" ",(2*(length(scale.levels)+1)-1)), "<NA>","\n", sep="")
    }
  }

  if(plot==TRUE) {
    sub <- ""
    if(rp.object@options$method=="acors") {
      for(lag in rp.object@options$min.lag:rp.object@options$max.lag) {
        r <- rp.object@coefficients[obs,paste0("lag",lag)]
        if(is.na(r))
          r <- "NA"
        else if(r!=1) {
          r <- round(r,1)
          sign <- ifelse(r < 0,"-","")
          r <- ifelse(r==0,".0",substr(abs(r),start=2,stop=3))
        }
        sub <- paste0(sub,"L",i,"=",r,", ")
      }
      sub <- substr(sub,start=1,stop=(nchar(sub)-2))
    }
    graphics::plot(x=c(1:rp.object@n.vars), y=rp.object@data[obs,], type="l",
               main=paste0(deparse(substitute(rp.object)),", obs=", obs, ", rwnm=", rownames(rp.object@data[obs,]),", id=",rp.object@id[obs]),
               #main=paste0(deparse(substitute(rp.object)),", rwnm=", rownames(rp.object@data[obs,])),
               sub=sub,cex.sub=.80,
               ylim=c(as.numeric(min(scale.levels)),as.numeric(max(scale.levels))),
               xlab="Item", ylab="Response")
    if(bw==FALSE)
      graphics::lines(x=c(1:rp.object@n.vars), y=rp.object@data[obs,], type="p", pch=16,
            col=colors)
    else
      graphics::lines(x=c(1:rp.object@n.vars), y=rp.object@data[obs,], type="p", pch=colors+14,
            col=1)
    if(!is.null(page.breaks))
      for(pb in page.breaks)
        graphics::abline(v=pb+0.5, col="black", lty="dotted")
  }

}
