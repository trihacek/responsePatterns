#' Plots a histogram of the main index
#'
#' This function plots a histogram of the main "suspicion" index. The choice of the index depends on the type and setting of the analysis: it is either the maximum absolute auto-correlation or the sum of absolute auto-correlations if analyzed via the \code{\link{rp.acors}} function and the total score of analyzed via the \code{\link{rp.patterns}} function.
#'
#' @param rp.object A ResponsePatterns object.
#'
#' @return Returns a plot.
#' @export
#'
#' @seealso \code{\link{rp.acors}}, \code{\link{rp.patterns}}
#'
#' @examples
#' rp <- rp.acors(rp.simdata, id.var="optional_ID")
#' rp.hist(rp)
rp.hist <- function(rp.object){

  #Check rp.object
  if(!methods::is(rp.object,"ResponsePatterns"))
    stop("The object is not of class ResponsePatterns")

  density.col = 'blue'
  density.adjust = 1

  if(rp.object@options$method=="acors") {

    if(rp.object@options$percentile.method=="max") {
      graphics::hist(rp.object@indices$max.abs.ac, xlim=c(0,1), breaks=10,
                     xlab="Max. auto-correlation",
                     main="Histogram of auto-correlations")
      graphics::lines(stats::density(rp.object@indices$max.abs.ac, adjust=density.adjust),
                      col=density.col)
    }

    if(rp.object@options$percentile.method=="sum") {
      graphics::hist(rp.object@indices$sum.abs.ac,
                     xlab="Sum of auto-correlations",
                     main="Histogram of auto-correlations")
      graphics::lines(stats::density(rp.object@indices$sum.abs.ac, adjust=density.adjust),
                      col=density.col)
    }

  }

  if(rp.object@options$method=="patterns") {
    graphics::hist(rp.object@indices$score, xlim=c(0,1), breaks=10,
                   xlab="Pattern score",
                   main="Histogram of pattern scores")
    graphics::lines(stats::density(rp.object@indices$score, adjust=density.adjust),
                    col=density.col)
  }

}
