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

  if(rp.object@options$method=="acors") {
    main.text <- "Histogram of auto-correlations"

    if(rp.object@options$percentile.method=="max") {
      what <- rp.object@indices$max.abs.ac
      xlab.text <- "Max. auto-correlation"
    }

    if(rp.object@options$percentile.method=="sum") {
      what <- rp.object@indices$sum.abs.ac
      xlab.text <- "Sum of auto-correlations"
    }

  }

  if(rp.object@options$method=="patterns") {
    main.text <- "Histogram of pattern scores"
    what <- rp.object@indices$score
    xlab.text <- "Pattern score"
  }

  graphics::hist(what, xlab=xlab.text, main=main.text) #xlim=c(0,1), breaks=10,
  #graphics::lines(stats::density(what, adjust=1), col="blue")

}
