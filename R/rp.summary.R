#' SUmmary of an ResponsePatterns object
#'
#' @param rp.object A ResponsePatterns object.
#'
#' @return Prints a summary of a ResponsePatterns object.
#' @export
#'
#' @examples
#' rp <- rp.acors(rp.simdata, id.var="optional_ID")
#' rp.summary(rp)
#' summary(rp)
rp.summary <- function(rp.object) {

  cat("--------------------------\n")
  cat("Response Patterns Analysis\n")
  cat("--------------------------\n")
  cat("\n")
  cat("Method: ",rp.object@options$method,"\n")
  cat("Number of variables: ",rp.object@n.vars,"\n")
  cat("Number of observations: ",rp.object@n.obs,"\n")
  if(rp.object@options$method=="acors") {
    cat("Limited to: min.lag=",rp.object@options$min.lag,", max.lag=",rp.object@options$max.lag,"\n",sep="")
  }
  if(rp.object@options$method=="patterns") {
    cat("Limited to: min.length=",rp.object@options$min.length,", max.length=",rp.object@options$max.length,"\n",sep="")
    cat("Standarized patterns: ",rp.object@options$std.patterns,"\n")
  }
  cat("Contains: ")
  if(rp.object@percentile==0)
    cat("full data set")
  else
    cat("observations >= ",rp.object@percentile,"th percentile",sep="")
  cat("\n")
  if(rp.object@n.obs > 0) {
    cat("Remove missing values: ",rp.object@options$na.rm,"\n")
    #Object of type "acors"
    if(rp.object@options$method=="acors") {
      cat("Correlation method: ",rp.object@options$cor.method,"\n")
      cat("Average max. auto-correlation: ",round(mean(rp.object@indices$max.abs.ac,na.rm=T),2),"\n")
      modal.lag <- rp.object@indices$max.ac.lag[which(rp.object@indices$max.ac.lag==max(rp.object@indices$max.ac.lag,na.rm=TRUE))][1]
      cat("The modal lag of max. auto-correlation: ",modal.lag,"\n")
      cat("Average number of failed auto-correlations: ",round(mean(rp.object@indices$n.failed.ac),2),"\n")
    }
    #Object of type "patterns"
    if(rp.object@options$method=="patterns") {
      cat("Average score: ",round(mean(rp.object@indices$score,na.rm=T),2),"\n")
    }
  }
  cat("\n")
  cat("Tips\n")
  cat("\t- Use rp.indices() to extract max. auto-correlations and other indices\n")
  cat("\t- Use rp.select() to select cases for further exploration based on percentiles\n")
  cat("\t- Use rp.hist() to plot the distribution of max. auto-correlations\n")
  cat("\t- Use rp.plot() to plot individual responses for further exploration\n")
  cat("\t- Use rp.plots2pdf() to print a series of plots into a PDF file\n")
  cat("\t- Use rp.save2csv() to save results of the analysis into a CSV file\n")

}
