summary <- function(rp.object) {

  #if(!is(rp.object,"responsePatterns"))
  #  base::summary(rp.object)
  #else {
    cat("--------------------------\n")
    cat("Response Patterns Analysis\n")
    cat("--------------------------\n")
    cat("\n")
    cat("Method: ",rp.object@options$method,"\n")
    cat("Number of variables: ",rp.object@n.vars,"\n")
    cat("Number of observations: ",rp.object@n.obs,"\n")
    cat("Contains: ")
    if(rp.object@percentile==0)
      cat("full data set")
    else
      cat("observations >= ",rp.object@percentile,"th percentile", sep="")
    cat("\n")
    if(rp.object@n.obs > 0) {
      cat("Remove missing values: ",rp.object@options$na.rm,"\n")

      if(rp.object@options$method=="acors") {
        cat("Correlation method: ",rp.object@options$cor.method,"\n")
        cat("Average max. auto-correlation: ",round(mean(rp.object@indices$max.abs.ac,na.rm=T),2),"\n")
        modal.lag <- rp.object@indices$max.ac.lag[which(rp.object@indices$max.ac.lag==max(rp.object@indices$max.ac.lag,na.rm=TRUE))][1]
        cat("The modal lag of max. auto-correlation: ",modal.lag,"\n")
        cat("Average number of failed auto-correlations: ",round(mean(rp.object@indices$n.failed.ac),2),"\n")
      }

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
  #}

}
