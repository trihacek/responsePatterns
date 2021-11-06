rp.hist <- function(rp.object){

  #Check rp.object
  if(!is(rp.object,"responsePatterns"))
    stop("The object is not of class responsePatterns")

  if(rp.object@options$method=="acors") {

    if(rp.object@options$percentile.method=="max")
      graphics::hist(rp.object@indices$max.abs.ac, xlim=c(0,1), breaks=10,
                     xlab="Max. auto-correlation",
                     main="Histogram of auto-correlations")

    if(rp.object@options$percentile.method=="sum")
      graphics::hist(rp.object@indices$sum.abs.ac,
                     xlab="Sum of auto-correlations",
                     main="Histogram of auto-correlations")

  }

  if(rp.object@options$method=="patterns")
    graphics::hist(rp.object@indices$score, xlim=c(0,1), breaks=10,
                   xlab="Pattern score",
                   main="Histogram of pattern scores")

}
