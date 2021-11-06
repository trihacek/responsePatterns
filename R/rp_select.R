rp.select <- function(rp.object,
                      percentile=90
) {

  #Check rp.object
  if(!is(rp.object,"responsePatterns"))
    stop("The object is not of class responsePatterns")
  #Check the prop parameter
  percentile <- round(percentile)
  if(percentile < 0 | percentile > 99)
    stop("percentile must be a number between 0 and 99")

  indices <- cbind(c(1:rp.object@n.obs),rp.object@indices)
  indices <- indices[indices$percentile >= percentile,]
  indices <- indices[order(indices$percentile, decreasing=TRUE),]
  selected.rows <- indices[,1]
  rp.object@indices <- indices[,-1]
  rp.object@coefficients <- as.data.frame(rp.object@coefficients[selected.rows,])
  if(nrow(rp.object@data)!=0 & ncol(rp.object@data)!=0) {
    rp.object@data <- as.data.frame(rp.object@data[selected.rows,])
  }
  rp.object@n.obs <- nrow(indices)

  rp.object@id = rp.object@id[selected.rows]
  rp.object@percentile = percentile

  return(rp.object)

}
