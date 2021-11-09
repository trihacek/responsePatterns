#' Select observations
#'
#' This function reorders observations and selects those equal of above a defined percentile.
#'
#' @param rp.object A ResponsePatterns object.
#' @param percentile An integer. Defines a percentile cutoff. Setting the value to zero keeps all observations but the data are ordered based on the percentile.
#'
#' @return A ResponsePatterns object.
#' @export
#'
#' @seealso \code{\link{rp.acors}}, \code{\link{rp.patterns}}
#'
#' @examples
#' rp <- rp.acors(rp.simdata, id.var="optional_ID")
#' rp <- rp.select(rp, percentile=80)
rp.select <- function(rp.object,
                      percentile=90
) {

  #Check rp.object
  if(!methods::is(rp.object,"ResponsePatterns"))
    stop("The object is not of class ResponsePatterns")
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
