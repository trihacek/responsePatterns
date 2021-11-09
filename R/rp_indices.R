#' Extract indices from a ResponsePatterns object
#'
#' This function extracts indices from a ResponsePatterns object.
#'
#' @param rp.object A ResponsePatterns object.
#' @param round An integer. The number of decimal places to which the indices should be rounded.
#' @param include.coefs A logical scalar. Should the returned data frame include also the coefficients?
#'
#' @return Returns a data frame.
#' @export
#'
#' @seealso \code{\link{rp.acors}}, \code{\link{rp.patterns}}
#'
#' @examples
#' rp <- rp.acors(rp.simdata, id.var="optional_ID")
#' rp.indices(rp)
rp.indices <- function(rp.object,
                       round=2,
                       include.coefs=TRUE
) {

  #Check rp.object
  if(!methods::is(rp.object,"ResponsePatterns"))
    stop("The object is not of class ResponsePatterns")

  indices <- round(rp.object@indices,round)
  if(include.coefs==TRUE)
    indices <- cbind(round(rp.object@coefficients,round),indices)
  if(rp.object@options$id.var=="")
    indiced <- cbind(rp.object@id,indices)
  return(indices)

}
