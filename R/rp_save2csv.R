#' Export indices into a CSV file
#'
#' This functions exports the ResponsePatterns object indices and, optionally, coefficients and data.
#'
#' @param rp.object A ResponsePatterns object.
#' @param file A string. A filename or a path.
#' @param csv A string. Specify the CSV file format.
#' @param include.coefs A logical scalar. Should the exported file include the coefficients?
#' @param include.data A logical scalar. Should the exported file include the data?
#'
#' @return Exports a CSV file.
#' @export
#'
#' @examples
#' rp <- rp.acors(rp.simdata, id.var="optional_ID")
#' rp.save2csv(rp)
#' rp.save2csv(rp, include.coefs=FALSE, include.data=FALSE)
rp.save2csv <- function(rp.object,
                        file="rp_results.csv",
                        csv=c("csv","csv2"),
                        include.coefs=TRUE,
                        include.data=TRUE
) {

  #Check rp.object
  if(!is(rp.object,"ResponsePatterns"))
    stop("The object is not of class ResponsePatterns")
  #Check the csv parameter
  csv <- csv[1]
  if(!csv %in% c("csv","csv2"))
    stop("CSV file type not correctly specified")

  output <- rp.object@indices
  if(include.coefs==TRUE)
    output <- cbind(rp.object@coefficients, output)
  if(include.data==TRUE)
    output <- cbind(rp.object@data, output)
  if(rp.object@options$id.var!="")
    output <- cbind(rp.object@id,output)

  if(csv=="csv")
    write.csv(output, file=file)
  else
    write.csv2(output, file=file)

}
