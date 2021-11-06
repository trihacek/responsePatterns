rp.indices <- function(rp.object,
                       round=2,
                       include.coefs=TRUE
) {

  #Check rp.object
  if(!is(rp.object,"responsePatterns"))
    stop("The object is not of class responsePatterns")

  indices <- round(rp.object@indices,round)
  if(include.coefs==TRUE)
    indices <- cbind(round(rp.object@coefficients,round),indices)
  if(rp.object@options$id.var=="")
    indiced <- cbind(rp.object@id,indices)
  return(indices)

}
