rp.save2csv <- function(rp.object,
                        file="rp_results.csv",
                        csv=c("csv","csv2"),
                        include.data=TRUE
) {

  #Check rp.object
  if(!is(rp.object,"responsePatterns"))
    stop("The object is not of class responsePatterns")
  #Check the csv parameter
  csv <- csv[1]
  if(!csv %in% c("csv","csv2"))
    stop("CSV file type not correctly specified")

  output <- rp.object@indices
  if(include.data==TRUE)
    output <- cbind(rp.object@data, output)
  if(rp.object@options$id.var!="")
    output <- cbind(rp.object@id,output)

  if(csv=="csv")
    write.csv(output, file=file)
  else
    write.csv2(output, file=file)

}
