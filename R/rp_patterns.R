#' Repetitive pattern analysis
#'
#' This function searches mechanically for repetitive patterns in the data. It searches for patterns of a given length (all values between min.length and max.length) using an iterative algorithm. The patterns are defined based on the data: if a sequence of values occurs more than once within an observation, it is considered a repetition. The algorithm counts the number of repetitions for each pattern length and then weighs this sum by the length of the pattern (longer patterns are assigned higher weight). The total score for each respondent is determined as the sum of scores achieved for each pattern length and is standardized to a value between 0 and 1. It is essential to keep the variables in the order in which they were presented to respondents.
#'
#' @param data A data frame. A data set containing variables to analyze and, optionally, an ID variable.
#' @param max.length An integer. Define the maximum length of a pattern (cannot be longer than the number of variables/2).
#' @param min.length An integer. Define the minimum length of a pattern (defaults to 2).
#' @param id.var A string. If the data set contains an ID variable, specify it's name.
#' @param na.rm A logical scalar. Should missing values be ignored when comparing sequences of data?
#' @param std.patterns A logical scalar. If set to true, patterns are "standardized" by subtracting the minimum value from all elements in the sequence. As a result, patterns are compared in terms of their relative relationships (i.e., "1-2-3" and "3-4-5" are considered identical patterns). If set to FALSE, patterns are compared in terms of their absolute values (i.e., "1-2-3" and "3-4-5" are considered distinct patterns).
#' @param na.top A logical scalar. Should NA indices (i.e., those that could not be computed due to data missingness) be ranked at the top? Defaults to FALSE.
#' @param store.data A logical scalar. Should the data be stored within the object? Set to TRUE if you want to use the rp.plot or rp.save2csv functions.
#'
#' @return Returns an S4 object of class "ResponsePatterns".
#' @export
#' @importFrom methods new
#'
#' @seealso \code{\link{rp.acors}}, \code{\link{rp.indices}}, \code{\link{rp.select}}, \code{\link{rp.hist}}, \code{\link{rp.plot}}, \code{\link{rp.save2csv}}
#'
#' @examples
#' rp.patterns(rp.simdata, id.var="optional_ID")
rp.patterns <- function(data,
                        max.length=NULL,
                        min.length=2,
                        id.var=NULL,
                        na.rm=FALSE,
                        std.patterns=TRUE,
                        na.top=FALSE,
                        store.data=TRUE
) {

  #Coerce data to data.frame
  data <- as.data.frame(data)
  #If id.var specified, remove it from data frame and store separately
  if(!is.null(id.var)) {
    if(!id.var %in% names(data))
      stop("id.var not found in the data")
    id <- data[,paste0(id.var)]
    data <- data[,-which(names(data)==id.var)]
  } else
    id <- rep(NA,nrow(data))
  #Check if data are numbers
  if(!typeof(data[[1]]) %in% c("integer","double"))
    stop("Data set contains other than numeric values")
  #Check if data set empty
  if(nrow(data)==0 | ncol(data)==0)
    stop("Data set is empty")
  if(ncol(data) < 4)
    stop("The analysis cannot proceed with a data set of less than four items")

  n.vars <- ncol(data)
  n.obs <- nrow(data)

  #Check the max.length parameter
  if(is.null(max.length) | !is.numeric(max.length))
    max.length <- floor(ncol(data)/2)
  if(max.length > floor(ncol(data)/2))
    max.length <- floor(ncol(data)/2)
  #Check the min.length parameter
  if(!is.numeric(min.length) | min.length > max.length)
    min.length <- max.length

  patterns.df <- as.data.frame(matrix(nrow=nrow(data),ncol=max.length-min.length+1))
  indices.df <- as.data.frame(matrix(nrow=nrow(data),ncol=2))
  rownames(patterns.df) <- rownames(indices.df) <- rownames(data)
  colnames(patterns.df) <- paste0("L",c(min.length:max.length))
  patterns.df[,] <- 0
  colnames(indices.df) <- c("score","percentile")

  patterns.df <- t(apply(data, 1, function(row) {
    row <- sapply(c(min.length:max.length),function(length) {
      count <- 0
      for(start in 1:(n.vars-length)) {
        #Learn a potential pattern
        pattern <- row[c(start:(start+length-1))]
        #Standardize the pattern
        if(std.patterns==TRUE)
          pattern <- pattern - min(pattern, na.rm=TRUE)
        #Search for the pattern in the remaining part of the row
        for(position in (start+1):(n.vars-length+1)) {
          #Read a sequence
          sequence <- row[c(position:(position+length-1))]
          #Standardize the sequence
          if(std.patterns==TRUE)
            sequence <- sequence - min(sequence, na.rm=TRUE)
          #Compare the sequence to the pattern
          is.equal <- all(sequence==pattern, na.rm=na.rm)
          if(!is.na(is.equal) & is.equal==TRUE)
            count <- count + 1
        }
      }

      #Weigh te count by the length of the pattern (more weight for longer patterns)
      max.rep <- n.vars - length
      #Tato varianta nadhodnocuje opakovani stejnych hodnot a nezohlednuje
      #fakt, ze pocet ruznych vzorcu muze byt teoreticky vyssi nez toto maximum
      #max.rep <- floor(n.vars/length)-1
      count <- count/max.rep
      return(count)
    })
    return(row)
  }))

  indices.df$score <- rowSums(patterns.df)
  indices.df$score <- indices.df$score/max(indices.df$score)
  indices.df$percentile <- floor(rank(indices.df$score,na.last=na.top) / nrow(indices.df) * 100)

  if(store.data==TRUE)
    store <- data
  else
    store <- data.frame()
  rp <- methods::new("ResponsePatterns",
            options=list(
              method="patterns",
              max.length=max.length,
              min.length=min.length,
              id.var=ifelse(!is.null(id.var),id.var,""),
              na.rm=na.rm,
              std.patterns=std.patterns,
              cor.method="none"
            ),
            id=id,
            percentile=0,
            n.obs=n.obs,
            n.vars=n.vars,
            data=store,
            coefficients=as.data.frame(patterns.df),
            indices=indices.df
  )

  return(rp)

}
