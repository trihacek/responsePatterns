#' Auto-correlation patterns
#'
#' @param data
#' @param max.lag
#' @param id.var
#' @param na.rm
#' @param cor.method
#' @param store.data
#'
#' @return
#' @export
#'
#' @examples
rp.acors <- function(data,
                     max.lag=NULL,
                     id.var=NULL,
                     na.rm=FALSE,
                     cor.method=c("pearson","spearman","kendall"),
                     percentile.method=c("max","sum"),
                     store.data=TRUE
) {

  #Minimal number of values to compute a correlation
  min.length <- 3
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
  if(ncol(data) < min.length+1)
    stop("Too few variables to compute auto-correlations")
  #Check if max.lag is an integer
  if(is.null(max.lag)) {
    max.lag <- length(levels(as.factor(unlist(data))))
    message(paste0("max.lag automatically set to ",max.lag," (i.e., the number of scale levels). We recommend trying values between ",max.lag," and ",2*max.lag))
  }
  max.lag <- suppressWarnings(as.numeric(max.lag))
  if(is.na(max.lag) | max.lag < 1)
    stop("max.lag must be an integer")
  #Check if max.lag too large
  if(ncol(data)-max.lag < min.length) {
    max.lag <- ncol(data)-min.length
    message(paste0("max.lag too high, automatically set to ",max.lag))
  }
  #Check the cor.method parameter
  cor.method <- cor.method[1]
  if(!cor.method %in% c("pearson","spearman","kendall"))
    stop("cor.method must be one of the following: pearson, spearman, kendall")
  #Check the cor.method parameter
  percentile.method <- percentile.method[1]
  if(!percentile.method %in% c("max","sum"))
    stop("cor must be one of the following: max, sum")

  acors.df <- as.data.frame(matrix(nrow=nrow(data),ncol=max.lag))
  indices.df <- as.data.frame(matrix(nrow=nrow(data),ncol=5))
  rownames(acors.df) <- rownames(indices.df) <- rownames(data)
  colnames(acors.df) <- paste0("lag",c(1:max.lag))
  colnames(indices.df) <- c("sum.abs.ac","max.abs.ac","max.ac.lag","n.failed.ac","percentile")
  for(i in 1:nrow(data)) {
    failed <- 0
    for(lag in 1:max.lag) {
      row <- unlist(data[i,])
      if(na.rm==TRUE)
        row <- row[!is.na(row)]
      #In case the vector is too short to compute an auto-correlation
      if(length(row)-lag < min.length) {
        ac <- NA
      } else {
        row1 <- row[c( 1 : (ncol(data)-lag) )]
        row2 <- row[c( (1+lag) : ncol(data) )]
        #In case the vector is too short to compute an auto-correlation
        #if(sum(is.na(row1))==length(row1) | sum(is.na(row2))==length(row2))
        #In case variance cannot be computed
        if(is.na(var(row1)) | is.na(var(row1)))
          ac <- NA
        #In case zero variance prevents us from computing an auto-correlation
        else if(var(row1, na.rm=TRUE)==0 | var(row2, na.rm=TRUE)==0)
          ac <- 1
        #Compute the auto-correlation
        else
          ac <- suppressWarnings(cor(row1,row2,method=cor.method,use="pairwise.complete.obs"))
      }
      if(is.na(ac))
        failed <- failed + 1
      acors.df[i,lag] <- ac
      indices.df$n.failed.ac[i] <- failed
    }
    acors <- abs(acors.df[i,c(1:max.lag)])
    if(sum(is.na(acors)) < length(acors)) {
      indices.df$sum.abs.ac[i] <- sum(acors,na.rm=T) + sum(is.na(acors))
      indices.df$max.abs.ac[i] <- max(acors,na.rm=TRUE)
      indices.df$max.ac.lag[i] <- which(acors == max(acors,na.rm=TRUE))[1]
    } else {
      indices.df$max.abs.ac[i] <- NA
      indices.df$max.ac.lag[i] <- NA
    }
  }

  #indices.df$percentile <- floor(rank(indices.df$max.abs.ac,na.last=FALSE) / nrow(indices.df) * 100)
  #indices.df$percentile <- floor(rank(indices.df$sum.abs.ac,na.last=FALSE) / nrow(indices.df) * 100)
  indices.df$percentile <- switch(percentile.method,
    "max"=floor(rank(indices.df$max.abs.ac,na.last=FALSE) / nrow(indices.df) * 100),
    "sum"=floor(rank(indices.df$sum.abs.ac,na.last=FALSE) / nrow(indices.df) * 100)
  )

  if(store.data==TRUE)
    store <- data
  else
    store <- data.frame()
  rp <- new("responsePatterns",
            options=list(
              method="acors",
              max.lag=max.lag,
              id.var=ifelse(!is.null(id.var),id.var,""),
              na.rm=na.rm,
              cor.method=cor.method,
              percentile.method=percentile.method
            ),
            id=id,
            percentile=0,
            n.obs=nrow(data),
            n.vars=ncol(data),
            data=store,
            coefficients=as.data.frame(acors.df[,1:max.lag]),
            indices=indices.df
  )

  return(rp)

}
