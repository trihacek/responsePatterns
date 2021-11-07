#' Auto-correlation screening
#'
#' Auto-correlations of survey data allow for a probabilistic detection of repetitive patterns. This function calculates auto-correlation coefficients for all lags up to the value defined by the max.lag parameter for each observation (respondent). Subsequently, it assigns a percentile value to each observation (respondent) based either on the highest absolute auto-correlation or the sum of absolute auto-correlations. It is essential to keep the variables in the order in which they were presented to respondents.
#'
#' A response pattern yields perfect positive autocorrelation coefficient (r = 1) when the lag is equal to the length of the pattern, provided the pattern itself is uninterrupted over the whole vector of responses. There are two reasons for which the computation of auto-correlation computation can fail, both of which are associated with possible threat to data validity: (1) the pattern is composed of a vector of identical values (e.g., 2,2,2,2,2,2,2). In such cases, a correlation coefficient cannot be computed due to a zero variance but we arbitrarily set the value r = 1 because it meets the definition of a perfectly repetitive pattern; (2) the sequence contains too many missing values. In such cases we set the value to NA. However, in computing the sum of absolute auto-correlation values we treat these values as “r = 1” since they warrant closer inspection.
#'
#' Choosing a suitable maximum lag value, i.e. the maximum number of positions for the data to be shifted in autocorrelation analysis, is very important for a reliable screening. Maximum k value translates into the maximum length of a sequence within a repetitive response pattern that can be efficiently detected. Too low maximum k value hinders autocorrelation screening ability to detect longer repetitive response patterns, thus potentially lowering the method’s sensitivity (the ability to correctly detect careless respondents). On the other hand, maximum k value set too high generally lowers reliability, because it makes the instrumental data matrix smaller, and, by calculating higher numbers of autocorrelation coefficients, allows for higher frequency of occasionally strong autocorrelations that would inflate respondent’s final autocorrelation score (determined as the highest absolute autocorrelation coefficient found), thus lowering the method’s specificity (the ability to correctly not detect attentive respondents). If not specified by the used, the max.lag value is set to the number of variables – 3 internally.
#' @param data A data set containing variables to analyze and (optionally) an ID variable.
#' @param max.lag An integer. Define the maximum lag for which auto-correlations should be computed (defaults to number of variables - 3).
#' @param min.lag An integer. Define the minimum lag for which auto-correlations should be computed (defaults to 1).
#' @param id.var A string. If the data set contains an ID variable, specify it's name.
#' @param na.rm A logical scalar. Should missing values be removed from the computation of auto-correlations?
#' @param cor.method A string. Defined the method used to compute auto-correlations (defaults to "pearson").
#' @param percentile.method A string. Should the percentiles be based on the maximum absolute auto-correlation or on the sum of the absolute values of all auto-correlations (defaults to "max").
#' @param store.data A logical scalar. Should the data be stored within the object? Set to TRUE if you want to use the rp.plot or rp.save2csv functions.
#'
#' @return Returns an S4 object of class "ResponsePatterns".
#' @export
#'
#' @references Gottfried, J., Jezek, S., & Kralova, M. (2021). *Autocorrelation screening: A potentially efficient method for detecting repetitive response patterns in questionnaire data.* Manuscript submitted for review.
#' @seealso \code{\link{rp.patterns}}, \code{\link{rp.indices}}, \code{\link{rp.select}}, \code{\link{rp.hist}}, \code{\link{rp.plot}}, \code{\link{rp.save2csv}}
#'
#' @examples
#' rp.acors(rp.simdata, max.lag=10, id.var="optional_ID")
rp.acors <- function(data,
                     max.lag=NULL,
                     min.lag=1,
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
  #Check the min.lag value
  min.lag <- suppressWarnings(as.numeric(min.lag))
  if(is.na(min.lag) | min.lag > max.lag)
    stop("min.lag must be an integer smaller or equal to max.lag")
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

  acors.df <- as.data.frame(matrix(nrow=nrow(data),ncol=(max.lag-min.lag+1)))
  indices.df <- as.data.frame(matrix(nrow=nrow(data),ncol=5))
  rownames(acors.df) <- rownames(indices.df) <- rownames(data)
  colnames(acors.df) <- paste0("lag",c(min.lag:max.lag))
  colnames(indices.df) <- c("sum.abs.ac","max.abs.ac","max.ac.lag","n.failed.ac","percentile")
  for(i in 1:nrow(data)) {
    failed <- 0
    for(lag in min.lag:max.lag) {
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
        if(is.na(stats::var(row1)) | is.na(stats::var(row1)))
          ac <- NA
        #In case zero variance prevents us from computing an auto-correlation
        else if(stats::var(row1, na.rm=TRUE)==0 | stats::var(row2, na.rm=TRUE)==0)
          ac <- 1
        #Compute the auto-correlation
        else
          ac <- suppressWarnings(stats::cor(row1,row2,method=cor.method,use="pairwise.complete.obs"))
      }
      if(is.na(ac))
        failed <- failed + 1
      acors.df[i,paste0("lag",lag)] <- ac
      indices.df$n.failed.ac[i] <- failed
    }
    acors <- abs(acors.df[i,paste0("lag",c(min.lag:max.lag))])
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
  rp <- methods::new("ResponsePatterns",
            options=list(
              method="acors",
              max.lag=max.lag,
              min.lag=min.lag,
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
            coefficients=as.data.frame(acors.df[,paste0("lag",c(min.lag:max.lag))]),
            indices=indices.df
  )

  return(rp)

}
