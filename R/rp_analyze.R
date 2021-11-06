rp.analyze <- function(data,
                     method=c("acors","patterns"),
                     max.lag=NULL,
                     id.var=NULL,
                     na.rm=FALSE,
                     cor.method=c("pearson","spearman","kendall"),
                     percentile.method=c("max","sum"),
                     store.data=TRUE
) {

  #Check the method parameter
  method <- method[1]
  if(!method %in% c("acors","patterns"))
    stop("method must be one of the following: acors, patterns")

  rp <- switch(method,
          "acors"=rp.acors(data=data,
                           max.lag=max.lag,
                           id.var=id.var,
                           na.rm=na.rm,
                           cor.method=cor.method,
                           percentile.method=percentile.method,
                           store.data=store.data),
          "patterns"=rp.patterns(data=data,
                                 #max.lag=max.lag,
                                 id.var=id.var,
                                 na.rm=na.rm,
                                 #cor.method=cor.method,
                                 #percentile.method=percentile.method,
                                 store.data=store.data)
               )

  return(rp)

}
