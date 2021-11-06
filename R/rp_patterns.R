rp.patterns <- function(data,
                        id.var=NULL,
                        na.rm=FALSE,
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
  min.length <- 2
  max.length <- floor(ncol(data)/2)
  patterns.df <- as.data.frame(matrix(nrow=nrow(data),ncol=max.length-min.length+1))
  indices.df <- as.data.frame(matrix(nrow=nrow(data),ncol=2))
  rownames(patterns.df) <- rownames(indices.df) <- rownames(data)
  colnames(patterns.df) <- paste0("L",c(2:max.length))
  patterns.df[,] <- 0
  colnames(indices.df) <- c("score","percentile")

  patterns.df <- t(apply(data, 1, function(row) {
    row <- sapply(c(min.length:max.length),function(length) {
      count <- 0
      for(start in 1:(n.vars-length)) {
        #Learn a potential pattern
        pattern <- row[c(start:(start+length-1))]
        #Standardize the pattern
        pattern <- pattern - min(pattern, na.rm=TRUE)
        #Search for the pattern in the remaining part of the row
        for(position in (start+1):(n.vars-length+1)) {
          #Read a sequence
          sequence <- row[c(position:(position+length-1))]
          #Standardize the sequence
          sequence <- sequence - min(sequence, na.rm=TRUE)
          #Compare teh sequence to the pattern
          is.equal <- all(sequence==pattern)
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
  indices.df$percentile <- floor(rank(indices.df$score,na.last=FALSE) / nrow(indices.df) * 100)

  if(store.data==TRUE)
    store <- data
  else
    store <- data.frame()
  rp <- new("responsePatterns",
            options=list(
              method="patterns",
              max.lag=max.length,
              id.var=ifelse(!is.null(id.var),id.var,""),
              na.rm=na.rm,
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
