rp.plots2pdf <- function(rp.object,
                         file="rp_plots.pdf",
                         groups=NULL,
                         page.breaks=NULL,
                         bw=FALSE
) {

  #Check rp.object
  if(!is(rp.object,"responsePatterns"))
    stop("The object is not of class responsePatterns")

  pdf(file=file)
  for(i in 1:rp.object@n.obs)
    rp.plot(rp.object, obs=i, plot=TRUE, text.output=FALSE, groups=groups, page.breaks=page.breaks, bw=bw)
  dev.off()

  message("Plots saved to ",file)

}
