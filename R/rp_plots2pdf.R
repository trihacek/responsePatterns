#' Export multiple plots to PDF
#'
#' This function exports individual plots of all observations to a PDF file. Limit the number of observation by rp.select.
#'
#' @param rp.object A ResponsePatterns object.
#' @param file A string. A filename of the PDF file.
#' @param groups A list of vectors. Defines groups of items that should be plotted using the same color.
#' @param page.breaks A vector. Draws a vertical line after the items (useful if you want to display the pagination of the questionnaire in the plot).
#' @param bw A logical scalar. Should the plot be printed in black and white?
#'
#' @return Creates a PDF file.
#' @export
#'
#' @seealso \code{\link{rp.acors}}, \code{\link{rp.patterns}}, \code{\link{rp.plot}}
#'
#' @examples
#' rp <- rp.acors(rp.simdata, id.var="optional_ID")
#' rp.plots2pdf(rp)
rp.plots2pdf <- function(rp.object,
                         file="rp_plots.pdf",
                         groups=NULL,
                         page.breaks=NULL,
                         bw=FALSE
) {

  #Check rp.object
  if(!methods::is(rp.object,"ResponsePatterns"))
    stop("The object is not of class ResponsePatterns")

  grDevices::pdf(file=file)
  for(i in 1:rp.object@n.obs)
    rp.plot(rp.object, obs=i, plot=TRUE, text.output=FALSE, groups=groups, page.breaks=page.breaks, bw=bw)
  grDevices::dev.off()

  message("Plots saved to ",file)

}
