#' An S4 class to represent the results of response patterns analysis.
#'
#' @slot id A vector. Contains the ID variable (if declared by the user) or NAs (if not).
#' @slot n.obs An integer. Number of observations (responses) in the data set.
#' @slot n.vars An integer. Number of variables (excluding the ID variable, if declared).
#' @slot options A list. Contains diverse options set by the user.
#' @slot percentile An integer. If the rp.select() function is used to select a subsample, this keeps the information about the chosen percentile. Defaults to zero.
#' @slot data A data frame. Stores the data.
#' @slot coefficients A data frame. Stores the intermediate products of the analysis.
#' @slot indices A data frame. Stores the final products of the analysis.
ResponsePatterns <- setClass("ResponsePatterns", slots=c(
  id="vector",
  n.obs="numeric",
  n.vars="numeric",
  options="list",
  percentile="numeric",
  data="data.frame",
  coefficients="data.frame",
  indices="data.frame"
), prototype=c(
  id="",
  n.obs=0,
  n.vars=0,
  options=list(),
  percentile=0,
  data=data.frame(),
  coefficients=data.frame(),
  indices=data.frame()
))

setMethod("summary",signature(object="ResponsePatterns"),function(object) {rp.summary(object)})

# Copied from Psychonetrics:
.onAttach <- function(libname, pkgname) {
  version <- read.dcf(file=system.file("DESCRIPTION", package=pkgname),
                      fields="Version")
  packageStartupMessage("This is ",paste(pkgname, version),". Note: this is BETA software! Please mind that the package may not be stable and report any bugs! For questions and issues, please see github.com/trihacek/responsePatterns.")
}

#d <- read.csv2("_archive/data.csv")
#rpa <- rp.acors(d,id.var="id",percentile.method="sum", min.lag=2)
#rpp <- rp.patterns(d,id.var="id")
#x <- lm(d[,2]~d[,3])
