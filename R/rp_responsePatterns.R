responsePatterns <- setClass("responsePatterns", slots=c(
  id="vector",
  n.obs="numeric",
  n.vars="numeric",
  options="list",
  percentile="numeric",
  data="data.frame",
  coefficients="data.frame",
  indices="data.frame"
), prototype=c(
  percentile=0
))

#d <- read.csv2("_archive/data.csv")
#rpa <- rp.acors(d,id.var="id",percentile.method="sum")
#rpp <- rp.patterns(d,id.var="id")
