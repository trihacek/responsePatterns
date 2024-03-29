#' responsePatterns: A package for finding instances of careless responding
#'
#' Some survey participants tend to respond carelessly, which complicates data analysis. This package provides functions that make it easier to find repeated patterns in data and identify responses that may be problematic. This package implements two approaches to the problem of careless responses detection: one based on the auto-correlation approach and one based on a mechanistic approach. Both approaches yield scores that serve as estimates of how problematic the observations potentially are ("suspicion" scores). However, no conclusions should be made without a closer inspection of the problematic responses. Any decision about removing or downweighing an observation should be based on visual inspection of the responses, the specifics of the instrument used to collect the data, researchers' familiarity with the whole data set and the context of the data collection process.
#' @section Auto-correlation approach:
#' The \code{\link{rp.acors}} function allows for a probabilistic detection of repetitive patterns in data. This function calculates auto-correlation coefficients for all lags up to a value defined by the max.lag parameter for each observation (respondent). Subsequently, it assigns a percentile value to each observation (respondent) based either on the highest absolute auto-correlation or the sum of absolute auto-correlations.
#' @section Mechanistic approach:
#'The \code{\link{rp.patterns}} function searches for repetitive patterns in the data using an iterative algorithm. Patterns are defined based on the data themselves: if a sequence of values occurs more than once within an observation, it is considered a repetition. The algorithm counts the number of repetitions for different lengths of patterns and then weighs this sum by the length of the pattern (longer patterns are assigned higher weight). The total score for each respondent is determined as the sum of scores achieved for each pattern length and is standardized to a value between 0 and 1.
#' @section Auxiliary functions:
#' The package provides auxiliary functions to summarize the responsePatterns object (\code{\link{rp.summary}}), extract indices (\code{\link{rp.indices}}, \code{\link{rp.hist}}, \code{\link{rp.save2csv}}) and to visually inspect individual responses (\code{\link{rp.plot}}, \code{\link{rp.plots2pdf}}).
#' @references Gottfried, J., Jezek, S., & Kralova, M. (2022). Autocorrelation screening: A potentially efficient method for detecting repetitive response patterns in questionnaire data. *Practical Assessment, Research, and Evaluation, 27*, Article 2. https://doi.org/10.7275/vyxb-gt24
#'
#' @docType package
#' @name responsePatterns
NULL
#> NULL
