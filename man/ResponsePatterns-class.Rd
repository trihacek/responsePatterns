% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes.R
\docType{class}
\name{ResponsePatterns-class}
\alias{ResponsePatterns-class}
\alias{ResponsePatterns}
\title{An S4 class to represent the results of response patterns analysis.}
\description{
An S4 class to represent the results of response patterns analysis.
}
\section{Slots}{

\describe{
\item{\code{id}}{A vector. Contains the ID variable (if declared by the user) or NAs (if not).}

\item{\code{n.obs}}{An integer. Number of observations (responses) in the data set.}

\item{\code{n.vars}}{An integer. Number of variables (excluding the ID variable, if declared).}

\item{\code{options}}{A list. Contains diverse options set by the user.}

\item{\code{percentile}}{An integer. If the rp.select() function is used to select a subsample, this keeps the information about the chosen percentile. Defaults to zero.}

\item{\code{data}}{A data frame. Stores the data.}

\item{\code{coefficients}}{A data frame. Stores the intermediate products of the analysis.}

\item{\code{indices}}{A data frame. Stores the final products of the analysis.}
}}

