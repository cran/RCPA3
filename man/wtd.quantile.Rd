% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.functions.r
\name{wtd.quantile}
\alias{wtd.quantile}
\title{Finds the Quantile Values of Variable With Option for Weights}
\usage{
wtd.quantile(x, w = NULL, data, q = c(5, 10, 25, 50, 75, 90, 95),
  digits = 3, ...)
}
\arguments{
\item{x}{A variable (must be numeric), should be in dataset$var form unless dataset specified in optional data argument.}

\item{w}{(Optional) Sampling weights of variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.}

\item{data}{(Optional) Name of dataset that contains x (and w) variable.}

\item{q}{(Optional) The quantiles you want calculated as a number or vector of numbers. Input as percentages. For median only, q=50. For quartiles, q=c(25, 50, 75).}

\item{digits}{(Optional) Number of decimal places reported in result (defaults to 3).}

\item{...}{(Optional) Additional arguments passed to Hmisc::wtd.quantile function.}
}
\value{
Returns quantile values of a variable, a vector of numeric values.
}
\description{
Takes in variable and calculates desired quantile values, works with sampling weights. Makes use of the wtd.quantile function from the Hmisc package. See \code{\link[Hmisc]{wtd.stats}} documentation for details.
}
\examples{
   library(RCPA3)
   
   wtd.quantile(x=nes$ft.police, q=c(25, 50, 75), w=nes$wt)
   wtd.quantile(x=nes$ft.police, q=c(25, 50, 75))
}
\keyword{internal}
