% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.functions.r
\name{wtd.kurtosis}
\alias{wtd.kurtosis}
\title{Generates Kurtosis Statistic with Option to Weight Observations}
\usage{
wtd.kurtosis(x, w, data, digits = 3)
}
\arguments{
\item{x}{Variable (a numeric vector)}

\item{w}{(Optional) Weights variable}

\item{data}{(Optional) Dataset, can be used to abbreviate x to variable name}

\item{digits}{(Optional) Number of digits after decimal point (default: 3)}
}
\value{
Returns the kurtosis of the variable, a numeric value.
}
\description{
Generates Kurtosis Statistic with Option to Weight Observations
}
\examples{
   library(RCPA3)
   
   wtd.kurtosis(x=nes$age, w=nes$wt)
   wtd.kurtosis(x=nes$age)
}
\keyword{internal}
