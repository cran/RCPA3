% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CImean.r
\name{CImean}
\alias{CImean}
\title{Confidence interval of a dataset variable's sample mean in table and figure}
\usage{
CImean(x, w, data, digits = 3, level = 95, pop.sd, printC = FALSE,
  plot = TRUE, main, xlab, xlim, ...)
}
\arguments{
\item{x}{A numeric variable, should be in dataset$var form unless dataset specified in optional data argument.}

\item{w}{(Optional) Sampling weights of variable (optional), must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.}

\item{data}{(Optional) Name of dataset that contains x (and w) variable (optional).}

\item{digits}{(Optional) Number of decimal places reported in result (defaults to 3).}

\item{level}{(Optional) A single number equal to the desired confidence level (i.e. 95, 99, 90, etc.). Default value is 95 percent confidence level.}

\item{pop.sd}{(Optional) A single number equal to the known population standard deviation of x. This value is rarely know, but if it is, critical values for confidence interval are based on standard normal distribution.}

\item{printC}{(Optional) Do you want results printed to .html file in your working directory? Default is FALSE. Set to TRUE to print results.}

\item{plot}{(Optional) Do you want a plot of the confidence interval? Default is TRUE.}

\item{main}{(Optional) Change the main title of plot. Default title generated from level, x, and w.}

\item{xlab}{(Optional) Label for x-axis of confidence interval plot.}

\item{xlim}{(Optional) Modify x-axis limits of confidence interval plot.}

\item{...}{(Optional) additional arguments passed to \code{plot} function.}
}
\value{
Returns the confidence interval as a vector of numeric values (the lower and upper bounds).
}
\description{
Reports the confidence interval of a sample mean in table and plot. Default is 95\% CI but use can raise or lower confidence level.
}
\section{RCPA3 Package Tutorial Videos}{

\itemize{
\item \href{https://www.youtube.com/watch?v=EmU7cQJ_uRM}{Confidence Intervals for Sample Means with RCPA3's CImean Function} 16:16
\item \href{https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu}{Complete Playlist of RCPA3 Package Tutorial Videos}, includes video for this function and many more.
}
}

\section{Textbook References}{

\itemize{
\item Philip H. Pollock and Barry C. Edwards, \emph{An R Companion to Political Analysis, 3rd Edition} (Thousand Oaks, CA: Sage Publications, Forthcoming 2022), Chapter 8.
\item Philip H. Pollock and Barry C. Edwards, \emph{The Essentials of Political Analysis, 6th Edition} (Thousand Oaks, CA: Sage Publications, 2020), pp. 184-186. ISBN-13: 978-1506379616; ISBN-10: 150637961.
}
}

\section{Online Resources}{

\itemize{
\item \href{https://www.poliscidata.com/pages/rDemosResources.php?chapter=8}{R Tutorials & Resources for Foundations of Inference}, Compiled by Barry C. Edwards
\item \href{https://edge.sagepub.com/pollock}{Sage Edge Resources for Political Analysis Series}, for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications.
\item \href{https://www.poliscidata.com}{Political Science Data Web Site}: Find datasets for your own research and resources to help with the analysis.
}
}

\examples{
 
  library(RCPA3)
  
  CImean(nes$age)
  
  \donttest{
  # using optional w, level, and data arguments
  CImean(x=nes$age, w=nes$wt, level=90)
  CImean(x=age, data=nes, level=95)
  }
}
