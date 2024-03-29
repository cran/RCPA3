% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/freqC.r
\name{freqC}
\alias{freqC}
\title{Reports the frequency distribution of dataset variable with table and bar chart}
\usage{
freqC(x, w, data, digits = 2, rowlabs, printC = FALSE, plot = TRUE, main,
  xlab, ylab, bar.col, ...)
}
\arguments{
\item{x}{The variable to be analyzed. If dataset not specified with data argument, should be a vector in form dataset$var.}

\item{w}{(Optional) Sample weights (must be numeric if used), If dataset not specified with data argument, should be in form dataset$weighvar}

\item{data}{(Optional) Name of dataset that contains x (and w) variable.}

\item{digits}{(Optional) Number of digits to display after decimal point (default=2).}

\item{rowlabs}{(Optional) Vector specifying custom text for labeling table rows and chart bars. The rowlabs vector must correspond to x variable's levels (if the variable has five levels, rowlabs must be length 5). Useful when default value labels are too long.}

\item{printC}{(Optional) Do you want to print frequency distribution table and bar chart (if plot is used) to .html file in working directory? (default: FALSE)}

\item{plot}{(Optional) Do you want a bar chart? (default set to TRUE)}

\item{main}{(Optional) Main title of bar chart.}

\item{xlab}{(Optional) The x-axis label of bar chart.}

\item{ylab}{(Optional) The y-axis label of bar chart.}

\item{bar.col}{(Optional) The name of color to use for bars. Default is "gray80".}

\item{...}{(Optional) Additional arguments passed to \code{descr::freq} function.}
}
\value{
A frequency distribution table.
}
\description{
Generates frequency distribution table and bar chart to describe distribution of variable values. Based on \code{\link[descr]{freq}} function in descr package.
}
\section{RCPA3 Package Tutorial Videos}{

\itemize{
\item \href{https://www.youtube.com/watch?v=NwFB_JLgAko}{Make Frequency Distribution Tables and Bar Charts with RCPA3 Package's freqC Function} 15:33
\item \href{https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu}{Complete Playlist of RCPA3 Package Tutorial Videos}, includes video for this function and many more.
}
}

\section{Textbook References}{

\itemize{
\item Philip H. Pollock and Barry C. Edwards, \emph{An R Companion to Political Analysis, 3rd Edition} (Thousand Oaks, CA: Sage Publications, Forthcoming 2022), Chapter 2.
\item Philip H. Pollock and Barry C. Edwards, \emph{The Essentials of Political Analysis, 6th Edition} (Thousand Oaks, CA: Sage Publications, 2020), pp. 39-55. ISBN-13: 978-1506379616; ISBN-10: 150637961.
}
}

\section{Online Resources}{

\itemize{
\item \href{https://www.poliscidata.com/pages/rDemosResources.php?chapter=2}{R Tutorials & Resources for Descriptive Statistics}, compiled by Barry C. Edwards
\item \href{https://edge.sagepub.com/pollock}{Sage Edge Resources for Political Analysis Series}, for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications.
\item \href{https://www.poliscidata.com}{Political Science Data Web Site}: Find datasets for your own research and resources to help with the analysis.
}
}

\examples{
   library(RCPA3)
   
   # unordered factors
   freqC(x=region, data=world)
   
   # ordered factors
   freqC(x=threat.from.china, data=nes)
}
