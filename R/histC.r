#' Creates histogram to show distribution of interval (numeric) variable's values
#'
#' @description Generates frequency distribution table of binned values and a histogram to describe distribution of variable values.
#' @param x The variable to be analyzed. If dataset not specified with data argument, should be a vector in form dataset$var.
#' @param w (Optional) Sample weights (must be numeric if used), If dataset not specified with data argument, should be in form dataset$weighvar
#' @param data (Optional) Name of dataset that contains x (and w) variable.
#' @param breaks (Optional) Specify how to break the x variable into bins. Options include the number of breaks, a vector specifying the breakpoints, or the name of an algorithm that generates breakpoints. Default value is "Sturges" (other algorithms are "Scott" and "FD", see details in \code{\link[weights]{wtd.hist}} documentation).
#' @param digits (Optional) Number of digits to display after decimal points (default is 2).
#' @param printC (Optional) Do you want the histogram and binned frequencies table printed to working directory? (default: FALSE)
#' @param plot (Optional) Do you want the histogram graphic? (default: TRUE)
#' @param main (Optional) Customize main title for histogram.
#' @param xlab (Optional) Custom label for x-axis of histogram. 
#' @param ylab (Optional) Custom label for y-axis of histogram.
#' @param bar.col (Optional) Color for histogram bars; default is "gray80".
#' @param ... (Optional) Additional arguments passed to `weights::wtd.hist` function.
#' @return A frequency distribution table of binned x variable values.
#' @export
#' @examples 
#'    library(RCPA3)
#'    
#'    histC(x=states$covid.cases.per1000)
#'    
#' @section Textbook References:
#' * Philip H. Pollock and Barry C. Edwards, _An R Companion to Political Analysis, 3rd Edition_ (Thousand Oaks, CA: Sage Publications, Forthcoming 2022), Chapter 2. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Thousand Oaks, CA: Sage Publications, 2020), pp. 39-55. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Descriptive Statistics](https://www.poliscidata.com/pages/rDemosResources.php?chapter=2), Compiled by Barry C. Edwards
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications. 
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom weights wtd.hist
#' @importFrom graphics par box
#' @importFrom knitr kable
#' @importFrom grDevices png dev.off
#' @md


histC <- function(x, w, data, breaks, digits=2, printC=FALSE, plot=TRUE, 
                  main, xlab, ylab, bar.col, ...)
{
  
  if(missing(x)) stop("Oops. You need to identify the variable to be analyzed using this function's x argument. Enter example(wtd.hist) to see an example and help(wtd.hist) to learn more about this function.")
  
  
  if(plot!=FALSE)
  {
    old.par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old.par))
  }
  
  x.name = deparse(substitute(x))
  
  check.value(digits, valuetype="numeric")
  if(!missing(w)) w.name <- deparse(substitute(w))
  
  if(!missing(data))
  {
    if(is.matrix(data)) data <- data.frame(data)
    x <- vector.from.data(substitute(x), data)
    if(!missing(w)) w <- vector.from.data(substitute(w), data)
  }
  
  check.variable(x, vartype="numeric")
  if(!is.null(attr(x, "label"))) x.label <- attr(x, "label")
  
  if(!missing(w)) 
  {
    check.variable(w, vartype="numeric")
    weighted = TRUE
  }
  else 
  {
    w <- rep(1, length(x))
    weighted = FALSE    
  }
  
  
  if(length(unique(x)) <= 10) message(paste("Suggestion:", x.name, "has only", length(unique(x)), "unique values. Maybe you want to use a bar chart?"))
  
  if(missing(breaks)) breaks <- "Sturges"
  
  # here, printing binned freq table before plotting the histogram
  results <- weights::wtd.hist(x=x, weight=w, breaks=breaks, plot=F, ...) 
  
  main.heading <- headingbox("Describing Distribution of Values with Frequency Table and Histogram", width=75, marker="=")
  if(printC==TRUE) printC(main.heading)
  
  from.values   <- c(results$breaks[-length(results$breaks)], results$breaks[1])
    to.values   <- c(results$breaks[-1], results$breaks[length(results$breaks)])
    freq        <- round(c(results$counts, sum(results$counts)), digits)
  percents      <- round(c(results$counts/sum(results$counts)*100, 100), digits)
  cum.percents  <- rep(NA, length(results$breaks))
  for(i in 1:length(results$counts)) cum.percents[i] <- round(sum(percents[1:i]), digits)
  
  brackets <- c("[", rep("(", length(results$counts)-1))
  interval.names <- paste(brackets, from.values, " - ", to.values, "]", sep="")
  interval.names[length(percents)] <- "Total"
  # cum.percents[length(percents)] <- NA
  
  freq.table <- data.frame(interval.names, freq, percents, cum.percents)
  colnames(freq.table) = c("Interval", "Frequency", "Percent", "Cum. Percent")
  if(all(freq.table[, "Frequency"]%%1==0)) n.drop.decimals <- TRUE else n.drop.decimals <- FALSE
  freq.table[, "Frequency"] <- format(freq.table[, "Frequency"], drop0trailing=n.drop.decimals, nsmall=digits, digits=digits)
  
  caption <- paste("Distribution of Binned Values of", x.name)
  if(exists("x.label")) caption <- paste(caption, " (", x.label, ")", sep="")
  if(weighted==TRUE) caption <- paste(caption, ", Weighted by ", w.name, sep="")
  
  print(knitr::kable(format(freq.table, drop0trailing=F, nsmall=digits, digits=digits), format="simple", align="r", caption=caption))
  if(printC==TRUE) printC(knitr::kable(format(freq.table, drop0trailing=F, nsmall=digits, digits=digits), align="r", 
                                       caption=printCaption(caption), format="html"))
  cat("\n")

  if(plot==TRUE)
  {
    # this for loop lets plot be seen by user in R and then go to PNG driver
    for(k in 1:(1+as.numeric(printC)))
    {
      
    restore.par.ask <- graphics::par("ask")
    if(printC==TRUE & k==2) graphics::par(ask=FALSE) else graphics::par(ask=TRUE)
    
    if(printC==TRUE & k==2) 
    {
      imagename <- paste("histC.plot.", unclass(Sys.time()), ".png", sep="")
      grDevices::png(filename=imagename, width=4, height=3, units="in", 
                     type=getPNGtype(), pointsize=8, res=300, antialias="default")
      class(imagename) <- "image"
      printC(imagename)
    }
    if(missing(main)) main <- caption
    main <- strwrap(main, width=50)
    if(missing(xlab)) xlab <- paste(x.name, "values")
    if(missing(ylab)) ylab <- "Frequency"
    if(missing(bar.col)) bar.col <- "gray80"
    
    # calling wtd.hist again this time for the plot
    weights::wtd.hist(x=x, weight=w, breaks=breaks, plot=T, 
                      main=main, xlab=xlab, ylab=ylab, col=bar.col,...) 
    graphics::box()
    graphics::par(ask=restore.par.ask)
    if(printC==TRUE & k==2) grDevices::dev.off()
    }
  }

  if(printC==T) printC(match.call(expand.dots = FALSE))
  invisible(freq.table)
  
}
