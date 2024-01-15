#' Reports the frequency distribution of dataset variable with table and bar chart
#'
#' @description Generates frequency distribution table and bar chart to describe distribution of variable values. Based on \code{\link[descr]{freq}} function in descr package.
#' @param x The variable to be analyzed. If dataset not specified with data argument, should be a vector in form dataset$var.
#' @param w (Optional) Sample weights (must be numeric if used), If dataset not specified with data argument, should be in form dataset$weighvar
#' @param data (Optional) Name of dataset that contains x (and w) variable.
#' @param digits (Optional) Number of digits to display after decimal point (default=2).
#' @param rowlabs (Optional) Vector specifying custom text for labeling table rows and chart bars. The rowlabs vector must correspond to x variable's levels (if the variable has five levels, rowlabs must be length 5). Useful when default value labels are too long. 
#' @param printC (Optional) Do you want to print frequency distribution table and bar chart (if plot is used) to .html file in working directory? (default: FALSE)
#' @param plot (Optional) Do you want a bar chart? (default set to TRUE)
#' @param main (Optional) Main title of bar chart.
#' @param xlab (Optional) The x-axis label of bar chart.
#' @param ylab (Optional) The y-axis label of bar chart.
#' @param bar.col (Optional) The name of color to use for bars. Default is "gray80". 
#' @param ... (Optional) Additional arguments passed to `descr::freq` function.
#' @return A frequency distribution table.
#' @export
#' @examples 
#'    library(RCPA3)
#'    
#'    # unordered factors
#'    freqC(x=region, data=world)
#'    
#'    # ordered factors
#'    freqC(x=threat.from.china, data=nes)
#' @section RCPA3 Package Tutorial Videos:
#' * [Make Frequency Distribution Tables and Bar Charts with RCPA3 Package's freqC Function](https://www.youtube.com/watch?v=NwFB_JLgAko) 15:33  
#' * [Complete Playlist of RCPA3 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook References:
#' * Philip H. Pollock and Barry C. Edwards, _An R Companion to Political Analysis, 3rd Edition_ (Thousand Oaks, CA: Sage Publications, Forthcoming 2022), Chapter 2. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Thousand Oaks, CA: Sage Publications, 2020), pp. 39-55. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Descriptive Statistics](https://www.poliscidata.com/pages/rDemosResources.php?chapter=2), compiled by Barry C. Edwards
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications. 
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom descr freq
#' @importFrom graphics par
#' @importFrom stats na.omit
#' @importFrom grDevices png dev.off
#' @md


freqC <- function(x, w, data, digits=2, rowlabs, printC=FALSE, plot=TRUE, 
                  main, xlab, ylab, bar.col, ...)
{
  if(missing(x))  stop("Oops. You need to specify the variable to analyze using the x argument. To see how to use this function, try example(freqC) or help(freqC).")
  
  if(plot!=FALSE)
  {
    old.par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old.par))
  }
  
  x.name = deparse(substitute(x))
  # return(x.name)
  if(!missing(w)) w.name = deparse(substitute(w))
  check.value(digits, valuetype="numeric")


  if(!missing(data))
  {
    if(is.matrix(data)) data <- data.frame(data)
    if(!missing(x)) x <- vector.from.data(substitute(x), data)
    if(!missing(w))  w  <- vector.from.data(substitute(w), data)
  }
  
  check.variable(x)
  if(!is.null(attr(x, "label"))) x.label <- attr(x, "label")
  
  if(!missing(w)) 
  {
    check.variable(w, vartype="numeric")  
    weighted=TRUE    
  }
  if(missing(w)) 
  {
    w <- rep(1, length(x))  
    weighted=FALSE   
  }
  
  if(!missing(rowlabs))
  {
    if(!is.null(levels(x))) x.values <- levels(x) else x.values <- unique(stats::na.omit(x))
    if(length(rowlabs) != length(x.values)) stop("Oops. The x variable's labels aren't the right length for this x variable's values. There are ", length(rowlabs), " labels for ", length(x.values), " different values. To see how to use the rowlabs argument, try help(freqC).")
  }
  
  # this will lose variable's label attributes?
  k <- grep(FALSE, (is.na(x) | is.na(w))) # checks for missing values
  x <- x[k]
  w <- w[k]

  if(plot==TRUE) main.heading <- headingbox("Describing Distribution of Values with Frequency Table and Bar Chart", width=75, marker="=")
  if(plot==FALSE) main.heading <- headingbox("Describing Distribution of Values with Frequency Table", width=75, marker="=")
  if(printC==TRUE) printC(main.heading)
  
  caption <- paste("Frequency Distribution of", x.name)
  if(exists("x.label")) caption <- paste(caption, " (", x.label, ")", sep="")
  if(weighted==TRUE) caption <- paste(caption, ", Weighted by ", w.name, sep="")
  
  if(("ordered" %in% class(x)) & ("factor" %in% class(x))) class(x) <- c("ordered", "factor")
  
  obj1 <- descr::freq(x, w, y.axis="percent", cex.names=0.75, las=2, plot=FALSE, ...)
  obj1 <- round(data.frame(obj1), digits)
  
  if(!missing(rowlabs)) row.names(obj1) <- c(rowlabs, "Total")
  else rowlabs <- row.names(obj1)[-nrow(obj1)]
  
  if(all(obj1[, "Frequency"]%%1==0)) n.drop.decimals <- TRUE else n.drop.decimals <- FALSE
  obj1[, "Frequency"] <- format(obj1[, "Frequency"], drop0trailing=n.drop.decimals, nsmall=digits, digits=digits)
  # obj1[, "Percent"] <- format(obj1[, "Percent"], drop0trailing=F, nsmall=digits, digits=digits)
  
  print(knitr::kable(format(obj1, drop0trailing=F, nsmall=digits, digits=digits), caption=caption, format="simple", align="r"))
  if(printC==TRUE) printC(knitr::kable(format(obj1, drop0trailing=F, nsmall=digits, digits=digits), 
                                       caption=printCaption(caption), format="html", align="r"))
  cat("\n")  
  
  if(length(unique(x)) > 15)
  {
    tryhistmessage <- paste("Suggestion:", x.name, "has a lot of unique values. Try making a histogram with histC function for clearer description of its distribution.")
    # x <- as.ordered(x)
    # median <- wtd.median(x, w=w)
    message(tryhistmessage)
  }
  
  if(plot==TRUE)
  {
    # this for loop lets plot be seen by user in R and then go to PNG driver
    for(k in 1:(1+as.numeric(printC)))
    {
      
    if(printC==TRUE & k==2) 
    {
      imagename <- paste("freqC.plot.", unclass(Sys.time()), ".png", sep="")
      grDevices::png(filename=imagename, width=4, height=3, units="in", 
                     type=getPNGtype(), pointsize=8, res=300, antialias="default")
      class(imagename) <- "image"
      printC(imagename)
    }
    # may need additional margin below for labels
    # what's the balance between the length of tick labels and the number of them?
    # figure out when they need to be turned and how much room they need
    # need more margin under plot with xlab
    maxaxislabelsize <- max(nchar(dimnames(obj1)[[1]]))
    nticklabels <- length(dimnames(obj1)[[1]]) - 1
    
    restoreask <- graphics::par("ask")
    if(printC==TRUE & k==2) graphics::par(ask=FALSE) else graphics::par(ask=TRUE)
    
    restoremar <- graphics::par("mar")
    if((maxaxislabelsize*nticklabels) > 50) # will need extra room
    {
      graphics::par(mar=c(4.1 + sqrt(maxaxislabelsize + 2), 4.1, 4.1, 1.1))
      cex.ticklabels <- 0.75
      las.ticklabels <- 2 # 2
      mtext.lines <- (4.1 + sqrt(maxaxislabelsize)) - 1.5
      if(k<2) message(paste("One or more text labels for bars is long. Consider using rowlabs argument to abbreviate."))
    }
    else # labels should fit w/o rotation
    {
      graphics::par(mar=c(4.1, 4.1, 3.6, 1.1))
      cex.ticklabels <- 0.9
      las.ticklabels <- 1 # 2
      mtext.lines <- 2.5
    }
    
    if(missing(main)) main <- caption
    if(missing(ylab)) ylab <- "Percentage"
    if(missing(xlab)) xlab <- paste(x.name, "values")
    if(missing(bar.col)) bar.col <- "gray80"
    main <- strwrap(main, width=50)
    
    obj2 <- descr::freq(x, w, y.axis="percent", cex.names=cex.ticklabels, las=las.ticklabels, plot=plot, 
                        ylab=ylab, main=main, col=bar.col, names.arg=rowlabs)
    graphics::mtext(text = xlab, side = 1, line = mtext.lines)
    # obj2 not printed, just for generating plot if plot=TRUE
    graphics::par(ask=restoreask, mar=restoremar)
    if(printC==TRUE & k==2) grDevices::dev.off()
    }
  }
  
  if(printC==T) printC(match.call(expand.dots = FALSE))
  invisible(obj1)
  
}
