#' Generates box plots to compare interval-level dependent variable's distribution across categories of independent variable. 
#'
#' @description Generates box plots for visual comparison of interval-level dependent variable's distribution across categories of independent variable. Includes option for weighting observations, modifying colors, variable widths. Box plot can be used to compare values of interval-level dependent variable by categories of an independent variable (a factor).
#' @param dv Dependent variable, should be in dataset$var form unless dataset specified in optional data argument.
#' @param iv Independent variable, should be in dataset$var form unless dataset specified in optional data argument.
#' @param w (Optional) Sampling weights of variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains dv, iv (and w) variables (optional).
#' @param main (Optional) Supply custom main label for plot; default uses names of dv and iv.
#' @param xlab (Optional) Supply custom x-axis label for plot; default uses name of iv.
#' @param ylab (Optional) Supply y-axis label for plot; default uses name of dv.
#' @param box.col (Optional) The name of color to use for box colors. Default is "gray80". 
#' @param varwidth (Optional) Do you want the widths of boxes to be proportional to number of observations in each group? Default is TRUE; set `varwidth=FALSE` for equal-width boxes.
#' @param ivlabs (Optional) A vector of labels for the iv values that are box lablels. 
#' @param printC (Optional) Do you want to print box plot to .html file in working directory? (Default: FALSE)
#' @param ... Additional arguments passed to plotting functions, \code{\link[graphics]{boxplot}} or \code{\link[graphics]{bxp}}.
#' @return No return, creates a plot.
#' @examples 
#'   library(RCPA3)
#'    
#'   # basic usage with variables as vectors
#'   boxplotC(dv=nes$ft.rep, iv=nes$partyid3)
#'   
#'   # with w and data arguments
#'   boxplotC(dv=ft.rep, iv=partyid3, w=wt, data=nes)
#' @export
#' @section Textbook References:
#' * Philip H. Pollock and Barry C. Edwards, _An R Companion to Political Analysis, 3rd Edition_ (Thousand Oaks, CA: Sage Publications, Forthcoming 2022), Chapter 5. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Thousand Oaks, CA: Sage Publications, 2020), pp. 53-55. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Graphing Relationships and Describing Patterns](https://www.poliscidata.com/pages/rDemosResources.php?chapter=5), compiled by Barry C. Edwards
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications. 
#' * [Political Science Data Web Site](https://www.poliscidata.com), find datasets for your own research and resources to help with political analysis. 
#' @importFrom descr compmeans
#' @importFrom graphics par mtext
#' @importFrom grDevices png dev.off
#' @importFrom stats na.omit
#' @md


boxplotC <- function(dv, iv, w, data, main, xlab, ylab, box.col, 
                     varwidth=TRUE, ivlabs, printC=FALSE, ...) 
{ 
  
  if(missing(dv) && missing(iv)) stop("There's a problem: You need to specify dependent and independent variables (dv and iv). You can enter help(boxplotC) to learn how this function works. To see an example, enter example(boxplotC).")
  
  old.par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old.par))
  
  dv.name <- deparse(substitute(dv))
  iv.name <- deparse(substitute(iv))
  
  if(!missing(w)) w.name <- deparse(substitute(w))


  if(!missing(data))
  {
    if(is.matrix(data)) data <- data.frame(data)
    dv <- vector.from.data(substitute(dv), data)
    iv <- vector.from.data(substitute(iv), data)
    if(!missing(w))  w <- vector.from.data(substitute(w), data)
  }
  
  check.variable(dv, vartype="numeric")
  check.variable(iv, limitedvalues=12)
  if(!is.null(attr(dv, "label"))) dv.label <- attr(dv, "label")
  if(!is.null(attr(iv, "label"))) iv.label <- attr(iv, "label")
  
  if(missing(w))
  {
    w <- rep(1, length(dv))
    weighted = FALSE    
  }
  else
  {
    weighted = TRUE
    if (length(dv) != length(w)) stop(paste(dv.name, gettext("and", domain = "R-descr"), w.name, gettext("have different lengths.", domain = "R-descr")))
  }
  # check that x and w are same length
  check.variable(w, vartype="numeric")
  
  
  main.heading <- headingbox("Making Comparisons with Box Plots", width=80, marker="=")
  if(printC==TRUE) printC(main.heading)
  
  # if printC=T this runs plotting commands twice so plot seen in R and then goes to PNG driver
  for(k in 1:(1+as.numeric(printC)))
  {
    
  if(printC==TRUE & k==2) 
  {
    imagename <- paste("boxplotsC.plot.", unclass(Sys.time()), ".png", sep="")
    grDevices::png(filename=imagename, width=4, height=3, units="in", 
                   type=getPNGtype(), pointsize=8, res=300, antialias="default")
    class(imagename) <- "imageonly"
    printC(imagename)
  }
  
  if(!missing(main)) plotmain <- main
  
  if(missing(main)) plotmain <- paste("Boxplot of", dv.name, "by", iv.name)
  
  if(missing(main) && weighted==TRUE) plotmain <- paste(plotmain, ", weighted by ", w.name, sep="")
  
  if(missing(ylab)) ylab <- dv.name
  if(missing(xlab)) xlab <- iv.name
  if(missing(box.col)) box.col <- "gray80"

  plotmain <- strwrap(plotmain, width=50)
  restore.par.ask <- graphics::par("ask")
  if(printC==TRUE & k==2) graphics::par(ask=FALSE) else graphics::par(ask=TRUE)
  restore.par.mar <- graphics::par("mar")
  
  if(is.null(levels(iv))) levels(iv) <- unique(stats::na.omit(iv))
  
  if(!missing(ivlabs)) 
  {
    if(length(ivlabs) != length(levels(iv))) stop("Oops. The iv labels aren't the right length for this iv's values. There are ", length(ivlabs), " labels for ", length(levels(iv)), " different values. To see how to use the ivlabs argument, try help(boxplotC).")
    levels(iv) <- ivlabs
  }
  
  maxaxislabelsize <- max(nchar(as.character(levels(iv))))
  nticklabels <- length(levels(iv))
  
  if((maxaxislabelsize*nticklabels) > 50) # will need extra room
  {
    graphics::par(mar=c(4.1 + sqrt(maxaxislabelsize), 4.1, 4.1, 1.1))
    cex.ticklabels <- 0.75
    las.ticklabels <- 2 # 2
    mtext.lines <- (4.1 + sqrt(maxaxislabelsize)) - 1.5
    if(k<2) message(paste("One or more text labels for boxes is long. Consider using ivlabs arguments to shorten them."))
  }
  else # labels should fit w/o rotation
  {
    graphics::par(mar=c(4.1, 4.1, 3.6, 1.1))
    cex.ticklabels <- 0.9
    las.ticklabels <- 1 # 2
    mtext.lines <- 2.5
  }
  
  # las argument will turn box x-axis labels vertically, and y-axis horiz
  # cex.axis will change both axis labels size.
  
  sinktable <- descr::compmeans(x=dv, f=iv, w=w, plot=TRUE, warn=FALSE, main=plotmain, 
                                xlab="", ylab=ylab, col=box.col, cex.axis=cex.ticklabels,
                                las=las.ticklabels, relative.widths=varwidth, ...) 
  # not printing the table from compmeans, just the box plot
  graphics::mtext(text = xlab, side = 1, line = mtext.lines)
  graphics::par(mar=restore.par.mar, ask=restore.par.ask)
  if(printC==TRUE & k==2) grDevices::dev.off()
  }
  
  if(printC==T) printC(match.call(expand.dots = FALSE))
  
}



