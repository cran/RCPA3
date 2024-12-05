#' Confidence intervals of a dataset variable's sample proportions in table and figure
#'
#' @description Reports the confidence interval of sample proportions in table and plot. Default is 95% CI but use can raise or lower confidence level. 
#' @param x A nominal or ordinal variable (factor), should be in dataset$var form unless dataset specified in optional data argument.
#' @param w (Optional) Sampling weights of variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains x (and w) variable.
#' @param digits (Optional) Number of decimal places reported in result (defaults to 3).
#' @param level (Optional) A single number equal to the desired confidence level (i.e. 95, 99, 90, etc.). Default value is 95 percent confidence level.
#' @param printC (Optional) Do you want results printed to .html file in your working directory? Default is FALSE. Set to TRUE to print results.
#' @param plot (Optional) Do you want a plot of the confidence interval? Default is TRUE.
#' @param main (Optional) Change the main title of plot. Default title generated from level, x, and w.
#' @param xlab (Optional) Label for x-axis of confidence interval plot.
#' @param xlim (Optional) Modify x-axis limits of confidence interval plot.
#' @param ... (Optional) Additional arguments passed to `plot` function.
#' @return Returns a data frame that gives the lower bound, point estimate, and upper bounds of each value of x variable.
#' @examples  
#'   library(RCPA3)
#' 
#'   CIprop(nes$lifex.knowimmig)
#'   
#'   \dontrun{
#'   # using optional w and data arguments
#'   CIprop(x=nes$lifex.knowimmig, w=nes$wt)
#'   CIprop(x=lifex.knowimmig, w=wt, data=nes)
#'   }
#' @export
#' @section RCPA3 Package Tutorial Videos:
#' * [Confidence Intervals for Sample Proportions with RCPA3's CIprop Function](https://www.youtube.com/watch?v=uRukzm66BdI) 18:29 
#' * [Complete Playlist of RCPA3 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook References:
#' * Philip H. Pollock and Barry C. Edwards, _An R Companion to Political Analysis, 3rd Edition_ (Thousand Oaks, CA: Sage Publications, Forthcoming 2022), Chapter 8. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Thousand Oaks, CA: Sage Publications, 2020), pp. 184-186. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Foundations of Inference](https://www.poliscidata.com/pages/rDemosResources.php?chapter=8), Compiled by Barry C. Edwards
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications.  
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom knitr kable
#' @importFrom graphics par points segments axis box
#' @importFrom grDevices png dev.off
#' @md



CIprop = function(x, w, data, digits=3, level=95, printC=FALSE, plot=TRUE, 
                  main, xlab, xlim, ...) 
  {
  if(missing(x)) stop("Oops. You need to specify the variable to analyze with the x argument. To see how to use this function, try example(ci.prop) or enter help(ci.prop)")
  
  if(plot!=FALSE)
  {
    old.par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old.par))
  }
  
  x.name = deparse(substitute(x))
  if(!missing(w)) w.name = deparse(substitute(w))
  
  if(!missing(data))
  {
    if(is.matrix(data)) data <- data.frame(data)
    x <- vector.from.data(substitute(x), data)
    if(!missing(w)) w <- vector.from.data(substitute(w), data)
  }
  
  if(!is.null(attr(x, "label"))) x.label <- attr(x, "label")
  check.variable(x, limitedvalues = 12)
  
  if(!missing(w)) check.variable(w, vartype = "numeric")

  check.value(digits, valuetype="numeric")
  check.value(level, valuetype="numeric")
  # check if levels in reasonable range
  # could convert proportions to percent instead of stopping
  if((level > 0) & (level < 1)) 
  {
    level <- level*100
    message("Please note: It looks like you entered confidence level as a proportion. Confidence level should be percentage, like 95 or 99. Attempting to convert...")
  }
  if((level <= 50) & (level >= 1)) stop(paste(gettext("There's a problem: confidence level should be percentage, like 95 or 99, not Type I error rate.", domain = "R-descr")))
  
  
  x.length <- length(x)
  
  if(missing(w)) 
  {
    w <- rep(1, x.length)
    weighted=FALSE
  }
  else
  {
    weighted = TRUE
    if (x.length != length(w)) stop(paste(x.name, gettext("and", domain = "R-descr"), w.name, gettext("have different lengths.", domain = "R-descr")))
  }
  # check that x and w are same length
  
  k <- grep(FALSE, (is.na(x) | is.na(w))) # checks for missing values
  x <- x[k]
  w <- w[k]
  
  if(!is.null(levels(x))) x.values <- trimws(levels(x), which="both")
  else x.values <- trimws(sort(unique(x)), which="both")
  
  n.x.values <- length(x.values)
  x.n  <- sum(w) # the n of sample, needs to adjust in case of weights
  props <- rep(NA, n.x.values)
  se.prop <- rep(NA, n.x.values)
  out1 <- NULL
  
  for(i in 1:n.x.values)
  {
    thisvalue = x.values[i]
    j <- (x==thisvalue)
    n.thisvalue <- sum(w[j])
    props[i] <- n.thisvalue / x.n
    se.prop[i] <- sqrt(props[i]*(1-props[i])/x.n)
  }

   half_alpha = (1 - level/100) / 2
   critical_value <- stats::qnorm(1-half_alpha)

   lowers = props - critical_value*se.prop
   uppers = props + critical_value*se.prop
   
   main.heading <- headingbox("Confidence Intervals of Proportions", width=75, marker="=")
   if(printC==TRUE) printC(main.heading)
   
   title <- paste(level, "% CI of values of ", x.name, sep="")
   if(exists("x.label")) title <- paste(title, " (", x.label, ")", sep="")
   if(weighted==TRUE) title <- paste(title, ", weighted by ", w.name, sep="")

   ci.tab = round(data.frame(lowers, props, uppers), digits)
   colnames(ci.tab) = c("Lower Bound", "Point Estimate","Upper Bound")
   rownames(ci.tab) <- x.values
   print(knitr::kable(format(ci.tab, drop0trailing=F, nsmall=digits, digits=digits), format="simple", align="r", caption=title))
   if(printC==TRUE) printC(knitr::kable(format(ci.tab, drop0trailing=F, nsmall=digits, digits=digits), 
                                  format="html", align="r", caption=printCaption(title)))
   cat("\n")
   
   if(plot==TRUE)
   {
     for(k in 1:(1+as.numeric(printC)))
     {
       
     if(printC==TRUE & k==2) 
     {
       imagename <- paste("CIprop.plot.", unclass(Sys.time()), ".png", sep="")
       grDevices::png(filename=imagename, width=3.6, height=1.2+length(x.values)*.3, units="in", 
                      type=getPNGtype(), pointsize=8, res=300, antialias="default")
       class(imagename) <- "image"
       printC(imagename)
     }
     
     if(missing(xlab)) xlab <- "Estimates of Population Proportions"
     if(missing(main)) main <- title
     main <- strwrap(main, width=50)
     
     if(missing(xlim)) xlim <- c(0,1)
     restore.par.ask <- graphics::par("ask")
     if(printC==TRUE & k==2) graphics::par(ask=FALSE) else graphics::par(ask=TRUE)
     restore.par.mar <- graphics::par("mar") 
     # how much left margin buffer needed?
     graphics::par(mar=c(4.6, 4.1 + sqrt(max(nchar(x.values))), 4.1, 3.1))
     plot(x="",y="",xlim=xlim, ylim=c(.5,n.x.values+.5), 
          axes=F, xlab=xlab, ylab="", main=main, ...)
     # abline(v=0, col="gray80")
     graphics::points(x=props, y=1:n.x.values, pch=16, cex=1)
     graphics::segments(x0 = lowers, x1 = uppers, y0=1:n.x.values, y1=1:n.x.values, lty=3, lwd=1.5)
     graphics::axis(side = 1, at = seq(0,1,by=.1))
     graphics::axis(side = 2, at = seq(1,n.x.values,by=1), labels = x.values, las=2, hadj=1)
     graphics::box()
     graphics::par(mar=restore.par.mar, ask=restore.par.ask) # restore original margins
     
     if(printC==TRUE & k==2) grDevices::dev.off()
     }
   }

   if(printC==T) printC(match.call(expand.dots = FALSE))
   invisible(ci.tab)
   
}

