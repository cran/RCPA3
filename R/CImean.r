#' Confidence interval of a dataset variable's sample mean in table and figure
#'
#' @description Reports the confidence interval of a sample mean in table and plot. Default is 95% CI but use can raise or lower confidence level. 
#' @param x A numeric variable, should be in dataset$var form unless dataset specified in optional data argument.
#' @param w (Optional) Sampling weights of variable (optional), must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains x (and w) variable (optional).
#' @param digits (Optional) Number of decimal places reported in result (defaults to 3).
#' @param level (Optional) A single number equal to the desired confidence level (i.e. 95, 99, 90, etc.). Default value is 95 percent confidence level.
#' @param pop.sd (Optional) A single number equal to the known population standard deviation of x. This value is rarely know, but if it is, critical values for confidence interval are based on standard normal distribution.
#' @param printC (Optional) Do you want results printed to .html file in your working directory? Default is FALSE. Set to TRUE to print results.
#' @param plot (Optional) Do you want a plot of the confidence interval? Default is TRUE.
#' @param main (Optional) Change the main title of plot. Default title generated from level, x, and w.
#' @param xlab (Optional) Label for x-axis of confidence interval plot.
#' @param xlim (Optional) Modify x-axis limits of confidence interval plot.
#' @param ... (Optional) additional arguments passed to `plot` function.
#' @return Returns the confidence interval as a vector of numeric values (the lower and upper bounds).
#' @examples  
#'   library(RCPA3)
#'   
#'   CImean(nes$age)
#'   
#'   \dontrun{
#'   # using optional w, level, and data arguments
#'   CImean(x=nes$age, w=nes$wt, level=90)
#'   CImean(x=age, data=nes, level=95)
#'   }
#' @export
#' @section RCPA3 Package Tutorial Videos:
#' * [Confidence Intervals for Sample Means with RCPA3's CImean Function](https://www.youtube.com/watch?v=EmU7cQJ_uRM) 16:16 
#' * [Complete Playlist of RCPA3 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook References:
#' * Philip H. Pollock and Barry C. Edwards, _An R Companion to Political Analysis, 3rd Edition_ (Thousand Oaks, CA: Sage Publications, Forthcoming 2022), Chapter 8. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Thousand Oaks, CA: Sage Publications, 2020), pp. 184-186. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Foundations of Inference](https://www.poliscidata.com/pages/rDemosResources.php?chapter=8), Compiled by Barry C. Edwards
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications. 
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom Hmisc all.is.numeric
#' @importFrom knitr kable
#' @importFrom grDevices png dev.off
#' @importFrom graphics par abline points segments axis box
#' @md


CImean = function(x, w, data, digits=3, level=95, pop.sd, printC=FALSE, 
                  plot=TRUE, main, xlab, xlim, ...) 
  {
  
  if(missing(x)) stop("Oops. You need to specify the variable to analyze with the x argument. To see how to use this function, try example(ci.prop) or enter help(ci.prop)")
  
  if(plot!=FALSE)
  {
    old.par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old.par))
  }
  
  x.name = deparse(substitute(x))
  if(!missing(w)) w.name = deparse(substitute(w))
  
  check.value(digits, valuetype="numeric")
  check.value(level, valuetype="numeric")
  if((level > 0) & (level < 1)) 
  {
    level <- level*100
    message("Please note: It looks like you entered confidence level as a proportion. Confidence level should be percentage, like 95 or 99. Attempting to convert...")
  }
  if((level <= 50) & (level >= 1)) stop(paste(gettext("There's a problem: confidence level should be percentage, like 95 or 99, not Type I error rate.", domain = "R-descr")))

  if(!missing(data))
  {
    if(is.matrix(data)) data <- data.frame(data)
    x <- vector.from.data(substitute(x), data)
    if(!missing(w)) w <- vector.from.data(substitute(w), data)
  }

  if(Hmisc::all.is.numeric(x, extras=c(NA))) x <- as.numeric(x)
  check.variable(x, vartype = "numeric")
  x.length <- length(x)
  if(!is.null(attr(x, "label"))) x.label <- attr(x, "label")  
  
  if(!missing(w)) 
  {
    weighted = TRUE
    if (x.length != length(w)) stop(paste(x.name, gettext("and", domain = "R-descr"), w.name, gettext("have different lengths.", domain = "R-descr")))
  }
  else
  {
    w <- rep(1, x.length)
    weighted = FALSE
  }
  check.variable(w, vartype = "numeric")
  
  if(!missing(pop.sd)) check.value(pop.sd, valuetype="numeric")
  
  k <- grep(FALSE, (is.na(x) | is.na(w))) # checks for missing values
  x <- x[k]
  w <- w[k]
  
  half_alpha = (1 - level/100) / 2
  x.n  <- sum(w) # the n of sample, needs to adjust in case of weights
  
  mean <- wtd.mean(x, w)
  sd   <- wtd.sd(x, w)
  x.n  <- sum(w) # the n of sample, needs to adjust in case of weights
  
  half_alpha = (1 - level/100) / 2
   
   if(!missing(pop.sd)) {
     se <- pop.sd/sqrt(x.n)
     critical_value <- stats::qnorm(1-half_alpha)
   } else
     {
       se <- sd/sqrt(x.n)
       critical_value <- stats::qt(1-half_alpha, df=(x.n-1))
     }

   lower = mean - critical_value*se
   upper = mean + critical_value*se
   

   
   main.heading <- headingbox("Confidence Interval of a Mean", width=75, marker="=")
   if(printC==TRUE) printC(main.heading)
   
   title <- paste(level, "% CI of Mean of ", x.name, sep="")
   if(exists("x.label")) title <- paste(title, " (", x.label, ")", sep="")
   if(weighted==TRUE) title <- paste(title, ", weighted by ", w.name, sep="")
   

   ci.tab <- data.frame(lower, mean, upper)
     names(ci.tab) = c("Lower Bound", "Point Estimate","Upper Bound")
     print(knitr::kable(format(ci.tab, drop0trailing=F, nsmall=digits, digits=digits), format="simple", caption=title, align="r"))
     if(printC==TRUE) printC(knitr::kable(format(ci.tab, drop0trailing=F, nsmall=digits, digits=digits), 
                                          format="html", align="r", caption=printCaption(title)))
     cat("\n")
     # print(out2)

   
   if(plot==TRUE)
   {
     # if printC=T this runs plotting commands twice so plot seen in R and then goes to PNG driver
     for(k in 1:(1+as.numeric(printC)))
     {
       
     if(printC==TRUE & k==2) 
     {
       imagename <- paste("CImean.plot.", unclass(Sys.time()), ".png", sep="")
       grDevices::png(filename=imagename, width=3.6, height=1.5, units="in", 
                      type=getPNGtype(), pointsize=8, res=300, antialias="default")
       class(imagename) <- "image"
       printC(imagename)
     }
       
     restore.par.ask <- graphics::par("ask")
     if(printC==TRUE && k==2) graphics::par(ask=FALSE) else graphics::par(ask=TRUE)
     
     if(missing(xlab)) xlab <- "Estimate of Population Mean"
     if(missing(main)) main <- title
     if(missing(xlim)) xlim <- c(lower-.5*abs(mean-lower), upper+.5*abs(upper-mean))
     main <- strwrap(main, width=50)
     
     plot(x="",y="",xlim=xlim, ylim=c(.5,1.5), axes=F, xlab=xlab, ylab="", main=main, ...)
     graphics::abline(v=0, col="gray80")
     graphics::points(x=mean, y=1, pch=16, cex=1.5)
     graphics::segments(x0 = lower, x1 = upper, y0=1, y1=1, lty=3, lwd=1.5)
     graphics::axis(side = 1)
     graphics::box()
     graphics::par(ask=restore.par.ask)
     

     if(printC==TRUE & k==2) grDevices::dev.off()
     }
   }

    if(printC==T) printC(match.call(expand.dots = FALSE))
    invisible(ci.tab)
     
}

