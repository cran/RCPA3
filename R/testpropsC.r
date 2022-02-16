#' Tests the difference between two sample proportions, or difference between sample proportion and hypothesized value, with options for weighted observations, confidence intervals  
#'
#' @description Difference of proportions test with optional sample weights. Reports P-value of two-tailed significance test. Currently limited to testing one response from one dataset. If you want to compare x1 from dataset1 and x2 from dataset2, you can create new dataframe to test as dv ~ iv where dv is vector of x1 and x2 values and iv is vector identifying the source (i.e. dataset1 and dataset2). If you want to compare different responses, such as "Yes" value for x1 and "Agree" value for x2, you will need to transform one of the variables so they have comparable response values.   
#' @param x1 A categorical variable
#' @param x2 Value or variable to compare x1 against. 
#' @param w (Optional) Weights variable.
#' @param data (Optional) Specify name of dataset (data frame) with x1 and x2 variables (or dv and iv).
#' @param dv Dependent variable
#' @param iv Independent variable, should have only two unique values. For comparison purposes, group1 will be first level of iv and group2 will be the second level of iv. To change order of groups, you can modify levels(iv). 
#' @param digits (Optional) Number of digits to report after decimal place, optional (default: 3).
#' @param response (Optional) Identify the response value you wish to compare. If not specified, the function will compare first value of the dv (or x1 variable). If you want to group multiple responses together, use transformC to make dummy variable.
#' @param ci.table (Optional) Do you want a table reporting confidence interval of the difference of proportions? (default: TRUE)
#' @param ci.level (Optional) Desired confidence level, as percentage (default: 95)
#' @param ci.plot (Optional) Do you want a plot of the confidence interval of the difference of proportions? (default: TRUE)
#' @param printC (Optional) Do you want results printed to .html file in your working directory? Default is FALSE. Set to TRUE to print results.
#' @param main (Optional) Main title for plot of confidence interval of difference
#' @param xlab (Optional) Label for x-axis of plot of confidence interval of difference
#' @param xlim (Optional) A vector (of length 2) specifying the range of the x-axis, useful to zoom in on CI. By default, `xlim=c(-1, 1)`.
#' @param ... (Optional) Additional arguments passed to plot function for the CI plot
#' @return No return
#' @examples 
#'    library(RCPA3)
#'    
#'    \donttest{
#'    # one sample test: x1 variable against hypothesized value (of x2)
#'    testpropsC(x1=nes$gun.bg.checks, x2=.500, w=nes$wt, response="1. Favor a great deal", 
#'               xlim=c(0, .2))
#'
#'    # two sample test x1 versus x2
#'    testpropsC(x1=approve.local.covid, x2=approve.pres.covid, w=wt, data=nes, 
#'               response="1. Approve strongly", xlim=c(0, .2))
#'    
#'    # test of proportions dv by iv
#'    testpropsC(dv=marital, iv=gender, w=wt, data=nes, response="3. Widowed", 
#'               xlim=c(-.10, 0))
#'    }
#' @export
#' @section Textbook References:
#' * Philip H. Pollock and Barry C. Edwards, _An R Companion to Political Analysis, 3rd Edition_ (Thousand Oaks, CA: Sage Publications, Forthcoming 2022), Chapter 9. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Thousand Oaks, CA: Sage Publications, 2020), pp.201-215. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Hypothesis Tests with One and Two Samples](https://www.poliscidata.com/pages/rDemosResources.php?chapter=9), Compiled by Barry C. Edwards 
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications.  
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom grDevices png dev.off
#' @importFrom stats na.omit pnorm
#' @importFrom graphics par abline points segments axis box 
#' @md



testpropsC = function(x1, x2, w, data, dv, iv, digits=3, response, printC=FALSE, 
                      ci.table=TRUE, ci.level=95, ci.plot=TRUE, main, xlab, xlim, ...)
  {
  
  if(missing(dv) && missing(iv) && missing(x1) && missing(x2)) stop("Oops. You need to specify a dependent variable (dv) and indepenent variable (iv), or two variables to compare (x1 and x2). Enter example(testpropsC) to see an example and help(testpropsC) to learn more about this function.")
  
  if(!missing(dv)) dv.name = deparse(substitute(dv))
  if(!missing(iv)) iv.name = deparse(substitute(iv))
  if(!missing(w))  w.name  = deparse(substitute(w))
  if(!missing(x1)) x1.name = deparse(substitute(x1))
  if(!missing(x2)) x2.name = deparse(substitute(x2))
  check.value(digits, valuetype="numeric")
  
  if(!missing(data))
  {
    if(is.matrix(data)) data <- data.frame(data)
    if(!missing(dv)) dv <- vector.from.data(substitute(dv), data)
    if(!missing(iv)) iv <- vector.from.data(substitute(iv), data)
    if(!missing(w))  w  <- vector.from.data(substitute(w), data)
    if(!missing(x1)) x1 <- vector.from.data(substitute(x1), data)
    if(!missing(x2)) x2 <- vector.from.data(substitute(x2), data)
  }
  
  if(missing(ci.level)) ci.level <- 95
  check.value(ci.level, valuetype="numeric")
  if((ci.level > 0) & (ci.level < 1)) 
  {
    ci.level <- ci.level*100
    message("Please note: It looks like you entered the level for confidence interval table as a proportion. Confidence level should be percentage, like 95 or 99. Attempting to convert...")
  }
  if((ci.level <= 50) & (ci.level >= 1)) stop(paste(gettext("There's a problem: confidence level for confidence interval table should be percentage, like 95 or 99, not Type I error rate.", domain = "R-descr")))
  
  if(missing(w) && !missing(dv)) w <- rep(1, length(dv))
  if(missing(w) && !missing(x1)) w <- rep(1, length(x1))
  
  if(!missing(w) && length(w)==0) stop(paste("The weights variable,", w.name, "doesn't have values. Check spelling?"))
  

  if(!missing(x1)) 
  {
    if(!is.null(levels(x1))) response.options <- levels(x1) else response.options <- unique(stats::na.omit(x1))
  }
  if(!missing(dv)) 
  {
    if(!is.null(levels(dv))) response.options <- levels(dv) else response.options <- unique(stats::na.omit(dv))
  }
  # if no response value selected, user should select one
  if(missing(response)) 
  {
    message("Note: You did not identify the response value you want to compare.")
    message(paste("Therefore, testpropC will compare the first response value: \"", response.options[1], "\"", sep=""))
    response <- response.options[1]
    message("To compare another response value, use this function's response argument.\n")
    slightpause()
  }
  # if response isn't one of the x1 values, user should select valid one
  if(!(response %in% response.options))
  {
    cat("There's a problem:", response, "isn't one of available response values.\n")
    cat("Here are your options:\n")
    cat(as.character(response.options), sep="\n")
    stop("Please correct this function's response argument or choose another x1.")
  }


if(!missing(x2))
{
  # if response isn't one of the x2 values, user should select valid one
  if(length(x2)!=1)
  {
    # if no response value selected, user should select one
    if(missing(response)) 
    {
      response.options <- unique(stats::na.omit(x2))
      cat("\nThere's a problem: You need to identify the variable value you want to compare.\n")
      cat("Here are your options:", as.character(response.options), "\n")
      stop("Please complete this function's response argument.")
    }
    if(!(response %in% unique(stats::na.omit(x2)))) {
      response.options <- unique(stats::na.omit(x2))
      cat("\nThere's a problem:", response, "isn't one of values of", x2.name, ".\n")
      cat("Here are your options:", as.character(response.options), "\n")
      stop("Please correct this function's response argument or choose another x2.")
    }
  }
  else
  {
    if(!is.numeric(x2)) {
      cat("\nThere's a problem: For one sample test of proportions, x2 must be a single number.")
      stop("Please correct this function's x2 argument or choose another x2.")
    }
  }
}

  if(!missing(dv) & !missing(iv))
  {
    # dv, iv approach: try using cross-tabulation
    xtp.obj1 <- descr::crosstab(dep=dv, indep=iv, weight=w, prop.c=T, digits=12, row.labels=F, cell.layout=FALSE, format="SAS", plot=FALSE)
    # cat("\n\n")
    prop1 <- round(xtp.obj1$prop.col[response,1], digits)
    n1    <- round(sum(xtp.obj1$tab[,1]), digits)
    prop2 <- round(xtp.obj1$prop.col[response,2], digits)
    n2    <- round(sum(xtp.obj1$tab[,2]), digits)
    # return(list(n1,n2))
    group.names <- names(xtp.obj1$cs)
    group1.name <- group.names[1] # names(xtp.obj1$cs)[1]
    group2.name <- group.names[2] # names(xtp.obj1$cs)[2]
    onesampletest <- FALSE
  }

  
  if(!missing(x1) & !missing(x2))
  {
    k1  <- grep(FALSE, (is.na(x1) | is.na(w))) # checks for missing values for x1
    x1  <- x1[k1]
    w1  <- w[k1]
    n1    <- sum(w1)
    prop1 <- sum(as.numeric(x1==response)*w1) / n1
    # one sample test scenario
    if(length(x2)==1) 
    {
        prop2 <- x2
        n2    <- ""
        onesampletest <- TRUE
    }
    # two sample test scenario
    if(length(x2)!=1) 
    {
        # the issue is missing values, should all x1 and x2 be used?
        k2  <- grep(FALSE, (is.na(x2) | is.na(w))) # checks for missing values
        x2  <- x2[k2]
        w2  <- w[k2]
        n2    <- sum(w2)
        prop2 <- sum(as.numeric(x2==response)*w2) / n2
        onesampletest <- FALSE
    }
    group1.name <- x1.name
    group2.name <- x2.name
 }


  
  main.heading <- headingbox("Difference of Proportions Test", width=75, marker="=")
  if(printC==TRUE) printC(main.heading)
  
  testvalue1 <- paste("Prop(", response, "|", group1.name, ")", sep="")
  
  if(!missing(x2) && length(x2)==1) testvalue2 <- x2
  else testvalue2 <- paste("Prop(", response, "|", group2.name, ")", sep="")

  
  null.hypo.statement <- paste("Null hypo:  ", testvalue1," - ", testvalue2, " = 0", sep="")
  alt.hypo.statement <- paste("Alt. hypo:  ", testvalue1," - ", testvalue2, " != 0", sep="")
  test.framework.output <- c("Test Framework:", null.hypo.statement, alt.hypo.statement)
  cat(test.framework.output, sep="\n")
  class(test.framework.output) <- "statement"
  if(printC==TRUE) printC(test.framework.output)
  slightpause()
  

  
  # colnames(obj1) = c("Proportion","N","SD")
  se1 = sqrt(prop1*(1-prop1)) / sqrt(n1)
  if(!missing(x2) && length(x2)==1) se2 = 0
  else se2 = sqrt(prop2*(1-prop2)) / sqrt(n2)
  
  if(onesampletest==TRUE) sediff <- round(se1, digits) # sediff for one sample test

  if(onesampletest==FALSE) 
  {
    pooled.prop <- (prop1*n1 + prop2*n2) / (n1 + n2)
    sediff <- sqrt(pooled.prop*(1-pooled.prop)*(1/n1 + 1/n2))
  }
  
  diff = prop1 - prop2
  
  descr.df.caption = "Descriptive Statistics"
  descr.df <- data.frame(group1=c(prop1, n1), group2=c(prop2, n2))
  colnames(descr.df) <- c(group1.name, group2.name)
  rownames(descr.df) <- c(paste("Prop(", response, ")", sep=""), c("N"))
  print(knitr::kable(format(descr.df, drop0trailing=F, nsmall=digits, digits=digits), format="simple", align="r", caption=descr.df.caption))
  if(printC==TRUE) printC(knitr::kable(format(descr.df, drop0trailing=F, nsmall=digits, digits=digits), format="html", align="r",
                                      caption=printCaption(descr.df.caption)))
  # cat("\n")
  slightpause()
  
  
  z.stats.caption <- "Z-Test Statistics"
  half_alpha = (1 - ci.level/100) / 2
  # avoid rounding numbers until final printout
  zstat = diff/sediff
  pval = stats::pnorm(-1*abs(zstat)) * 2
  z.stats.df <- data.frame(diff, sediff, zstat, pval)
  colnames(z.stats.df) <- c("Diff. of Props.", "SE of Diff.", "Z-statistic", "P-value (2-tailed)")
  rownames(z.stats.df) <- NULL
  print(knitr::kable(format(z.stats.df, drop0trailing=F, nsmall=digits, digits=digits), format="simple", align="r", caption=z.stats.caption))
  if(printC==TRUE) printC(knitr::kable(format(z.stats.df, drop0trailing=F, nsmall=digits, digits=digits), format="html", align="r", 
                                       caption=printCaption(z.stats.caption)))
  # cat("\n")
  slightpause()


  # might need for ci.plot even if ci.table=F
  ci.caption <- paste(ci.level, "% Confidence Interval of Difference of Proportions", sep="")
  sediff.ci <- sqrt(se1^2 + se2^2)
  lower <- round(diff - stats::qnorm(1-half_alpha)*sediff.ci, digits)
  upper <- round(diff + stats::qnorm(1-half_alpha)*sediff.ci, digits)
  ci.df <- data.frame(lower, diff, upper)
  
  if(ci.table!=FALSE)
  {
    # only print CI if requested
    ci.caption <- paste(ci.level, "% Confidence Interval of Difference of Proportions", sep="")
    sediff.ci <- sqrt(se1^2 + se2^2)
    lower <- round(diff - stats::qnorm(1-half_alpha)*sediff.ci, digits)
    upper <- round(diff + stats::qnorm(1-half_alpha)*sediff.ci, digits)
    ci.df <- data.frame(lower, diff, upper)
    colnames(ci.df) <- c("Lower Bound", "Point Estimate", "Upper Bound")
    rownames(ci.df) <- NULL
    print(knitr::kable(format(ci.df, drop0trailing=F, nsmall=digits, digits=digits), format="simple", align="r", caption=ci.caption))
    if(printC==TRUE) printC(knitr::kable(format(ci.df, drop0trailing=F, nsmall=digits, digits=digits), format="html", align="r", 
                                         caption=printCaption(ci.caption)))
    cat("\n")
    slightpause()
  }
  
  
  if(ci.plot==TRUE)
  {
    for(k in 1:(1+as.numeric(printC)))
    {
      
      if(printC==TRUE & k==2) 
      {
        imagename <- paste("testpropsC.plot.", unclass(Sys.time()), ".png", sep="")
        grDevices::png(filename=imagename, width=3.6, height=1.5, units="in", 
                       type=getPNGtype(), pointsize=8, res=300, antialias="default")
        class(imagename) <- "image"
        printC(imagename)
      }
      
      if(missing(xlab)) xlab <- "Estimates of Difference of Proportions"
      if(missing(main)) main <- ci.caption
      main <- strwrap(main, width=50)
      
      if(missing(xlim)) xlim <- c(-1,1)
      restore.par.ask <- graphics::par("ask")
      if(printC==TRUE & k==2) graphics::par(ask=FALSE) else graphics::par(ask=TRUE)
      plot(x="",y="",xlim=xlim, ylim=c(.5,1.5), 
           axes=F, xlab=xlab, ylab="", main=main, ...)
      graphics::abline(v=0, col="gray80")
      graphics::points(x=diff, y=1, pch=16, cex=1)
      graphics::segments(x0 = lower, x1 = upper, y0=1, y1=1, lty=3, lwd=1.5)
      graphics::axis(side = 1, at = seq(-1,1,by=.1))
      # axis(side = 2, at = seq(1,n.x.values,by=1), labels = x.values, las=2, hadj=1)
      graphics::box()
      graphics::par(ask=restore.par.ask) # restore original margins
      
      if(printC==TRUE & k==2) grDevices::dev.off()
    }
  }
  
  
  if(printC==T) printC(match.call(expand.dots = FALSE))
  
}
