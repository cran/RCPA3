#' One and two-sample difference of means tests (t-tests) with confidence intervals.
#'
#' @description Conducts one and two-sample difference of means tests (t-tests). Options for weighting observations, known population standard deviation, equal or unequal variances, paired observations. 
#' @param x1 The first variable to be compared (mean of x1 will be compared to mean of x2). Must be numeric variable. Should be in the form dataset$var, unless dataset specified with data argument.
#' @param x2 The variable (or number) to which x1 is compared. Should be in the form dataset$var, unless dataset specified with data argument. You can set x2 equal to a number to conduct a one sample means test. For example, to test whether x1 could have population mean of 50, you'd set x2 = 50.
#' @param w (Optional) Weights variable (optional). Should be in the form dataset$var, unless dataset specified with data argument.
#' @param data (Optional) The dataset that contains x1, x1 and x2, or dv and iv. 
#' @param dv The dependent variable. Must be numeric variable. Should be in the form dataset$var, unless dataset specified with data argument.
#' @param iv	The independent variable. Should have two distinct values (like treatment and control). Should be in the form dataset$var, unless dataset specified with data argument.
#' @param digits (Optional) Number of digits to report after decimal place, optional (default: 3).
#' @param ci.table (Optional) Confidence level for calculating the confidence interval of the difference of means, defaults to 95. Set to F or FALSE to omit confidence interval from results.
#' @param ci.level (Optional) Desired confidence level, as percentage (default: 95)
#' @param ci.plot (Optional) Do you want a plot of the confidence interval of the difference of means? (default: TRUE)
#' @param var.equal (Optional) With two-sample tests, do you want to assume equal variances? (default: FALSE) 
#' @param paired (Optional) With two-sample tests, are the observations paired? (default: FALSE)
#' @param pop.sd (Optional) If the population standard deviation is known, you can specify it.
#' @param var.test (Optional) If set to TRUE, will test the assumption that two sample variance are equal using an F test. Default is FALSE. The var.test option implemented for both weighted and unweighted analysis. If you are not using sample weights, you can supplement this F test with additional tests such as `stats::bartlett.test` and `car::leveneTest`.  
#' @param printC (Optional) Do you want results printed to .html file in your working directory? Default is FALSE. Set to TRUE to print results.
#' @param main (Optional) Main title for plot of confidence interval of difference
#' @param xlab (Optional) Label for x-axis of plot of confidence interval of difference
#' @param xlim (Optional) A vector (of length 2) specifying the range of the x-axis, useful to zoom in on CI. 
#' @param ... (Optional) Additional arguments passed to `plot` function for the CI plot
#' @return No return
#' @examples 
#'    library(RCPA3)
#'    
#'    \donttest{ 
#'    # one sample test against hypothesized value
#'    testmeansC(x1=world$literacy, x2=80)
#'    
#'    # with x1 and x2 
#'    testmeansC(x1=ft.trump.post, x2=ft.pence.post, w=wt, data=nes)
#'    
#'    # with paired x1 and x2
#'    testmeansC(x1=nes$ft.pence.post, x2=nes$ft.pence.pre, w=nes$wt, paired=TRUE)
#'
#'    # with dv and iv 
#'    testmeansC(dv=nes$ft.bigbiz, iv=nes$gender, w=nes$wt)
#'    }
#' @export
#' @section Textbook References:
#' * Philip H. Pollock and Barry C. Edwards, _An R Companion to Political Analysis, 3rd Edition_ (Thousand Oaks, CA: Sage Publications, Forthcoming 2022), Chapter 9. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Thousand Oaks, CA: Sage Publications, 2020), pp.201-215. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Hypothesis Tests with One and Two Samples](https://www.poliscidata.com/pages/rDemosResources.php?chapter=9), Compiled by Barry C. Edwards 
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications. 
#' * [Political Science Data Web Site](https://www.poliscidata.com), find datasets for your own research and resources to help with the analysis. 
#' @importFrom grDevices png dev.off
#' @importFrom stats na.omit pt pnorm pf
#' @importFrom graphics par abline points segments axis box
#' @md



testmeansC <- function(x1, x2, w, data, dv, iv, digits=2, var.equal=FALSE, paired=FALSE, pop.sd=FALSE, 
                       var.test=FALSE, printC=FALSE, ci.table=TRUE, ci.level=95, ci.plot=TRUE, main, xlab, xlim, ...)
{
  if(missing(dv) && missing(iv) && missing(x1) && missing(x2)) stop("Oops. You need to specify a dependent variable (dv) and indepenent variable (iv), or two variables (x1 and x2). Enter example(testmeansC) to see an example and help(testmeansC) to learn more about this function.")
  check.value(digits, valuetype="numeric")
  
  if(!missing(dv)) dv.name = deparse(substitute(dv))
  if(!missing(iv)) iv.name = deparse(substitute(iv))
  if(!missing(w))  w.name  = deparse(substitute(w))
  if(!missing(x1)) x1.name = deparse(substitute(x1))
  if(!missing(x2)) x2.name = deparse(substitute(x2))
  
  if(!missing(data))
  {
    if(is.matrix(data)) data <- data.frame(data)
    if(!missing(dv)) dv <- vector.from.data(substitute(dv), data)
    if(!missing(iv)) iv <- vector.from.data(substitute(iv), data)
    if(!missing(w))  w  <- vector.from.data(substitute(w), data)
    if(!missing(x1)) x1 <- vector.from.data(substitute(x1), data)
    if(!missing(x2)) x2 <- vector.from.data(substitute(x2), data)
  }

# what about missing values? does function need to manually exclude them?
# I think both analysis functions are okay with missing values in vectors

if(!missing(dv)) check.variable(dv, vartype="numeric")
if(!missing(iv)) 
  {
    check.variable(iv)
    if(length(unique(stats::na.omit(iv))) > 2) stop(paste(gettext("There's a problem:", domain = "R-descr"), iv.name, gettext("has more than 2 unique values. Collapse values or use ANOVA instead?", domain = "R-descr")))
}
  
if(!missing(w))
  {
    check.variable(w, vartype="numeric")
    weighted <- TRUE
  }
  else weighted <- FALSE

if(!missing(x1)) check.variable(x1, vartype="numeric")
  
  if(missing(ci.level)) ci.level <- 95
  check.value(ci.level, valuetype="numeric")
  if((ci.level > 0) & (ci.level < 1)) 
  {
    ci.level <- ci.level*100
    message("Please note: It looks like you entered the level for confidence interval table as a proportion. Confidence level should be percentage, like 95 or 99. Attempting to convert...")
  }
  if((ci.level <= 50) & (ci.level >= 1)) stop(paste(gettext("There's a problem: confidence level for confidence interval table should be percentage, like 95 or 99, not Type I error rate.", domain = "R-descr")))
  
  
  onesampletest <- FALSE
  if(!missing(x2)) 
  {
      if(length(x2) == 1) check.value(x2, valuetype="numeric")
      if(length(x2) == 1) onesampletest <- TRUE    
  }
  

  if(!missing(dv) && missing(iv) && missing(x1) && missing(x2)) stop(paste(gettext("There's a problem: You have a dv but didn't specify the iv", domain = "R-descr")))
  if(missing(dv) && !missing(iv) && missing(x1) && missing(x2)) stop(paste(gettext("There's a problem: You have a iv but didn't specify the dv", domain = "R-descr")))
  if(missing(dv) && missing(iv) && !missing(x1) && missing(x2)) stop(paste(gettext("There's a problem: You have an x1 but didn't specify the x2.", domain = "R-descr")))
  if(missing(dv) && missing(iv) && missing(x1) && !missing(x2)) stop(paste(gettext("There's a problem: You have an x2 but didn't specify the x1.", domain = "R-descr")))

  
  half_alpha = (1 - ci.level/100) / 2

  main.heading <- headingbox("Difference of Means Test", width=75, marker="=")
  if(printC==TRUE) printC(main.heading)
  

  # set-up for dv ~ iv comparison, can't be paired comparison or one sample test
  if(!missing(dv) && !missing(iv) && missing(x1) && missing(x2)) 
  {
    if(!is.null(levels(iv))) iv.values <- levels(iv) else iv.values <- unique(iv)
    x1 <- dv[iv==iv.values[1]]
    if(missing(w)) w1 <- rep(1, length(x1)) else w1 <- w[iv==iv.values[1]]
    x2 <- dv[iv==iv.values[2]]
    if(missing(w)) w2 <- rep(1, length(x2)) else w2 <- w[iv==iv.values[2]]
    group1.name <- iv.values[1]
    group2.name <- iv.values[2]
    x1.name <- paste("Mean(", dv.name, "|", group1.name, ")", sep="")
    x2.name <- paste("Mean(", dv.name, "|", group2.name, ")", sep="")
    paired <- FALSE
    onesampletest <- FALSE
  }
  

  # set-up for x1 v. x2 comparison, could be a paired test or one sample
  if(!missing(x1) && !missing(x2) && missing(dv) && missing(iv)) 
  {
    if(missing(w)) 
      {
        w <- w1 <- rep(1, length(x1))  
        w2 <- rep(1, length(x2))
      }
    else w2 <- w1 <- w
    group1.name <- x1.name
    group2.name <- x2.name
  }
  
  ### need to process x1 x2 and w so it's not inflating N when w==1 and x1 or x2 missing
  if(paired==TRUE)
  {
    k <- grep(FALSE, (is.na(x1) | is.na(x2) | is.na(w))) # checks for missing values for x1
    x1 <- x1[k]
    x2 <- x2[k]
    w1 <- w2 <- w[k]
    var.equal <- FALSE
  }
  else if(onesampletest==TRUE) # is a one-sample test
  {
    mean2 <- x2
    sd2 <- 0
    n2 <- 0
    var.equal <- FALSE
    paired <- FALSE
  }
  else # paired==FALSE and onesampletest==FALSE
  {
    k1  <- grep(FALSE, (is.na(x1) | is.na(w1))) # checks for missing values for x1
    x1  <- x1[k1]
    w1  <- w1[k1]
    k2  <- grep(FALSE, (is.na(x2) | is.na(w2))) # checks for missing values for x2
    x2  <- x2[k2]
    w2  <- w2[k2]
  }

  n1  <- sum(w1)
  mean1 <- wtd.mean(x1, w1)
  sd1 <- wtd.sd(x1, w1)

  if(onesampletest==FALSE)
  {
    mean2 <- wtd.mean(x2, w2)
    sd2 <- wtd.sd(x2, w2)
    n2  <- sum(w2)
  }
  
  
  
  meancomptable <- data.frame(means=c(mean1, mean2),
                              n=c(n1, n2),
                              sd=c(sd1, sd2))
  rownames(meancomptable) <- c(group1.name, group2.name)
  colnames(meancomptable) <- c("Mean","N","St. Dev.")
  
  null.hypo.statement <- alt.hypo.statement <- NULL
  variances.statement <- paired.statement <- NULL
  
  null.hypo.statement <- paste("Null Hypothesis: ", x1.name, " - ", x2.name, " = 0", sep="")
  alt.hypo.statement <- paste("Alt. Hypothesis: ", x1.name, " - ", x2.name, " != 0", sep="")
  # cat("\n\n")

    # assumptions about variances
    if(pop.sd!=FALSE & paired==FALSE) # if known, pop.sd entered as a number
    {
      check.value(pop.sd, valuetype="numeric")
      # could also say that comptable[,4] <- rep(pop.sd, nrow(comptable))
      se1 <- pop.sd / sqrt(n1)
      if(onesampletest==FALSE) se2 <- pop.sd / sqrt(n2) else se2 <- 0
      sediff <- sqrt(se1^2 + se2^2)
      df <- NA # using norm dist not t-dist.
      meancomptable[,3] <- pop.sd
      colnames(meancomptable)[3] <- "Pop. St. Dev."
      variances.statement <- paste("Population standard deviation assumed to be ", pop.sd, ".", sep="")
      # will use a z-statistic instead of t-statistic
      var.equal==FALSE
    }
    else if(paired==TRUE)
    {
      if(pop.sd!=FALSE) sediff <- pop.sd / sqrt(n1)
      differences <- x1 - x2
      diff.means <- wtd.mean(differences, w1)
      if(pop.sd==FALSE) sediff <- wtd.sd(differences, w1) / sqrt(n1)
      df <- n1 - 1
      if(pop.sd==FALSE) variances.statement <- paste("Population standard deviation is unknown.")
      if(pop.sd!=FALSE) variances.statement <- paste("Population standard deviation assumed to be ", pop.sd, ".", sep="")
    }
    else if(var.equal==TRUE)
    {
      variances.statement <- paste("Population standard deviation is unknown. Assuming that samples have equal variances.")
      pooled.variance <- ((n1-1)*sd1^2+(n2-1)*sd2^2) / (n1+n2-2)
      sediff <- sqrt(pooled.variance)*(sqrt((1/n1)+(1/n2)))
      df <- n1 + n2 - 2
    }
    else
    {
      se1 <- sd1 / sqrt(n1)
      if(onesampletest==FALSE) se2 <- sd2 / sqrt(n2) else se2 <- 0
      sediff <- sqrt(se1^2 + se2^2)
      df <- floor(((se1^2+se2^2)^2)/(((1/(n1-1))*(se1^4)) +((1/(n2-1))*(se2^4))))
      variances.statement <- paste("Population standard deviation is unknown. Not assuming that samples have equal variances.")
    }
  
  test.framework.output <- c("Test Framework and Assumptions", "", null.hypo.statement, alt.hypo.statement, variances.statement)
  if(paired==TRUE) test.framework.output <- c(test.framework.output, "Observations are paired.")
  
  cat(test.framework.output, sep="\n")
  class(test.framework.output) <- "statement"
  if(printC==TRUE) printC(test.framework.output)
  slightpause()
  
  
    if(onesampletest==TRUE) meancomptable <- meancomptable[-2,]

    descr.tab.caption <- "Descriptive Statistics"

    print(knitr::kable(format(meancomptable, drop0trailing=F, nsmall=digits, digits=digits), format="simple", caption=descr.tab.caption, align="r"))
    
    if(printC==TRUE) printC(knitr::kable(format(meancomptable, drop0trailing=F, nsmall=digits, digits=digits), 
                                         format="html", caption=printCaption(descr.tab.caption), align="r"))
    cat("\n")
    slightpause()
    

    
    if(pop.sd==FALSE) t.stats.tab.caption <- "t-Test Statistics"
    if(pop.sd!=FALSE) t.stats.tab.caption <- "Z-Test Statistics"
    if(paired==FALSE) diff.means <- mean1 - mean2
    tstat <- diff.means/sediff
    if(pop.sd==FALSE) pvalue <- stats::pt(q = abs(tstat), df = df, lower.tail = F) * 2 # two-tailed value
    if(pop.sd!=FALSE) pvalue <- stats::pnorm(-1*abs(tstat), 0, 1) * 2
    t.stats.tab <- data.frame(diff.means, sediff, tstat, df, pvalue=pvalue)
    # t.stats.tab <- data.frame(t(t.stats.tab))
    names(t.stats.tab) = c("Diff. of Means", "SE of Diff.", "t-Statistic", "DF","P-value (2-tailed)")
    if(pop.sd!=FALSE) 
      {
        names(t.stats.tab)[3] <- "Z-Statistic"
        t.stats.tab <- t.stats.tab[, -4]
      }
    print(knitr::kable(format(t.stats.tab, drop0trailing=F, nsmall=digits, digits=digits), format="simple", caption=t.stats.tab.caption, align="r"))
    if(printC==TRUE) printC(knitr::kable(format(t.stats.tab, drop0trailing=F, nsmall=digits, digits=digits), 
                                         format="html", caption=printCaption(t.stats.tab.caption), align="r"))
    cat("\n")
    slightpause()

    # ci.table.caption might be used with ci.plot though ci.table=F
    ci.table.caption <- paste(ci.level, "% Confidence Interval of Difference of Means", sep="")
    if(pop.sd==FALSE) 
    {
      lower = diff.means - stats::qt(1-half_alpha, df=df)*sediff
      upper = diff.means + stats::qt(1-half_alpha, df=df)*sediff
    }
    else
    {
      lower = diff.means - stats::qnorm(1-half_alpha, 0, 1)*sediff
      upper = diff.means + stats::qnorm(1-half_alpha, 0, 1)*sediff
    }
    if(ci.table != FALSE)
    {
      # print(list(lower, diff, upper))
      ci.tab = data.frame(lower, diff.means, upper)
      # print(ci.tab)
      names(ci.tab) = c("Lower Bound", "Point Estimate","Upper Bound")
      print(knitr::kable(format(ci.tab, drop0trailing=F, nsmall=digits, digits=digits), format="simple", caption=ci.table.caption, align="r"))
      if(printC==TRUE) printC(knitr::kable(format(ci.tab, drop0trailing=F, nsmall=digits, digits=digits), format="html", align="r",
                                           caption=printCaption(ci.table.caption)))
      cat("\n")
      slightpause()
    } # end of ci.table

    
    if(var.test==T)
    {
      var.test.heading <- "Equal Variances Test"
      headingbox(var.test.heading, marker="-")
      
      if(!missing(dv) && !missing(iv)) 
      {
        x1.name <- paste(dv.name, "|", group1.name, sep="")
        x2.name <- paste(dv.name, "|", group2.name, sep="")
      }
      if(onesampletest==TRUE) message("Sorry, you can't compare two variances with a one sample test.")
      else if(paired==TRUE) message("Sorry, you can't compare two variances with a paired samples test.")
      else 
        { 
          var.test.null.hypo <- paste("Null Hypothesis:", x1.name, "and", x2.name, "have equal variances.")
          var1 <- wtd.var(x1, w1)
          var2 <- wtd.var(x2, w2)
          f.stat <- var1/var2
          df1 <- n1 - 1
          df2 <- n2 - 1
          p_value_fstat <- min(stats::pf(f.stat, df1, df2, lower.tail=F), stats::pf(f.stat, df1, df2, lower.tail=T)) * 2
          var.group1 <- paste("Variance(", x1.name, "): ", round(var1, digits))
          var.group2 <- paste("Variance(", x2.name, "): ", round(var2, digits))
          f.test.results <- paste("F-stat: ", round(f.stat, digits), ",  num df: ", df1, ",  denom df: ", df2, ",  p-value: ", round(p_value_fstat, digits), sep="")
          var.test.output <- c(var.test.heading, "", var.test.null.hypo, "",
                               var.group1, var.group2, "", f.test.results)
          if(weighted==TRUE) var.test.output <- c(var.test.output, paste("Note: Variances and df weighted by", w.name))
          cat(var.test.output, sep="\n")
          class(var.test.output) <- "statement"
          if(printC==TRUE) printC(var.test.output)
        }
      # print(stats::var.test(x1, x2)) confirm output in unweighted analysis
    }
    cat("\n")
    
    
    
    if(ci.plot==TRUE)
    {
      # if printC=T this runs plotting commands twice so plot seen in R and then goes to PNG driver
      for(k in 1:(1+as.numeric(printC)))
      {
        
        if(printC==TRUE & k==2) 
        {
          imagename <- paste("testmeansC.plot.", unclass(Sys.time()), ".png", sep="")
          grDevices::png(filename=imagename, width=3.6, height=1.5, units="in", type="cairo", pointsize=8, res=300, antialias="default")
          class(imagename) <- "image"
          printC(imagename)
        }
        
        restore.par.ask <- graphics::par("ask")
        if(printC==TRUE && k==2) graphics::par(ask=FALSE) else graphics::par(ask=TRUE)
        
        if(missing(xlab)) xlab <- "Estimate of Difference of Means"
        if(missing(main)) main <- ci.table.caption
        if(missing(xlim)) xlim <- c(lower-.5*abs(diff.means-lower), upper+.5*abs(upper-diff.means))
        main <- strwrap(main, width=50)
        
        plot(x="",y="",xlim=xlim, ylim=c(.5,1.5), axes=F, xlab=xlab, ylab="", main=main, ...)
        graphics::abline(v=0, col="gray80")
        graphics::points(x=diff.means, y=1, pch=16, cex=1.5)
        graphics::segments(x0 = lower, x1 = upper, y0=1, y1=1, lty=3, lwd=1.5)
        graphics::axis(side = 1)
        graphics::box()
        graphics::par(ask=restore.par.ask)
        
        
        if(printC==TRUE & k==2) grDevices::dev.off()
      }
    }
    
    
 if(printC==T) printC(match.call(expand.dots = FALSE))
    
}
