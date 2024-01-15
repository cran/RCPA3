#' Linear regression analysis (OLS regression), with options for weighted observations, diagnostic tests, and plots of residuals
#'
#' @description Linear regression analysis function with many useful features. Standard output of results includes table of coefficients, table of residuals, and additional model information. Options for weighting observations, analysis of variance (ANOVA), performing post-estimation diagnostic tests, including testing normality of residuals and constant variance, and generating diagnostic plots of residuals.
#' @param formula should be in dataset$dv ~ datatset$iv1 + dataset$iv2 unless dataset specified in optional data argument. If weights are specified using w argument, the formula cannot contain functions or logical statements (all variables in the function must be named in the dataset).
#' @param w (Optional) Sampling weights of variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains dv, iv (and w) variables.
#' @param digits (Optional) Number of decimal places reported in result (defaults to 2).
#' @param anova (Optional) Do you want ANOVA table reporting F-test for all predictors? (default: FALSE)
#' @param norm.test (Optional) Test assumption that regression residuals follow normal distribution (default: FALSE)
#' @param ncv.test (Optional) Test assumption that regression residuals have constant variance (default: FALSE)
#' @param linear.test (Optional) Report results of linearity test? (default: FALSE)
#' @param reset.test (Optional) Report results of model specification test? (default: FALSE)
#' @param outlier.test (Optional) Test whether outlier observations have outsized leverage on results (default: FALSE)
#' @param vif (Optional) Report variance inflation factors to assess multicollinearity? (default: FALSE) 
#' @param printC (Optional) Do you want to print tables of results (and residuals plots if res.plots=TRUE) to .html file in working directory? (default: FALSE)
#' @param res.plots (Optional)
#' @param ... (Optional) Additional arguments passed to \code{\link[stats]{lm}} function (unweighted models) or \code{\link[survey]{svyglm}} function (weighted models).
#' @return Returns a lm or svyglm object.
#' @examples 
#'   library(RCPA3)
#'   
#'   \donttest{ 
#'   # basic usage
#'   regC(states$vep20.turnout ~ states$hs.or.more)
#'    
#'   # with w and data arguments
#'   regC(nes$ft.unions ~ nes$ft.dem, w=nes$wt)
#'   
#'   # multiple IV with some post-estimation tests
#'   regC(peace.index ~ vdem.edi.score + hdi, data=world, norm.test=TRUE, ncv.test=TRUE)
#'   }
#' @export
#' @section RCPA3 Package Tutorial Videos:
#' * [Bivariate Regression Analysis with RCPA3 Package's regC Function](https://www.youtube.com/watch?v=53NPD-BGvwU) 10:31   
#' * [Multiple Regression Analysis with RCPA3 Package's regC Function](https://youtu.be/17Dxg5D3qtQ) 14:55   
#' * [Analyzing Regression Residuals with RCPA3 Package's regC Function](https://youtu.be/97hoiYuQnl4) 12:41   
#' * [Complete Playlist of RCPA3 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook References:
#' * Philip H. Pollock and Barry C. Edwards, _An R Companion to Political Analysis, 3rd Edition_ (Thousand Oaks, CA: Sage Publications, Forthcoming 2022), Chapters 11, 12, 13. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Thousand Oaks, CA: Sage Publications, 2020), pp. 244-271. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Correlation and Bivariate Regression](https://www.poliscidata.com/pages/rDemosResources.php?chapter=11), [Multiple Regression](https://www.poliscidata.com/pages/rDemosResources.php?chapter=12), and [Analyzing Regression Residuals](https://www.poliscidata.com/pages/rDemosResources.php?chapter=13), compiled by Barry C. Edwards.
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications.  
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom methods is
#' @importFrom survey svydesign svyglm
#' @importFrom car Anova outlierTest vif
#' @importFrom stats lm as.formula pchisq shapiro.test
#' @importFrom lmtest harvtest resettest bptest hmctest
#' @importFrom graphics par
#' @importFrom grDevices png dev.off
#' @md



regC <- function(formula, w, data, digits=3, anova=FALSE, 
                 norm.test=FALSE, ncv.test=FALSE, linear.test=FALSE, reset.test=FALSE, 
                 outlier.test=FALSE, vif=FALSE, printC=FALSE, res.plots=FALSE, ...) 
{
  
  if(missing(formula) || (length(formula) != 3)) stop("There's a problem: Your formula is missing or specified incorrectly. Try example(regC) or help(regC) for assistance with this function.")
  if(!methods::is(formula, class2="formula")) stop("There's a problem: The regC function works with a formula in the form dv ~ iv.")
  check.value(digits, valuetype="numeric")
  
  if(res.plots==TRUE)
  {
    old.par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old.par))
  }
  
  if(!missing(w)) w.name <- deparse(substitute(w))
    
  if(!missing(data))
  {
    data.name <- deparse(substitute(data))
    if(is.matrix(data)) data <- data.frame(data)
    if(!missing(w)) 
    {
      w <- data$w <- vector.from.data(substitute(w), data)
    }
  }
  
  if(!missing(w)) {
    w.length <- length(w)
    w.label <- attr(w, "label")  
    weighted = TRUE
    if(is.numeric(w)) class(w) <- "numeric"
    check.variable(w, vartype="numeric")
  }
  else 
  {
    weighted = FALSE
  }
  
  if(weighted==FALSE) reg.model <- stats::lm(formula, data=data, na.action="na.omit", ...)
  
  if(weighted==TRUE)
  {
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "w", "data"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf[[1L]] <- quote(stats::model.frame)
    dataframe <- eval(mf)
    # return(list(colnames(dataframe), w.name))
    colnames(dataframe) <- gsub("$", ".", colnames(dataframe), fixed=TRUE)
    colnames(dataframe)[colnames(dataframe)=="(w)"] <- "w"
    svy.design <- survey::svydesign(ids=~0, weights=~w, data=dataframe)
    svy.formula <- gsub("$", ".", substitute(formula), fixed=TRUE)
    svy.formula <- stats::as.formula(paste(svy.formula[2], svy.formula[1], svy.formula[3]))
    reg.model <- survey::svyglm(formula=svy.formula, design=svy.design, na.action="na.omit", family="gaussian", ...)
    svy.design <- NULL
    dataframe <- NULL
  }
  
  main.heading <- headingbox("Linear Regression Analysis", marker="=")
  if(printC==TRUE) printC(main.heading)

  # identify the DV as a helpful reminder
  dv.name <- as.character(formula)[[2]]

  # standard and weighted tables print the same way
  coef.table <- round(summary(reg.model)$coefficients, digits)
  coef.table[coef.table[,4]==0,4] <- "<.001"
  coef.heading <- "Linear Regression Coefficients"
  print(knitr::kable(format(coef.table, drop0trailing=F, nsmall=digits, digits=digits), format="simple", caption=coef.heading, align="r"))
  slightpause()
  if(printC==TRUE) printC(knitr::kable(coef.table, format="html", caption=printCaption(coef.heading), digits=3, escape=F, align="r"))
  
  residuals.table <- round(t(data.matrix(summary(reg.model$residuals)[-4])), digits)
  residuals.heading <- "Regression Residuals"
  print(knitr::kable(format(residuals.table, drop0trailing=F, nsmall=digits, digits=digits), format="simple", caption=residuals.heading, align="r"))
  slightpause()
  if(printC==TRUE) printC(knitr::kable(residuals.table, format="html", caption=printCaption(residuals.heading), digits=3))
  
  # the additional information is different for standard and weighted reg
  if(weighted==FALSE) 
  {
    lm.summary.output <- utils::capture.output(summary(reg.model))
    if(is.null(reg.model$na.action)) capture.lines <- 5 else capture.lines <- 6
    lm.summary.output <- lm.summary.output[(length(lm.summary.output)-capture.lines):length(lm.summary.output)]
    lm.summary.output[length(lm.summary.output)] <- paste("Dependent variable:", dv.name)
    lm.summary.output[1] <- "Additional Information:"
    n.sample <- length(reg.model$residuals)
    if(is.null(reg.model$na.action)) 
    {
      lm.summary.output[7] <- lm.summary.output[6]
      lm.summary.output[6] <- lm.summary.output[5]
      lm.summary.output[5] <- lm.summary.output[4]
      lm.summary.output[4] <- paste("N:", n.sample, "(0 observations deleted due to missingness)")
    }
    else lm.summary.output[4] <- paste("N:", n.sample, lm.summary.output[4])
    cat("\n\n")
    cat(lm.summary.output, sep="\n")
    slightpause()
    class(lm.summary.output) <- "statement"
    if(printC==TRUE) printC(lm.summary.output)
    
  }
  
  if(weighted==TRUE) 
  {

    svyglm.summary.output <- utils::capture.output(summary(reg.model))
    svyglm.summary.output <- svyglm.summary.output[(length(svyglm.summary.output)-5):length(svyglm.summary.output)]
    svyglm.summary.output[1] <- "Additional Information:"
    # svyglm.summary.output[8] <- svyglm.summary.output[3] # dispersion parameter
    # svyglm.summary.output[9] <- svyglm.summary.output[5] # fisher iterations
    residual.se <- round(sqrt(summary(reg.model)$dispersion[1]), digits)
    svyglm.summary.output[3] <- paste("Residual standard error:", residual.se, "on", reg.model$df.residual, "degrees of freedom")
    n.sample <- reg.model$df.null + 1
    if(!is.null(attr(reg.model$data, which="na.action")))  n.missing <- length(attr(reg.model$data, which="na.action"))
     else n.missing <- 0    
    svyglm.summary.output[4] <- paste("N: ", n.sample, " (", n.missing, " observations deleted due to missingness)", sep="")
    svyglm.summary.output[5] <- paste("Multiple R-squared: ",  fit.svyglm(reg.model)[1], ",  Adjusted R-squared: ", fit.svyglm(reg.model)[2], sep="")
    f.df.num <- reg.model$df.null - reg.model$df.residual
    f.df.denom <- reg.model$df.residual
    f.stat.num <- (reg.model$null.deviance - reg.model$deviance) / f.df.num
    f.stat.denom <- reg.model$deviance / f.df.denom
    f.stat <- f.stat.num / f.stat.denom
    f.stat.p.value <- round(pf(f.stat, df1=f.df.num, df2=f.df.denom, lower.tail=F), digits)
    if(f.stat.p.value < .001) f.stat.p.value <- "<.001"
    svyglm.summary.output[6] <- paste("F-statistic:", round(f.stat, digits), "on", f.df.num, "and", f.df.denom, "DF, p-value:", f.stat.p.value)
    svyglm.summary.output[7] <- paste("Dependent variable:", dv.name)
    svyglm.summary.output[8] <- paste("Observations weighted by:", w.name)
    cat("\n\n")
    cat(svyglm.summary.output, sep="\n")
    slightpause()
    class(svyglm.summary.output) <- "statement"
    if(printC==TRUE) printC(svyglm.summary.output)
  }
  cat("\n")
 
  # this works okay when weighted or svyglm used.
  # the print output with knitr::kable is incomplete, similar to summary output
  if(anova==TRUE) 
  {
    headingbox("ANOVA Table", marker="-")
    print(car::Anova(reg.model))
    cat("\n")
    if(printC==TRUE) printC(utils::capture.output(car::Anova(reg.model)))
    slightpause()
  }
  
  
  # needs a lot of reformatting to work with knitr::kable
  if(norm.test==TRUE) # working for lm and svyglm
  {
    heading.norm.test <- "Test of the Normality of Residuals"
    headingbox(heading.norm.test, marker="-")
    null.hypo.norm.test <- "Null Hypothesis: Residuals follow normal distribution."
    cat(null.hypo.norm.test, "\n\n")
    residuals <- reg.model$residuals
    res.skewness <- wtd.skewness(residuals)
    res.kurtosis <- wtd.kurtosis(residuals) - 3
    jb.stat <- length(residuals) * ((res.skewness^2)/6 + (res.kurtosis^2)/24)
    p.value <- stats::pchisq(jb.stat, df=2, lower.tail=FALSE)
    jb.test.results <- c(jb.stat, p.value)
    jb.test.name <- "        Jarque-Bera Test"
    cat(jb.test.name, "\n\n")
    names(jb.test.results) <- c("JB test stat.", "p-value")
    print(round(jb.test.results, digits))
    cat("\n")
    # print(suppressWarnings(ks.test(residuals, "pnorm")))
    # print(stats::shapiro.test(residuals)) 
    
    if(printC==TRUE)
    {
      norm.test.output <- c(heading.norm.test, "", null.hypo.norm.test, "", jb.test.name, "",
                            utils::capture.output(round(jb.test.results, digits))
                            # utils::capture.output(suppressWarnings(ks.test(residuals, "pnorm"))),
                            # utils::capture.output(stats::shapiro.test(residuals))
                            )
      # norm.test.output <- norm.test.output[-length(norm.test.output)]
      class(norm.test.output) <- "statement"
      printC(norm.test.output)
    }
    slightpause()
  }

  # this has text plus table, needs work for printC
  if(outlier.test==TRUE) 
  {
    outlier.test.heading <- "Bonferroni Outlier Test"
    headingbox(outlier.test.heading, marker="-")
    outlier.test.null.hypo <- "Null Hypothesis: Largest regression residual is not an outlier."
    cat(outlier.test.null.hypo, "\n\n")
    print(car::outlierTest(reg.model)) # works for lm and svyglm
    if(printC==TRUE)
    {
      outlier.test.output <- c(outlier.test.heading, "", outlier.test.null.hypo, "",
                            utils::capture.output(car::outlierTest(reg.model)))
      printC(outlier.test.output)
    }
    cat("\n")
    slightpause()
  }
  
  
  # this isn't printing for lm or glm, not sure why
  # formatting needed for printC
  if(ncv.test==TRUE)
  {
     ncv.test.heading = "Non-Constant Error Variance Test"
     headingbox(ncv.test.heading, marker="-")
     ncv.test.null.hypo <- "Null hypothesis: Residuals have constant variance."
     cat(ncv.test.null.hypo, "\n")
     # bptest is Breusch-Pagan test against heteroskedasticity.
     print(lmtest::bptest(reg.model))
     # car::ncvTest only works for lm and svyglm
     # hmctest is Harrison-McCabe test for heteroskedasticity.
     # print(lmtest::hmctest(reg.model))
     if(printC==TRUE)
     {
       ncv.test.output <- c(ncv.test.heading, "", ncv.test.null.hypo,
                            utils::capture.output(lmtest::bptest(reg.model))) 
                            # utils::capture.output(lmtest::hmctest(reg.model))
       ncv.test.output <- ncv.test.output[-length(ncv.test.output)]
       class(ncv.test.output) <- "statement"
       printC(ncv.test.output)
     }
     cat("\n")
     slightpause()
  }

  
  # you can only report VIF if there are 2+ IV
  # looks pretty straightforward for printC, just need to convert to labeled df
  if(vif==TRUE) 
  {
    n.iv <- length(reg.model$coefficients) - 1
    vif.heading <- "Variance Inflation Factors"
    headingbox(vif.heading, marker="-")
    if(n.iv > 1) 
    {
      vif.table <- data.frame(car::vif(reg.model))
      colnames(vif.table) <- c("VIF")
      print(knitr::kable(format(vif.table, drop0trailing=F, nsmall=digits, digits=digits), format="simple", caption=vif.heading, align="r"))
      if(printC==TRUE) printC(knitr::kable(format(vif.table, drop0trailing=F, nsmall=digits, digits=digits), format="html", align="r",
                                           caption=printCaption(vif.heading)))
    }  
    else
    {
      cat("Sorry, can only report VIF for regression models with two or more independent variables.\n") 
    }
    cat("\n")
    slightpause()
  }
  
  # would need formatting for printC
  if(reset.test==TRUE) 
  {
    reset.test.heading <- "Model Specification Test"
    headingbox(reset.test.heading, marker="-")
    reset.test.null.hypo <- "Null Hypothesis: Regression model is correctly specified."
    cat(reset.test.null.hypo, "\n\n")
    print(lmtest::resettest(reg.model)) # works for lm and svyglm
    cat("\n")
    if(printC==TRUE)
    {
      reset.test.output <- c(reset.test.heading, "", reset.test.null.hypo, "",
                               utils::capture.output(lmtest::resettest(reg.model)))
      printC(reset.test.output[-length(reset.test.output)])
    }
    slightpause()
  }
  
  # these tests have such different values, are they really testing same thing?
  # need  formatting work for printC to work
  if(linear.test==TRUE) 
  {
    linear.test.heading <- "Test of Linearity"
    headingbox(linear.test.heading, marker="-")
    linear.test.null.hypo <- "Null Hypothesis: The relationship in regression model is linear."
    cat(linear.test.null.hypo, "\n\n")
    print(lmtest::harvtest(reg.model)) # works for lm and svyglm?
    if(printC==TRUE)
    {
      linear.test.output <- c(linear.test.heading, "", linear.test.null.hypo,
                           utils::capture.output(lmtest::harvtest(reg.model)))
      printC(linear.test.output[-length(linear.test.output)])
    }
    cat("\n")
    slightpause()
  }
  
  if(res.plots==TRUE)
  {
    # this for loop lets plot be seen by user in R and then go to PNG driver
    for(k in 1:(1+as.numeric(printC)))
    {
      
    if(printC==TRUE & k==2) 
    {
      imagename <- paste("regC.plot.", unclass(Sys.time()), ".png", sep="")
      grDevices::png(filename=imagename, width=4, height=4, units="in", 
                     type=getPNGtype(), pointsize=8, res=300, antialias="default")
      class(imagename) <- "image"
      printC(imagename)
    }
    par.mfrow.restore <- graphics::par("mfrow")
    par.ask.restore <- graphics::par("ask")
    par.mar.restore <- graphics::par("mar")
    if(printC==TRUE & k==2) graphics::par(ask=FALSE) else graphics::par(ask=TRUE)
    
    graphics::par(mfrow=c(2,2), mar=c(4.5,4.5,2,1.5))
    plot(reg.model)
    graphics::par(mfrow=par.mfrow.restore, ask=par.ask.restore, mar=par.mar.restore)
    if(printC==TRUE & k==2) grDevices::dev.off()
    }
  }
  
  if(printC==T) printC(match.call(expand.dots = FALSE))

  invisible(reg.model) # return lm or svyglm object 
  # if there is more to return, need to make a list 
}
  
  