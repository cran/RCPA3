#' Logistic regression analysis with options for weighted observations, odds ratio reports, model fit statistics, and plots of residuals 
#'
#' @description Logistic regression analysis function with many useful features. Its standard output included a table of coefficients, table of deviance residuals, and summary of additional model information. Options include weighting observations, additional reports on odds ratios, ANOVA, multiple measures of model fit, proportional reduction in error, and diagnostic plots of residuals.
#' @param formula should be in dataset$dv ~ datatset$iv1 + dataset$iv2 unless dataset specified in optional data argument.
#' @param w (Optional) Sampling weights of variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains dv, iv (and w) variables.
#' @param digits (Optional) Number of decimal places reported in result (defaults to 2).
#' @param orci (Optional) Do you want table reporting odds ratios for coefficients with confidence intervals? (default: FALSE)
#' @param fit.stats (Optional) Do you want a table of assorted model fit statistics? (default: FALSE)
#' @param anova (Optional) Do you want ANOVA table reported? (default: FALSE)
#' @param pre (Optional) Do you want table reporting proportion reduction in error achieved by model? This is a Lambda-style measure of model fit. (default: FALSE)
#' @param printC (Optional) Do you want results printed to .html file in your working directory? Default is FALSE. Set to TRUE to print results.
#' @param res.plots (Optional) Do you want a set of diagnostic plots of model residuals? (default: FALSE)
#' @param ... (Optional) Additional arguments passed to \code{\link[stats]{glm}} function (unweighted models) or \code{\link[survey]{svyglm}} function (weighted models).
#' @return Returns a glm (unweighted models) or svyglm (weighted models) object.
#' @examples 
#'   library(RCPA3)
#'   
#'   \donttest{
#'   # basic usage with variable vectors
#'   logregC(states$battleground2020 ~ states$vep16.turnout)
#'   
#'   # with post-estimation analysis
#'   logregC(states$battleground2020 ~ states$vep16.turnout, orci=TRUE, fit.stats=TRUE, 
#'           anova=TRUE, pre=TRUE, res.plots=TRUE)
#'   }
#' @export
#' @section RCPA3 Package Tutorial Videos:
#' * [Logistic Regression Analysis with RCPA3 Package's logregC Function](https://youtu.be/tbFzTNtU8aw) 13:36   
#' * [Complete Playlist of RCPA3 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook References:
#' * Philip H. Pollock and Barry C. Edwards, _An R Companion to Political Analysis, 3rd Edition_ (Thousand Oaks, CA: Sage Publications, Forthcoming 2022), Chapter 14. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Thousand Oaks, CA: Sage Publications, 2020), Chapter 9. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Logistic Regression](https://www.poliscidata.com/pages/rDemosResources.php?chapter=14), compiled by Barry C. Edwards
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications. 
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom methods is
#' @importFrom survey svydesign svyglm
#' @importFrom grDevices png dev.off
#' @importFrom descr crosstab
#' @importFrom knitr kable
#' @importFrom car Anova
#' @importFrom graphics par
#' @importFrom stats as.formula pchisq glm
#' @md


logregC <- function(formula, w, data, digits=3, orci=FALSE, fit.stats=FALSE, 
                    anova=FALSE, pre=FALSE, printC=FALSE, res.plots=FALSE, ...) 
{
  
  if (missing(formula) || (length(formula) != 3)) stop("There's a problem: Your formula is missing or incorrect. To see how to use this function, try example(logregC) or help(logregC).")
  if(!methods::is(formula, class2="formula")) stop("There's a problem: The logregC function works with a formula in the form dv ~ iv.")
  check.value(digits, valuetype="numeric")
  
  if(res.plots==TRUE)
  {
    old.par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old.par))
  }
  
  if(!missing(w)) w.name <- deparse(substitute(w))
  
  if(!missing(data))
  {
    if(is.matrix(data)) data <- data.frame(data)
    if(!missing(w)) w <- data$w <- vector.from.data(substitute(w), data)
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
  
  if(weighted==FALSE)  logit.model <- stats::glm(formula, data=data, family="binomial", na.action="na.omit", ...)

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
    # return(list(colnames(dataframe), formula, svy.formula, class(formula)))
    logit.model <- survey::svyglm(formula=svy.formula, design=svy.design, 
                                  na.action="na.omit", family="quasibinomial", ...)
    svy.design <- NULL
    dataframe <- NULL
  }
  
  main.heading <- headingbox("Logistic Regression Analysis", marker="=")
  if(printC==TRUE) printC(main.heading)
  
  dv.name <- as.character(formula)[[2]]

  coef.table <- round(summary(logit.model)$coefficients, digits)
  colnames(coef.table)[3:4] <- c("z stat", "Pr(>|z|)")
  coef.table[coef.table[,4]==0,4] <- "<.001"
  coef.table.caption <- "Logistic Regression Coefficients"
  print(knitr::kable(format(coef.table, drop0trailing=F, nsmall=digits, digits=digits), format="simple", caption=coef.table.caption, align="r"))
  slightpause()
  if(printC==TRUE) printC(knitr::kable(coef.table, format="html", caption=printCaption(coef.table.caption), digits=3, escape=F, align="r"))
  
  res.table.caption <- "Deviance Residuals"
  dev.res.table <- t(data.matrix((summary(summary(logit.model)$deviance.resid))[-4]))
  print(knitr::kable(format(dev.res.table, drop0trailing=F, nsmall=digits, digits=digits), format="simple", caption=res.table.caption, align="r"))
  slightpause()
  if(printC==TRUE) printC(knitr::kable(dev.res.table, format="html", caption=printCaption(res.table.caption), digits=3))
  
  if(weighted==FALSE) 
  {
    
    if(is.null(logit.model$na.action)) lines.capture <- 9 else lines.capture <- 10
    glm.summary.output <- utils::capture.output(summary(logit.model))
    glm.summary.output <- glm.summary.output[(length(glm.summary.output)-lines.capture):length(glm.summary.output)]
    glm.summary.output[1] <- paste("Additional Information:")
    n.sample <- logit.model$df.null + 1
    if(is.null(logit.model$na.action)) glm.summary.output[8] <- paste("N:", n.sample, "(0 observations deleted due to missingness)")
      else glm.summary.output[7] <- paste("N:", n.sample, glm.summary.output[7])
    glm.summary.output[5] <- trimws(glm.summary.output[5])
    glm.summary.output[length(glm.summary.output)] <- paste("Dependent variable:", dv.name)
    cat("\n\n")
    cat(glm.summary.output, sep="\n")
    slightpause()
    class(glm.summary.output) <- "statement"
    if(printC==TRUE) printC(glm.summary.output)
  }
  
  if(weighted==TRUE) 
  {
    svyglm.summary.output <- utils::capture.output(summary(logit.model))
    svyglm.summary.output <- svyglm.summary.output[(length(svyglm.summary.output)-5):length(svyglm.summary.output)]
    svyglm.summary.output[1] <- "Additional Information:"
    n.sample <- logit.model$df.null + 1
    if(is.null(attr(logit.model$data, which="na.action"))) svyglm.summary.output[4] <- paste("N:", n.sample, "(0 observations deleted due to missingness)")
      else svyglm.summary.output[4] <- paste("N: ", n.sample, " (", length(attr(logit.model$data, which="na.action")), " observations deleted due to missingness)", sep="")
    svyglm.summary.output[length(svyglm.summary.output)] <- paste("Dependent variable:", dv.name)
    svyglm.summary.output[length(svyglm.summary.output)+1] <- paste("Observations weighted by:", w.name)
    cat("\n\n")
    cat(svyglm.summary.output, sep="\n")
    slightpause()
    class(svyglm.summary.output) <- "statement"
    if(printC==TRUE) printC(svyglm.summary.output)
  }
  cat("\n")
  slightpause()  
  # print(summary(logit.model))
  
  
  # will this work okay when weighted and svyglm used?
  # when weighted, will fit.svyglm give pseudo-R2 results?
  if(fit.stats==TRUE) 
  {
    fit.stats.heading <- "Model Fit Statistics"
    headingbox(fit.stats.heading, marker="-")
    fitstats <- logregR2(logit.model)
    print(knitr::kable(fitstats, digits=digits, format="simple", caption=fit.stats.heading, align="r"))
    if(printC==TRUE) printC(knitr::kable(fitstats, digits=digits, format="html", 
                                         caption=printCaption(fit.stats.heading), align="r"))
    cat("\n")
    slightpause()
  }

  # will this work okay when weighted and svyglm used?
  # putting orci last because it seems a little slow maybe...
  if(orci==TRUE) 
  {
    orci.heading <- "Odds Ratios for Logistic Regression Coefficients"
    headingbox(orci.heading, marker="-")
    oddsratios <- orci(logit.model, digits)
    print(knitr::kable(oddsratios, digits=digits, format="simple", caption=orci.heading, align="r"))
    if(printC==TRUE) printC(knitr::kable(oddsratios, digits=digits, format="html", 
                                         caption=printCaption(orci.heading), align="r"))
    cat("\n")
    slightpause()
  }
  
  # will this work okay when weighted and svyglm used?
  # orci seems a little slow, but there's nothing exotic in it
  # the print output with knitr::kable is incomplete, similar to summary output
  if(anova==TRUE) 
  {
    anova.heading <- "ANOVA Table"
    headingbox(anova.heading, marker="-")
    cat("\n")
    print(car::Anova(logit.model))
    cat("\n")
    if(printC==TRUE) printC(utils::capture.output(car::Anova(logit.model)))
    slightpause()
  }
  
  
  if(pre==TRUE)
  {
    pre.heading <- "Classification Table and Proportional Reduction in Error"
    headingbox(pre.heading, marker="-")
    predicted.outcome <- as.numeric(logit.model$fitted.values >= .5)
    observed.outcome  <- logit.model$y
    if(missing(w)) w <- rep(1, length(predicted.outcome))
    else w <- logit.model$prior.weights
    classtab <- descr::crosstab(dep=observed.outcome, indep=predicted.outcome, 
                                dnn=c("Observed Outcomes", "Predicted Outcomes"),
                                weight=w, plot=F, cell.layout=FALSE)
    # classtab is CrossTable class object, not work with knitr::kable
    classtab.output <- c("Classification Table",
                         utils::capture.output(print(classtab)))
    cat("\n")
    cat(classtab.output, sep="\n") 
    if(printC==TRUE) printC(classtab.output) # works okay with printC
    
    if(ncol(classtab$tab)==1) message("Predicted outcome doesn't change. Lambda will be 0.")
    
    lambda.caption <- "Lambda Measure of Association"
    lambda.df <- lambda(classtab$tab, digits, detailed=TRUE)
    print(knitr::kable(lambda.df, digits=digits, format="simple", 
                       caption=lambda.caption, align="r"))
    if(printC==TRUE) printC(knitr::kable(lambda.df, digits=digits, format="html", 
                                         caption=printCaption(lambda.caption), align="r"))
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
      imagename <- paste("logregC.plot.", unclass(Sys.time()), ".png", sep="")
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
    plot(logit.model)
    graphics::par(mfrow=par.mfrow.restore, ask=par.ask.restore, mar=par.mar.restore)
    if(printC==TRUE & k==2) grDevices::dev.off()
    }
  }
  
  if(printC==T) printC(match.call(expand.dots = FALSE))

  invisible(logit.model)
  # if there is more to return, need to make a list
}
  
  