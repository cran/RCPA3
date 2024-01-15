##' @name helper.functions
##' @noRd
##' @title Functions that help user-facing functions but not called directly.

#' Check value and generate custom message about problems
#' @param msg Prompt to show user
#' @param con Connect (default: standard console input)
#' @noRd
#' @keywords internal
ask <- function(msg = "Press <Enter> to continue: ", con = stdin()) 
{
  cat(msg)
  readLines(con = con, n = 1)
}

#' Check value and generate custom message about problems
#' @param x Value to be checked (must be length 1)
#' @param valuetype Can check if "numeric" (default: NULL)
#' @noRd
#' @importFrom Hmisc all.is.numeric
#' @keywords internal
# multiple purpose value checker, use check.value for single values
check.value <- function(x, valuetype=NULL)
{
  x.name = deparse(substitute(x))
  if(length(x)==0) stop(paste(gettext("There's a problem:", domain = "R-descr"), x.name, gettext("has no value or can't be found. Check your spelling?", domain = "R-descr")))
  if(length(x) > 1) stop(paste(gettext("There's a problem:", domain = "R-descr"), x.name, gettext("has multiple values. A single value was expected. Check your work?", domain = "R-descr")))
  
  if(!missing(valuetype)) 
  {
    if(valuetype == "numeric") 
    {
      if(Hmisc::all.is.numeric(x, extras=c(NA)) | is.logical(x)) x = as.numeric(x)
      if(!is.numeric(x)) stop(paste(gettext("There's a problem:", domain = "R-descr"), x.name, gettext("is not a numeric value Check your work?", domain = "R-descr")))
    } 
  }
}



#' Check variable and generate custom message about problems
#' @param x A vector of values
#' @param vartype Can check if "numeric" (default: NULL)
#' @param limitedvalues Check if length of x is less than this number (default: NULL)
#' @noRd
#' @importFrom stats na.omit
#' @importFrom Hmisc all.is.numeric
#' @keywords internal
# multiple purpose variable checker, use check.value for single values
check.variable <- function(x, vartype=NULL, limitedvalues=NULL)
{
  x.name = deparse(substitute(x))
  # print(list(x.name, head(x), length(x)))
  if(length(x)==0) stop(paste(gettext("There's a problem:", domain = "R-descr"), x.name, gettext("has no values or can't be found. Check your spelling?", domain = "R-descr")))
  if(length(x)==1) stop(paste(gettext("There's a problem:", domain = "R-descr"), x.name, gettext("has a single value. A vector of variable values was expected. Check your spelling?", domain = "R-descr")))
  
  if(!missing(limitedvalues)) 
  {
    if(length(unique(stats::na.omit(x))) > limitedvalues) stop(paste(gettext("There's a problem:", domain = "R-descr"), x.name, gettext("has too many unique values. Use another function instead?", domain = "R-descr")))
  }
  
  if(!missing(vartype)) 
  {
    if(vartype == "numeric") 
    {
      if(Hmisc::all.is.numeric(x, extras=c(NA)) | is.logical(x)) x = as.numeric(x)
      # print(head(x))
      # stop()
      if(!is.numeric(x)) stop(paste(gettext("There's a problem:", domain = "R-descr"), x.name, gettext("is not a numeric variable. Check your spelling?", domain = "R-descr")))
    } 
  }
}




#' Calculates Cramer's V
#' @param chi A number equal to the Chi-Squared statistic
#' @param r A number equal to the number of rows
#' @param c A number equal to the number of columns
#' @param n A number equal to the sample size
#' @return The Cramer's V statistic, a number between 0 and 1.
#' @description Calculates Cramer's V, a measure of association to gauge the strength of the relationship between two nominal-level variables.  
#'   A score of 0 indicates no relationship; a score of 1 indicates a perfect relationship.
#' @examples 
#'   library(RCPA3)
#'   
#'   CramersV(84.18, 2, 2, 1315)
#'   CramersV(chi=84.18, r=2, c=2, n=1315)
#' @export
#' @keywords internal
#'
CramersV = function(chi, r, c, n)
{
  r1 = r-1
  c1 = c-1
  minr1c1 = min(r1, c1)
  denom = n * minr1c1
  V = sqrt(chi/denom)
  return(V)
}



#' Output dashed line to console to organize results
#' @param width Numeric value, length of dashed line (default: 60) 
#' @param marker Character to use as dash (default: "-")
#' @noRd
#' @keywords internal
dashedline <- function(width, marker="-")
{
  if(missing(width)) width <- 60
  # if(missing(marker)) marker <- "-"
  for(j in 1:width) cat(marker)
  cat("\n") 
}


#' Calculates model fit statistics for a svyglm linear regression model
#' @param svyglm An object of type svyglm. This object is the output of the svyglm function as well as regC function with w argument.
#' @param digits Number of digits to report after decimal place, optional (default = 3).
#' @return Returns a numeric vector of R-Squared and Adjusted R-sSquared statistics.
#' @description Model fit statistics for a svyglm weighted linear regression model (not for weighted logistic regression).
#  With weighted logit, McFadden's R2 is same as R-Squared from this function and adj-R2 is not useful
#' @examples 
#'    library(RCPA3)
#'    
#'    \donttest{
#'    ft.police.model <- regC(ft.police ~ race.ethnicity, w=wt, data=nes)
#'    fit.svyglm(ft.police.model)
#'    }
#' @export
#' @importFrom methods is
#' @keywords internal
fit.svyglm = function(svyglm, digits=3)
{
  if(missing(svyglm)) stop("There's a problem: You need to specify the svyglm results to analyze. Enter example(fit.svyglm) to see an example and help(fit.svyglm) to learn more about this function.")
  if(methods::is(svyglm, "svyglm")!=TRUE) message("Warning: Not a svyglm object.")
  
  r2 = (svyglm$null.deviance - svyglm$deviance) / svyglm$null.deviance 
  adjust = svyglm$df.null / svyglm$df.residual
  value  = 1 - ((1-r2) * adjust)
  results = c(R2=round(r2, digits), adjR2=round(value, digits))
  names(results) = c("R-Squared","     Adjusted R-Squared")
  return(results)
}

#' Get and return user's operating system type ("mac", "windows", or "linux")
#' @noRd
#' @keywords internal
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "mac"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "mac"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

#' Get type to use for PNG graphics based on user's cairo support
#' @noRd
#' @keywords internal
getPNGtype <- function()
{
  if((suppressWarnings(capabilities()["cairo"]))==TRUE) return("cairo")
  else return(NULL)
}

#' Generate heading box to organize Console output
#' @param text String of text to enclose in a heading box
#' @param width Numeric value, length of dashed line (default: 75) 
#' @param marker Character to use as dash (default: "=")
#' @noRd
#' @keywords internal
headingbox <- function(text, width, marker="=")
{
  # if the text > width, need to adjust width
  if(missing(width)) width <- 75
  text <- as.character(text)
  textchar <- nchar(text)
  leadchar <- floor((width-textchar) / 2) 
  if(leadchar < 0) leadchar <- 0
  border <- paste(rep(marker, width), collapse = "")
  leadspace <- paste(rep(" ", leadchar), collapse = "")
  heading <- c(paste(border, "\n", sep=""),
               paste(leadspace, text, "\n", sep=""),
               paste(border, "\n", sep=""))
  cat(heading, sep="")
  Sys.sleep(1) # built in delay to help user see output
  class(heading) <- "banner.heading"
  return(heading)
}


#' Converts linear predictor to probability 
#' @param logged_odds A numeric value, or vector of numeric values.
#' @return Returns predicted probability corresponding the the logged odds value, a numeric value between 0 and 1.
#' @description Calculates predicted probability for a given logged odds value. This function calculates predicted probability for a given logged odds value, often useful for plotting or reporting predicted probabilities.
#' @examples 
#'    library(RCPA3)
#'    
#'    inverse.logit(0)
#'    inverse.logit(-5:5)
#' @export
#' @keywords internal
inverse.logit = function(logged_odds)
{
  # check if user supplied a numeric
  if(length(logged_odds)==1) check.value(logged_odds, valuetype="numeric")
  if(length(logged_odds)> 1) check.variable(logged_odds, vartype="numeric")
  probability = exp(logged_odds) / (1 + exp(logged_odds))
  return(probability)
}



#' Calculates Lambda, a measure of association for a cross-tabulation. 
#' @param tab The conditional frequencies of a cross-tabulation. The dependent variable should correspond to rows and the independent variable, to columns.
#' @param digits Number of decimal places of Lambda reported (defaults to 3).
#' @param detailed Do you want detailed report on how lambda value calculated? (default is FALSE).
#' @return Returns the value of Lambda.
#' @description Given a table of conditional frequencies, the lambda function calculates and returns Lambda, a measure of association between two categorical variables (factors). Lambda is equal to the proportion of errors reduced by knowledge of the independent variable.
#' @examples 
#'   library(RCPA3)
#'   
#'   conditional.frequencies <- table(dv=states$lgbtq.equality.3cat, iv=states$religiosity3)
#'   lambda(tab=conditional.frequencies, detailed=TRUE)
#' @export
#' @keywords internal
#'
lambda = function(tab, digits=3, detailed=FALSE)
{
  if(missing(tab))  stop("Opps. You need to specify the table of values to be analyzed using this function's tab argument. To see how to use this function, try example(lambda) or help(lambda).")
  tab.name <- deparse(substitute(tab))
  if(!is.table(tab)) tab <- as.table(tab)
  
  # print(tab)
  if((nrow(tab) <= 1)) stop(paste(gettext("There's a problem:", domain = "R-descr"), tab.name, gettext("doesn't appear to be a table.", domain = "R-descr")))
  if(sum(tab[-nrow(tab), -ncol(tab)]) == tab[nrow(tab), ncol(tab)]) message(paste("There may be a problem:", tab.name, "appears to include margin totals; it should only contain conditional distributions. Try omitting last row and last column."))
  # return(list(sum(tab[-nrow(tab), -ncol(tab)]), tab[nrow(tab), ncol(tab)]))
  n.total <- sum(tab)
  row.sums <- rowSums(tab)
  naive.errors <- n.total - max(row.sums)
  n.columns <- ncol(tab)
  prediction.errors <- 0
  for(i in 1:n.columns)
  {
    prediction.errors <- prediction.errors + sum(tab[,i]) - max(tab[,i])
    # print(prediction.errors)
  }
  lambda = (naive.errors - prediction.errors) / naive.errors

  lambda.df    <- data.frame(naive.errors=naive.errors, prediction.errors=prediction.errors, lambda=lambda)
  colnames(lambda.df) <- c("Naive Errors", "Prediction Errors", "Lambda")
  rownames(lambda.df) <- NULL
  # names(lambda) = c("Lambda")
  # cat("Hello from lambda function!\n")
  if(detailed==FALSE) 
  {
    lambda <- round(lambda.df[ , 3], digits)
    names(lambda) <- "Lambda"
    return(lambda)
  }
  # names(naive.errors) = c("  Naive Errors")
  # names(prediction.errors) = c("  Model Errors")
  if(detailed==TRUE) return(round(lambda.df, digits))
}



#' Logistic regression model fit statistics
#' @param model An estimated logistic regression model (a glm or svyglm object)
#' @param digits Number of digits to be displayed after decimal points
#' @return Returns list of statistics about model (a "LogRegR2" class object)
#' @description Logistic regression model statistics, reported by logregC with fit.stats=TRUE argument, can be used for both weighted and unweighted logregC models.
#' @examples 
#'    library(RCPA3)
#'    
#'    \donttest{
#'    logit.model <- logregC(states$battleground2020 ~ states$vep16.turnout)
#'    logregR2(logit.model)
#'    }
#' @export
#' @importFrom methods is
#' @importFrom stats pchisq
#' @keywords internal
#'
logregR2 = function(model, digits=3)
{
  if(missing(model))  stop("Opps. You need to specify the glm object to be analyzed using the model argument. To see how to use this function, try example(logregR2) or help(logregR2).")
  # check if user supplied a glm
  if(methods::is(model, "glm")!=TRUE) message("Warning: Model is not a glm object.")
  n    <- dim(model$model)[1]
  Chi2 <- round(model$null - model$dev, digits)
  Df   <- model$df.null - model$df.res
  p    <- round(1 - stats::pchisq(Chi2, Df), digits)
  if (p==0) { p = "<.001"}
  Cox  <- round(1 - exp(-Chi2/n), digits)
  Nag  <- round(Cox/(1 - exp(-model$null/n)), digits)
  RL2  <- round(Chi2/model$null, digits)
  fit.stats.df    <- data.frame(Statistic= c("Chi2", "DF (for Chi2)", "p-Value of Chi2", "McFadden's R2", "Cox & Snell Index", "Nagelkerke Index"), 
                                Value=c(Chi2, Df, p, RL2, Cox, Nag))
  # colnames(fit.stats.df) 
  # class(x) <- "LogRegR2"
  return(fit.stats.df)
}



#' Translate logistic regression coefficients into odds ratios with confidence interval
#' @param model An estimated logistic regression model
#' @param digits Number of digits after decimal to display
#' @return Returns odds rations and confidence intervals in columns (a matrix class object)
#' @description Generates odds-ratios based on logistic regression model coefficients, reported by logregC with orci=TRUE argument
#' @examples 
#'    library(RCPA3)
#'    
#'    \donttest{
#'    logit.model <- logregC(states$battleground2020 ~ states$vep16.turnout)
#'    orci(logit.model)
#'    }
#' @export
#' @keywords internal
#' @importFrom methods is
#' @importFrom stats coef
#'
orci = function(model, digits=3)
{
  if(missing(model))  stop("Opps. You need to specify the model to be analyzed using the model argument. To see how to use this function, try example(orci) or help(orci).")
  # check if user supplied a glm
  if(methods::is(model, "glm")!=TRUE) message("Warning: Model is not a glm object.")
  results = exp(cbind(OddsRatio=stats::coef(model), stats::confint(model)))
  percent.change <- (exp(stats::coef(model)) -1) * 100
  
  results <- data.frame(results, percent.change)
  colnames(results) = c("Odds Ratio", "OR CI 2.5%", "OR CI 97.5%", "% Change in Odds")
  return(round(results[-1, ], digits))
}


#' Print caption heading to .html file to organize page
#' @param text String of text to be printed in caption
#' @noRd
#' @keywords internal
printCaption <- function(text)
{
  text <- paste("Table:", text)
  # text <- paste(strwrap(text, width=90),"<BR>", sep="")
  newcaption <- paste("<h3 style=\"color: #404040;\">" , paste(text, collapse = ""), "</h3>", sep="")
  # newcaption <- paste("<nobr><h3 style=\"color: #404040;\">", text, "</h3></nobr>", sep="")
  return(newcaption)
}



#' Prints string to console quickly
#' @param linetoprint Text to be printed to console
#' @return No value returned
#' @description Prints string to console quickly, but still scolls for visual effect
#' @noRd
#' @keywords internal
#' @importFrom utils flush.console
#' 
quickConsolePrint = function(linetoprint)
{
  line = strsplit(linetoprint, split="")
  linetoprint = line[[1]]
  for(i in 1:length(linetoprint))
  {
    Sys.sleep(.006)
    cat(linetoprint[i])
    utils::flush.console()
  }
}


#' Pauses R briefly for user to see scrolling Console output 
#' @description Slight pauses help user see Console output before it scrolls by. 
#' @param second Number of seconds to pause (default: 2)
#' @noRd
#' @keywords internal
slightpause = function(seconds=2)
{
  Sys.sleep(seconds)
}


#' Prints string to console slowly
#' @param linetoprint Text to be printed to console
#' @param slow Time, in milliseconds, to wait between characters printed to console (default is .05).
#' @return No value returned
#' @description Prints string to console slowly to make text more readable to the user. Used by welcome and widgetFactory functions.
#' @noRd
#' @keywords internal
#' @importFrom utils flush.console
#'
slowConsolePrint = function(linetoprint, slow=.05)
{
  line = strsplit(linetoprint, split="")
  linetoprint = line[[1]]
  for(i in 1:length(linetoprint))
  {
    Sys.sleep(slow)
    cat(linetoprint[i])
    utils::flush.console()
  }
}




#' Calculates the Somers' d measure of association for a cross-tabulation. 
#'
#' @param tab The conditional frequencies of a cross-tabulation. The dependent and independent variable in cross-tabulation should be be ordinal-level (ordered factors).
#' @param dep	which dimension stands for the dependent variable (1 = ROWS, 2 = COLS), default is 2.
#' @param digits Number of decimal places reported in result (defaults to 3).
#' @return Returns the value of Somers' D.
#' @description Given a table of conditional frequencies, tablesomersDC calculates and returns Somers' D, a measure of association between two ordinal-level variables.
#' @examples 
#'   library(RCPA3)
#'   
#'   conditional.frequencies <- table(dv=states$lgbtq.equality.3cat, iv=states$religiosity3)
#'   tablesomersDC(tab=conditional.frequencies)
#' @export
#' @keywords internal
tablesomersDC = function(tab, dep=1, digits=3) 
{ 
  
  if(missing(tab))  stop("Opps. You need to specify the table of values to be analyzed using this function's tab argument. To see how to use this function, try example(lambda) or help(lambda).")
  tab.name <- deparse(substitute(tab))
  if(!is.table(tab)) tab <- as.table(tab)
  if((nrow(tab) <= 1) || (ncol(tab) <= 1)) stop(paste(gettext("There's a problem:", domain = "R-descr"), tab.name, gettext("doesn't appear to be a table.", domain = "R-descr")))
  if(sum(tab[-nrow(tab), -ncol(tab)]) == tab[nrow(tab), ncol(tab)]) message(paste("There may be a problem:", tab.name, "appears to include margin totals; it should only contain conditional distributions. Try omitting last row and last column."))
  
  compADPQ = function(tab)
  {
    nr = nrow(tab)
    nc = ncol(tab)
    Aij = matrix(0, nrow=nr, ncol=nc)
    Dij = matrix(0, nrow=nr, ncol=nc)
    for (i in 1:nr){
      for (j in 1:nc){
        
        Aij[i,j] = sum(tab[1:i,1:j]) + sum(tab[i:nr,j:nc]) - sum(tab[i,]) - sum(tab[,j])
        Dij[i,j] = sum(tab[i:nr,1:j]) + sum(tab[1:i,j:nc]) - sum(tab[i,]) - sum(tab[,j])
      }
    }
    P = sum(tab*Aij)
    Q = sum(tab*Dij)
    return(list(Aij=Aij, Dij=Dij, P=P, Q=Q))
  }
  
  # Statistic 
  if (dep==1) tab=t(tab) 
  tmp = compADPQ(tab) 
  P   = tmp$P 
  Q   = tmp$Q 
  n   = sum(tab) 
  w   = n^2 - sum(apply(tab,1,sum)^2) 
  somers = (P-Q)/w 
  out = c(round(somers, digits))
  names(out) = c("Somers' dyx")
  # cat("     Somers'dyx:\n")
  return(out) 
}


#' Returns vector by applying expression to named dataset
#' @param expression An expression to be evaluated, typically the name of a variable
#' @param dataset The name of a dataset/dataframe object
#' @noRd
#' @keywords internal
#' @importFrom Hmisc all.is.numeric
vector.from.data <- function(expression, dataset)
{
  x <- eval(expression, dataset)
  data.name = deparse(substitute(dataset))
  if(length(x)==0) stop(paste(gettext("There's a problem: can't find", domain = "R-descr"), expression, gettext("in", domain = "R-descr"), data.name, gettext(". Check your spelling?", domain = "R-descr")))
  if("ordered" %in% class(x)) return(x)
  else if(Hmisc::all.is.numeric(x, extras=c(NA))) x = as.numeric(x)
  return(x)
}


#' Calculates Correlation Coefficient Between Two Numeric Variables
#' Makes use of the wtd.cor function, part of the weights package.
#' @param x1 A variable (must be numeric), should be in dataset$var form unless dataset specified in optional data argument.
#' @param x2 Another variable, different than x1 (must be numeric), should be in dataset$var form unless dataset specified in optional data argument.
#' @param w (Optional) Sampling weights of variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains x (and w) variable.
#' @param digits (Optional) Number of decimal places reported in result (defaults: 3).
#' @param stats (Optional) Do you want inferential statistics for correlation coefficient? (default: FALSE)
#' @param ... (Optional) Additional arguments passed to weights::wtd.cor function.
#' @return Returns the coefficient of correlation between x1 and x2 variables, a numeric value.
#' @description Given two numeric variables, wtd.cor reports correlation coefficient, works with sampling weights. 
#' The \code{\link[weights]{wtd.cor}} function is imported from the weights package. See \code{\link[weights]{wtd.cor}} documentation for details.
#' @examples 
#'    library(RCPA3)
#'    
#'    wtd.cor(x1=nes$ft.rep, x2=nes$ft.pence.pre, w=nes$wt)
#'    wtd.cor(x1=ft.dem, x2=ft.harris.pre, w=wt, data=nes)
#' @export
#' @keywords internal
#' @importFrom weights wtd.cor
wtd.cor <- function(x1, x2, w=NULL, data, digits=3, stats=FALSE, ...) 
{ 
  if(missing(x1) && missing(x2)) stop("Oops. You need to identify the variables to be analyzed using this function's x1 and x2 arguments. Enter example(wtd.cor) to see an example and help(wtd.cor) to learn more about this function.")
  
  x1.name = deparse(substitute(x1))
  x2.name = deparse(substitute(x2))
  
  if(!missing(data))
  {
    if(is.matrix(data)) data <- data.frame(data)
    x1 <- vector.from.data(substitute(x1), data)
    x2 <- vector.from.data(substitute(x2), data)
    if(!missing(w))  w <- vector.from.data(substitute(w), data)
  }
  
  check.variable(x1, vartype="numeric")
  check.variable(x2, vartype="numeric")
  if(!missing(w)) check.variable(w, vartype="numeric")
  
  result <- weights::wtd.cor(x=x1, y=x2, weight=w, ...) 
  # return(attributes(result))
  if(stats==FALSE) return(round(result[,"correlation"], digits))
  if(stats==TRUE) return(round(result, digits))
}




#' Creates a Histogram Showing Distribution of Variable Values With Option for Weights
#' Makes use of the wtd.hist function, part of the weights package.
#' @param x A variable (must be numeric), should be in dataset$var form unless dataset specified in optional data argument.
#' @param w Sampling weights of variable (optional), must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains x (and w) variable.
#' @param breaks (Optional) Specifies the breakpoints between bins of the histogram. See \code{\link[weights]{wtd.hist}} documentation for details. 
#' @param ... (Optional) Additional arguments passed to weights::wtd.hist function.
#' @return Generate a histogram plot.
#' @description Takes in variable and generates histogram plot to show the distribution of its values, works with sampling weights.
#' The wtd.hist function is imported from the weights package. See \code{\link[Hmisc]{wtd.stats}} documentation for details.
#' @examples 
#'    library(RCPA3)
#'    
#'    wtd.hist(x=nes$age, w=nes$wt)
#' @export
#' @keywords internal
#' @importFrom weights wtd.hist
#' @importFrom Hmisc all.is.numeric
wtd.hist <- function(x, w=NULL, data, breaks, ...) 
{ 
  if(missing(x)) stop("Oops. You need to identify the variable to be analyzed using this function's x argument. Enter example(wtd.hist) to see an example and help(wtd.hist) to learn more about this function.")
  x.name = deparse(substitute(x))
  
  if(!missing(data))
  {
    if(is.matrix(data)) data <- data.frame(data)
    x <- vector.from.data(substitute(x), data)
    if(!missing(w)) w <- vector.from.data(substitute(w), data)
  }
  
  check.variable(x, vartype="numeric")
  if(length(unique(x)) <= 10) 
  {
    msg <- paste(gettext("Suggestion:", domain = "R-descr"), x.name, 
                 gettext("has only", domain = "R-descr"), length(unique(x)), 
                 gettext("unique values. Maybe you want to use a bar chart?", domain = "R-descr") )
    message(msg)
  }
  if(!missing(w)) check.variable(w, vartype="numeric")
  weights::wtd.hist(x=x, weight=w, main=paste("Histogram of", x.name), xlab=paste(x.name, "Values"), ...) 
}



#' Generates Kurtosis Statistic with Option to Weight Observations
#' @param x Variable (a numeric vector)
#' @param w (Optional) Weights variable
#' @param data (Optional) Dataset, can be used to abbreviate x to variable name
#' @param digits (Optional) Number of digits after decimal point (default: 3)
#' @return Returns the kurtosis of the variable, a numeric value.
#' @examples 
#'    library(RCPA3)
#'    
#'    wtd.kurtosis(x=nes$age, w=nes$wt)
#'    wtd.kurtosis(x=nes$age)
#' @export
#' @keywords internal
#' @importFrom stats weighted.mean
wtd.kurtosis <- function(x, w, data, digits=3) {
  
  if(missing(x)) stop("Oops. You need to identify the variable to be analyzed using this function's x argument. Enter example(wtd.kurtosis) to see an example and help(wtd.kurtosis) to learn more about this function.")
  
  if(!missing(data))
  {
    if(is.matrix(data)) data <- data.frame(data)
    x <- vector.from.data(substitute(x), data)
    if(!missing(w)) w <- vector.from.data(substitute(w), data)
  }
  
  if (missing(w)) { # if no w given, then w=1 always
    w <- rep(1, length(x))
  }
  else {
    if (length(x) != length(w)) {
      msg <- paste(deparse(substitute(x)), gettext("and", domain = "R-descr"), deparse(substitute(w)), gettext("have different lengths.", domain = "R-descr"))
      stop(msg)
    }
  }
  if (is.numeric(w)) {
    class(w) <- "numeric"
  }
  k <- grep(FALSE, (is.na(x) | is.na(w))) # checks for missing values
  x <- x[k]
  w <- w[k]
  n <- length(x)
  w.n <- sum(w) # equals n if w=1
  x.mean <- stats::weighted.mean(x, w)
  # w.sd  <- sqrt(Hmisc::wtd.var(x=x, weights=w)) # weighted 
  # z.dev <- (x - x.mean) / w.sd
  wtd.kurtosis <- w.n * sum(w*(x - x.mean)^4)/(sum((x - x.mean)^2)^2) # same as moments::kurtosis
  class(wtd.kurtosis) <- "numeric"
  round(wtd.kurtosis, digits)
}



#' Calculates Mean of Variable With Option for Weights
#' @param x A variable (must be numeric), should be in dataset$var form unless dataset specified in optional data argument.
#' @param w (Optional) Sampling weights of variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains x (and w) variable.
#' @param digits (Optional) Number of decimal places reported in result (defaults to 3).
#' @param ... (Optional) Additional arguments passed to Hmisc::wtd.mean function.
#' @return Returns the mean of the variable, a numeric value.
#' @description Takes in variable and calculates its mean, works with sampling weights.
#' Makes use of the wtd.mean function, part of the Hmisc package. See \code{\link[Hmisc]{wtd.stats}} documentation for details.
#' @examples 
#'    library(RCPA3)
#'    
#'    wtd.mean(nes$ft.socialists, w=nes$wt)
#'    wtd.mean(nes$ft.socialists)
#'    
#'    wtd.mean(nes$age, w=nes$wt)
#'    wtd.mean(nes$age)
#' @export
#' @keywords internal
#' @importFrom Hmisc wtd.mean
wtd.mean = function(x, w=NULL, data, digits=3, ...) 
{
  if(missing(x)) stop("Oops. You need to identify the variable to be analyzed using this function's x argument. Enter example(wtd.mean) to see an example and help(wtd.mean) to learn more about this function.")
  x.name = deparse(substitute(x))
  
  if(!missing(data))
  {
    if(is.matrix(data)) data <- data.frame(data)
    if(!missing(x)) x <- vector.from.data(substitute(x), data)
    if(!missing(w))  w  <- vector.from.data(substitute(w), data)
  }
  
  if(length(x)<=1) return(NA)
  
  check.variable(x, vartype="numeric")
  if(!missing(w)) check.variable(w, vartype="numeric")
  
  result = round(Hmisc::wtd.mean(x=x, weights=w, ...), digits)
  return(result)
}



#' Find Median of Variable with Option to Weight Observations
#' @param x A variable
#' @param w (Optional) Sampling weights of variable
#' @param data (Optional) Name of dataset that contains x (and w) variable
#' @return Returns the median value of the variable
#' @description Takes in variable and finds median, works with sampling weights. Makes use of the wtd.quantile function, part of the Hmisc package. 
#' @examples 
#'    library(RCPA3)
#'    
#'    wtd.median(x=nes$approve.pres.hc, w=nes$wt)
#'    wtd.median(x=nes$approve.pres.hc)
#' @export
#' @keywords internal
#' @importFrom Hmisc wtd.quantile
wtd.median <- function(x, w=NULL, data, ...)
{
  if(missing(x)) stop("Oops. You need to identify the variable to be analyzed using this function's x argument. Enter example(wtd.median) to see an example and help(wtd.median) to learn more about this function.")
  
  if(!missing(data))
  {
    if(is.matrix(data)) data <- data.frame(data)
    x <- vector.from.data(substitute(x), data)
    if(!missing(w))  w <- vector.from.data(substitute(w), data)
  }
  
  check.variable(x)
  if(!missing(w)) check.variable(w, vartype="numeric")
  
  result = Hmisc::wtd.quantile(as.numeric(x), weights=w, probs=.5)
  if(is.factor(x)) return(levels(x)[result]) 
  if(!is.factor(x)) return(as.numeric(result))   
}



#' Find Mode of Variable with Option to Weight Observations
#' @param x A variable
#' @param w (Optional) Sampling weights of variable
#' @param data (Optional) A dataset
#' @param ... (Optional) Additional arguments pass to descr::freq function
#' @return Returns the modal value(s) of the variable, if mode exists
#' @description Takes in variable and finds mode, works with sampling weights. Makes use of the freq function, part of the descr package. 
#' @examples 
#'    library(RCPA3)
#'    
#'    wtd.mode(x=nes$angry.about.things, w=nes$wt)
#'    wtd.mode(x=nes$angry.about.things)
#'    
#'    wtd.mode(x=nes$discrim.vs.men, w=nes$wt)
#'    wtd.mode(x=nes$discrim.vs.men)
#' @export
#' @keywords internal
#' @importFrom descr freq
#' @importFrom Hmisc all.is.numeric
wtd.mode <- function(x, w, data, ...)
{
  if(missing(x)) stop("Oops. You need to identify the variable to be analyzed using this function's x argument. Enter example(wtd.mode) to see an example and help(wtd.mode) to learn more about this function.")
  
  if(!missing(data))
  {
    if(is.matrix(data)) data <- data.frame(data)
    x <- vector.from.data(substitute(x), data)
    if(!missing(w))  w <- vector.from.data(substitute(w), data)
  }
  
  check.variable(x)
  if(!missing(w)) check.variable(w, vartype="numeric")
  if(missing(w)) w <- rep(1, length(x))  

  k <- grep(FALSE, (is.na(x) | is.na(w))) # checks for missing values
  x <- x[k]
  w <- w[k]
  
  if(length(x) == length(unique(x))) {
    cat("Every value of", deparse(substitute(x)), "is unique. There is no modal value.\n")
    return("None")
  }

  result = data.frame(descr::freq(x=x, w=w, plot=F, ...))
  maxfreq = max(result$Frequency[-length(result$Frequency)]) # which.max returns row number with max
  # return(maxfreq)
  mode = row.names(result)[result$Frequency == maxfreq]
  # what if two or more values have same frequency? okay
  if(Hmisc::all.is.numeric(mode)) mode = as.numeric(mode)
  return(mode)
}



#' Finds the Quantile Values of Variable With Option for Weights
#' @param x A variable (must be numeric), should be in dataset$var form unless dataset specified in optional data argument.
#' @param w (Optional) Sampling weights of variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains x (and w) variable.
#' @param q (Optional) The quantiles you want calculated as a number or vector of numbers. Input as percentages. For median only, q=50. For quartiles, q=c(25, 50, 75).
#' @param digits (Optional) Number of decimal places reported in result (defaults to 3).
#' @param ... (Optional) Additional arguments passed to Hmisc::wtd.quantile function.
#' @return Returns quantile values of a variable, a vector of numeric values.
#' @description Takes in variable and calculates desired quantile values, works with sampling weights. Makes use of the wtd.quantile function from the Hmisc package. See \code{\link[Hmisc]{wtd.stats}} documentation for details.
#' @examples 
#'    library(RCPA3)
#'    
#'    wtd.quantile(x=nes$ft.police, q=c(25, 50, 75), w=nes$wt)
#'    wtd.quantile(x=nes$ft.police, q=c(25, 50, 75))
#' @export
#' @keywords internal
#' @importFrom Hmisc wtd.quantile
wtd.quantile = function(x, w=NULL, data, q=c(5,10,25,50,75,90,95), digits=3, ...) 
{
  if(missing(x)) stop("Oops. You need to identify the variable to be analyzed using this function's x argument. Enter example(wtd.quantile) to see an example and help(wtd.quantile) to learn more.")
  
  if(!missing(data))
  {
    if(is.matrix(data)) data <- data.frame(data)
    if(!missing(x)) x <- vector.from.data(substitute(x), data)
    if(!missing(w))  w  <- vector.from.data(substitute(w), data)
  }
  
  check.variable(x, vartype="numeric")
  if(!missing(w)) check.variable(w, vartype="numeric")
  
  if(all(q <= 1) & all(q >= 0)) {
    q <- q * 100
  }
  result = round(Hmisc::wtd.quantile(x=x, weights=w, probs=q/100, ...), digits)
  return(result)
}



#' Calculates Standard Deviation of Variable With Option for Weights
#' @param x A variable (must be numeric), should be in dataset$var form unless dataset specified in optional data argument.
#' @param w (Optional) Sampling weights of variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains x (and w) variable.
#' @param digits (Optional) Number of decimal places reported in result (defaults to 3).
#' @param ... (Optional) Additional arguments passed to Hmisc::wtd.var function.
#' @return Returns the standard deviation of the variable, a numeric value.
#' @description Takes in variable and calculates standard deviation, works with sampling weights. Makes use of the wtd.var function, part of the Hmisc package.
#' @examples 
#'    library(RCPA3)
#'    
#'    wtd.sd(x=nes$ft.socialists, w=nes$wt)
#'    wtd.sd(x=nes$ft.socialists)
#' @export
#' @keywords internal
#' @importFrom Hmisc wtd.var
wtd.sd = function(x, w=NULL, data, digits=3, ...) 
{
  if(missing(x)) stop("Oops. You need to identify the variable to be analyzed using this function's x argument. Enter example(wtd.sd) to see an example and help(wtd.sd) to learn more about this function.")
  x.name = deparse(substitute(x))
  
  if(!missing(data))
  {
    if(is.matrix(data)) data <- data.frame(data)
    if(!missing(x)) x <- vector.from.data(substitute(x), data)
    if(!missing(w))  w  <- vector.from.data(substitute(w), data)
  }
  if(length(x)<=1) return(NA)
  check.variable(x, vartype="numeric")
  if(!missing(w)) check.variable(w, vartype="numeric")
  result = round(sqrt(Hmisc::wtd.var(x=x, weights=w, ...)), digits)
  return(result)
}



#' Calculates Skewness of Numeric Variable with Option to Weight Observations
#' @param x Variable (must be a numeric vector)
#' @param w (Optional) Weights variable 
#' @param data (Optional) Dataset
#' @param digits (Optional) Number of digits after decimal point (default: 3)
#' @return Returns the skewness of the variable, a numeric value.
#' @examples 
#'    library(RCPA3)
#'    
#'    \donttest{
#'    wtd.skewness(x=nes$ft.socialists, w=nes$wt)
#'    wtd.skewness(x=nes$ft.socialists)
#'    }
#' @export
#' @keywords internal
#' @importFrom Hmisc wtd.var
#' @importFrom stats weighted.mean
wtd.skewness <- function(x, w, data, digits=3) 
  {
  
  if(missing(x)) stop("Oops. You need to identify the variable to be analyzed using this function's x argument. Enter example(wtd.skewness) to see an example and help(wtd.skewness) to learn more about this function.")
  
  if(!missing(data))
  {
    if(is.matrix(data)) data <- data.frame(data)
    x <- vector.from.data(substitute(x), data)
    if(!missing(w)) w <- vector.from.data(substitute(w), data)
  }
  
  if (missing(w)) { # if no w given, then w=1 always
    w <- rep(1, length(x))
  }
  else {
    if (length(x) != length(w)) {
      msg <- paste(deparse(substitute(x)), gettext("and", domain = "R-descr"), deparse(substitute(w)), gettext("have different lengths.", domain = "R-descr"))
      stop(msg)
    }
  }
  if (is.numeric(w)) {
    class(w) <- "numeric"
  }
  
  k <- grep(FALSE, (is.na(x) | is.na(w))) # checks for missing values
  x <- x[k]
  w <- w[k]
  n <- length(x)
  w.n <- sum(w) # equals n if w=1
  x.mean <- stats::weighted.mean(x, w)
  w.sd  <- sqrt(Hmisc::wtd.var(x=x, weights=w)) # weighted 
  z.dev <- (x - x.mean) / w.sd
  
  wtd.skewness <- (sum(w*(x - x.mean)^3)/w.n)/(sum((x - x.mean)^2)/n)^(3/2)
  class(wtd.skewness) <- "numeric"
  round(wtd.skewness, digits)
}



#' Calculates Variance of Variable With Option for Weights
#' @param x A variable (must be numeric), should be in dataset$var form unless dataset specified in optional data argument.
#' @param w (Optional) Sampling weights of variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains x (and w) variable.
#' @param digits (Optional) Number of decimal places reported in result (defaults to 3).
#' @param ... (Optional) Additional arguments passed to Hmisc::wtd.var function.
#' @return Returns the variance of the variable, a numeric value.
#' @description Takes in variable and calculates its variance, works with sampling weights. Makes use of the wtd.var function from the Hmisc package. See \code{\link[Hmisc]{wtd.stats}} documentation for details.
#' @examples 
#'    library(RCPA3)
#'    
#'    wtd.var(x=nes$ft.socialists, w=nes$wt)
#'    wtd.var(x=nes$ft.socialists)
#' @export
#' @keywords internal
#' @importFrom Hmisc wtd.var
wtd.var <- function(x, w=NULL, data, digits=3, ...) 
{
  if(missing(x)) stop("Oops. You need to identify the variable to be analyzed using this function's x argument. Enter example(wtd.var) to see an example and help(wtd.var) to learn more about this function.")
  x.name = deparse(substitute(x))
  
  if(!missing(data))
  {
    if(is.matrix(data)) data <- data.frame(data)
    if(!missing(x)) x <- vector.from.data(substitute(x), data)
    if(!missing(w))  w <- vector.from.data(substitute(w), data)
  }
  
  check.variable(x, vartype="numeric")
  if(!missing(w)) check.variable(w, vartype="numeric")
  
  result <- round(Hmisc::wtd.var(x=x, weights=w, ...), digits)
  return(result)
}