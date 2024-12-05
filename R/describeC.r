#' Generates table of descriptive statistics for one or more variables in a dataset 
#'
#' @description Prints a table of descriptive statistics for variable(s) specified with x argument. Works with variables measures at any level but output varies with level of measurement (e.g. you won't get standard deviation for a nominal variable). Option for weighting observations. 
#' @param x A variable or list of variables, should be in dataset$var form unless dataset specified in optional data argument.
#' @param w (Optional) Sampling weights of variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains x (and w) variable.
#' @param digits (Optional) Number of decimal places reported in result (defaults to 3).
#' @param printC (Optional) Do you want to print table of descriptive statistics to .html file in working directory? (default: FALSE)
#' @return Table of descriptive statistics
#' @examples  
#'   library(RCPA3)
#'   
#'   # descriptive statistics for qualitative variables
#'   describeC(x=world$region)
#'   
#'   \dontrun{
#'   # descriptive statistics for numeric variable
#'   describeC(x=world$infant.mortality)
#'   
#'   # describe multiple variables in list, with optional w argument
#'   describeC(x=list(angry.about.things, approve.cong), w=wt, data=nes)
#'   }
#' @export
#' @section RCPA3 Package Tutorial Videos:
#' * [Generate Descriptive Statistics with RCPA3 Package's describeC Function](https://www.youtube.com/watch?v=QbMlUgwG1H4) 17:40  
#' * [Complete Playlist of RCPA3 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook References:
#' * Philip H. Pollock and Barry C. Edwards, _An R Companion to Political Analysis, 3rd Edition_ (Thousand Oaks, CA: Sage Publications, Forthcoming 2022), Chapter 2. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Thousand Oaks, CA: Sage Publications, 2020), pp. 39-55. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [Tutorials & Resources for Descriptive Statistics](https://www.poliscidata.com/pages/rDemosResources.php?chapter=2), Compiled by Barry C. Edwards
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications. 
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom Hmisc all.is.numeric wtd.quantile wtd.var
#' @importFrom knitr kable
#' @md



describeC <- function(x, w, data, digits=3, printC=FALSE) 
  {
  
  if(missing(x))  stop("Oops. You need to specify the variables to be analyzed. To see how to use this function, try example(descibeC) or help(describeC).")
  if(!missing(w)) w.name = deparse(substitute(w))
  check.value(digits, valuetype="numeric")
  
  
  if(!missing(data)) # data arg supplied
  {
    variables.list <- as.list(substitute(x)) # was as.list(substitute(x))
    if(length(variables.list)>1) variables.list <- variables.list[-1]
    if(is.matrix(data)) data <- data.frame(data)
    variable.names = NULL
    variable.set = data.frame(matrix(NA, nrow=nrow(data), ncol=length(variables.list)))
    for(i in 1:length(variables.list)) 
      {
        variable.names[i] <- deparse(variables.list[[i]])
        variable.set[, i] <- vector.from.data(variables.list[[i]], data)
      }
    colnames(variable.set) <- variable.names
    if(!missing(w))  w  <- vector.from.data(substitute(w), data)
  }
  
  if(missing(data))
  {
    # if(!is.vector(x))
    if(!is.list(x))
    {
      variables.list <- NULL
      variables.list[[1]] = substitute(x)
      # return(variables.list)
    }
    if(is.list(x))
    {
      variables.list <- as.list(substitute(x))
      # variables.list <- substitute(x)
      if(length(variables.list)>1) variables.list <- variables.list[-1]
      # return(variables.list)
    }
    variable.names = NULL
    variable.set = data.frame(matrix(NA, ncol=length(variables.list), nrow=length(unlist(x)) / length(variables.list)))
    for(i in 1:length(variables.list)) 
      {
        variable.names[i] <- deparse(variables.list[[i]])
        variable.set[, i] <- eval(variables.list[[i]])
      }
    variable.set <- data.frame(variable.set, stringsAsFactors=TRUE)
    colnames(variable.set) <- variable.names
    # return(variable.names)
  }
  

  
  if(!missing(w)) 
  {
    weighted = TRUE    
    w.length <- length(w)
    # return(list(w.length, nrow(variable.set)))
    if (nrow(variable.set) != w.length) stop(paste(gettext("X variables and", domain = "R-descr"), w.name, gettext("have different lengths.", domain = "R-descr")))
  }
  else 
    {
      w <- rep(1, nrow(variable.set))
      weighted = FALSE
    }
  
  check.variable(w, vartype="numeric")
  w.full <- w 
  w.rawsum <- sum(w, na.rm=T)
  
  
  # set-up the table of results
  descriptives.names = c("Observed values", "Missing values", "Unique values", "Class", 
                         "Mean", "Median", "Mode",
                         "Variance", "Standard deviation", "Minimum", "Maximum", "Range", "First quartile (25%)",
                         "Third quartile (75%)", "Interquartile range (IQR)", "Skewness", "Kurtosis")
  descriptives.df <- data.frame(matrix(NA, nrow=length(descriptives.names), ncol=length(variable.names)))
  rownames(descriptives.df) <- descriptives.names
  colnames(descriptives.df) <- variable.names
  factormessage = NULL
  # return(descriptives.df)
  
  for(i in 1: length(variable.names))
  {
      x.name <- variable.names[i]
      x <- variable.set[, x.name]
      # this could be done for each variable in variable.set
      k <- grep(FALSE, (is.na(x) | is.na(w.full))) # checks for missing values
      x <- x[k]
      w <- w.full[k]
      x.nonmissing <- sum(w)
      x.missing <- w.rawsum - x.nonmissing
      # if(is.numeric(x)) message("variable is numeric")
      if(Hmisc::all.is.numeric(x, extras=c(NA)) & !is.numeric(x))
      {
        transform.warning <- paste("Note:", x.name, "is not a numeric (interval-level) variable, but can be analyzed as one. Do you want to analyze", x.name, "as numeric?\nEnter y to analyze", x.name,"as numeric or any other key to leave it as is.")
        if(tolower(ask(transform.warning))=="y") x = as.numeric(x)        
      }
      
      # can calculate mode for factor, ordered, and numeric
      unique <- length(unique(x))
      if(unique == length(x)) mode = "None"  
      else 
      { 
        if(is.numeric(x)) mode <- round(as.numeric(wtd.mode(x=x, w=w)), digits)
        else mode <- wtd.mode(x=x, w=w)
      }
      mode <- paste(mode, collapse=", ")
      
      # should be able to find median for ordered or numeric
      if(is.numeric(x))  median <- round(as.numeric(wtd.median(x=x, w=w)), digits)
      if(is.ordered(x))  median <- wtd.median(x=x, w=w)
    
      if(is.numeric(x))
      {
        # cat("x is numeric\n")
        mean <- stats::weighted.mean(x=x, w=w, na.rm = T)
        var  <- Hmisc::wtd.var(x=x, weights=w)
        sd   <- sqrt(var)
        min  <- min(x)
        max  <- max(x)
        range <- (max - min)
        quantiles <- Hmisc::wtd.quantile(x=x, weights=w, probs=c(.10,.25,.50,.75,.90))
        Q1 <- quantiles["25%"]
        Q3 <- quantiles["75%"]
        IQR <- Q3 - Q1
        skewness <- wtd.skewness(x, w)
        kurtosis <- wtd.kurtosis(x, w)
      }
      
      descriptives.df["Observed values", x.name] <- round(x.nonmissing, digits)
      descriptives.df["Missing values", x.name] <- round(x.missing, digits)
      descriptives.df["Unique values", x.name] <- unique 
      descriptives.df["Class", x.name] <- paste(class(x), collapse=", ")
      
      if(is.numeric(x))   descriptives.df["Mean", x.name] <- round(mean, digits)
      if(is.numeric(x) || is.ordered(x))   descriptives.df["Median", x.name] <- median
      descriptives.df["Mode", x.name] <- mode
      
      if(is.numeric(x))
      {
        descriptives.df["Variance", x.name] <-  round(var, digits)
        descriptives.df["Standard deviation", x.name] <-  round(sd, digits)
        descriptives.df["Minimum", x.name] <-  round(min, digits)
        descriptives.df["Maximum", x.name] <-  round(max, digits)
        descriptives.df["Range", x.name] <-  round(range, digits)
        descriptives.df["First quartile (25%)", x.name] <-  round(Q1, digits)
        descriptives.df["Third quartile (75%)", x.name] <-  round(Q3, digits)
        descriptives.df["Interquartile range (IQR)", x.name] <-  round(IQR, digits)
        descriptives.df["Skewness", x.name] <-  round(skewness, digits)
        descriptives.df["Kurtosis", x.name] <-  round(kurtosis, digits)
      }

      if(is.factor(x) && !is.ordered(x)) factormessage <- paste(factormessage, "Note: ", x.name, " was analyzed as an unordered factor.\nTo evaluate as ordered factor, try x=list(as.ordered(", x.name, "))\n", sep="")

  }
  
  # cat("about to print\n")
  # after processing each variable in variable set, determine if any rows empty
  printrows <- NULL
  for(j in 1:nrow(descriptives.df)) if(!all(is.na(descriptives.df[j, ]))) printrows = c(printrows, j)
  # return(printrows)
  # cat("\n")
  # looks like this needs to reset or kable prints right right
  descriptives.df <- data.frame(descriptives.df[printrows, ])
  descriptives.df[is.na(descriptives.df)] <- ""
  rownames(descriptives.df) <- descriptives.names[printrows]
  colnames(descriptives.df) <- variable.names
  
  # display table of results
 
  main.heading <- headingbox("Descriptive Statistics", width=75, marker="=")
  if(printC==TRUE) printC(main.heading)
  
  variable.names.string <- paste(variable.names, collapse=", ")
  table_caption <- paste("Descriptive Statistics for", variable.names.string)
  if(weighted) table_caption <- paste(table_caption, ", weighted by ", w.name, sep="")
  
  print(knitr::kable(format(descriptives.df, drop0trailing=F, nsmall=digits, digits=digits), format="simple", align="r", caption=table_caption))
  if(printC==TRUE) printC(knitr::kable(format(descriptives.df, drop0trailing=F, nsmall=digits, digits=digits), caption=printCaption(table_caption), format="html"))
  cat("\n")
  slightpause()
  
  if(!is.null(factormessage)) message(factormessage)
  
  if(printC==T) printC(match.call(expand.dots = FALSE))
  invisible(descriptives.df)
  
}

             