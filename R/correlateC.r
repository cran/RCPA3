#' Correlation analysis for two or more numeric variables, with options for scatterplots, weighted observations, and inferential statistics.
#'
#' @description Given two or more numeric variables, `correlateC` reports correlation coefficients, along with inferential statistics (if requested), works with sampling weights. If more than two x variables are supplied, the function calculates correlation coefficients using pairwise complete observations (as opposed to limiting analysis to observations complete on all variables).
#' The \code{\link[weights]{wtd.cor}} function is imported from the weights package. See \code{\link[weights]{wtd.cor}} documentation for details.
#' @param x A list of variables for correlation analysis, variables must be numeric. Should be entered as list(dataset$var1, dataset$var2, dataset$var3 ... ) form unless dataset specified in optional data argument.
#' @param w (Optional) Sampling weights variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains x (and w) variable (optional).
#' @param digits (Optional) Number of decimal places reported in result (defaults to 3).
#' @param stats (Optional) Do you want the inferential statistics (standard errors, t-statistics, and p-values)? Default is FALSE. Set to TRUE for inferential statistics.
#' @param printC (Optional) Do you want results printed to .html file in your working directory? Default is FALSE. Set to TRUE to print results.
#' @param plot (Optional) Do you want scatterplot(s)? Default is FALSE.
#' @param jitter (Optional) Do you want scatterplot pointed jittered? By default, points jittered when there are more than 500 observations, but you can set this arguments to TRUE/FALSE to override the default.
#' @param ... (Optional) Additional arguments passed to `weights::wtd.cor` function.
#' @details Makes use of the wtd.cor function, part of the weights package.
#' @return Returns the coefficients of correlation among x variables; if `stats=TRUE`, inferential statistics returned in tables as well.
#' @examples 
#'    library(RCPA3)
#'    
#'    \donttest{
#'    correlateC(x=list(abortlaws, women.stateleg), data=states, plot=FALSE)
#'    
#'    # with weighted observations and inferential statistics
#'    correlateC(x=list(nes$ft.rep, nes$ft.trump.pre, nes$ft.dem, nes$ft.biden.pre), 
#'               w=nes$wt, stats=TRUE)
#'    }
#' @export
#' @section Textbook References:
#' * Philip H. Pollock and Barry C. Edwards, _An R Companion to Political Analysis, 3rd Edition_ (Thousand Oaks, CA: Sage Publications, Forthcoming 2022), Chapter 11. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Thousand Oaks, CA: Sage Publications, 2020), pp. 240-244. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Correlation and Bivariate Regression](https://www.poliscidata.com/pages/rDemosResources.php?chapter=11), compiled by Barry C. Edwards.
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications. 
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom weights wtd.cor
#' @importFrom knitr kable
#' @importFrom graphics par pairs
#' @importFrom grDevices png dev.off
#' @md

# need to eliminate option(warn=-1), use suppressWarnings() instead

correlateC <- function(x, w, data, digits=3, stats=FALSE, printC=FALSE, 
                       plot=FALSE, jitter=FALSE, ...) 
{ 
  if(missing(x))  stop("Oops. You need to specify the variables to be analyzed. To see how to use this function, try example(correlateC) or help(correlateC).")
  
  if(plot!=FALSE)
  {
    old.par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old.par))
  }
  
  if(!missing(w))  w.name = deparse(substitute(w))
  check.value(digits, valuetype="numeric")
  
  
  variables.list <- as.list(substitute(x)[-1])
  # return(list(variables.list, length(variables.list)))
  variable.names = rep(NA, length(variables.list))
  for(i in 1:length(variables.list)) variable.names[i] <- deparse(variables.list[[i]])
  if(length(variable.names)==1) stop("There's a problem: Your list of variables must contain two or more variables for correlation analysis. To see how to use this function, try example(correlateC) or help(correlateC).")
  # return(variable.names)
  
  if(!missing(data)) # data arg supplied
  {
    if(is.matrix(data)) data <- data.frame(data)
    variable.set <- data[, variable.names]
    if(!missing(w))  w  <- vector.from.data(substitute(w), data)
  }
  
  if(missing(data))
  {
    variable.set <- data.frame(variables.list) # was (x)
    colnames(variable.set) <- variable.names
    # return(head(variable.set))
  }
  
  if(!missing(w)) 
  {
    check.variable(w, vartype="numeric")
    weighted <- TRUE    
  }
  else 
    {
      weighted <- FALSE
      w <- NULL
    }
  


    
    main.heading <- headingbox("Correlation Analysis", marker="=")
    if(printC==TRUE) printC(main.heading)
  
    # simple bivariate correlation doesn't need matrix display
    if(ncol(variable.set) == 2)
    {
      result <- weights::wtd.cor(x=variable.set[, 1], y=variable.set[, 2], weight=w, ...)
      result <- data.frame(round(result, digits))
      rownames(result) <- NULL
      
      caption = paste("Correlation between", variable.names[1], "and", variable.names[2])
      if(weighted==TRUE) caption = paste(caption, ", weighted by ", w.name, sep="")
      
      if(stats==TRUE)
      {
        # restore.options.scipen <- options("scipen") 
        # options(scipen = 999)
        print(knitr::kable(format(result, drop0trailing=F, nsmall=digits, digits=digits, scientific=999), format="simple", 
                           caption=caption, align="r"))
        return.result <- result
        # options(scipen = restore.options.scipen)
        if(printC==TRUE) printC(knitr::kable(format(result, drop0trailing=F, nsmall=digits, digits=digits, scientific=999), format="html", 
                                           caption=printCaption(caption), align="r"))
      }
      if(stats==FALSE) 
      {
        # cat("\n", caption, "\n")
        # there isn't table to print, it's just one number
        cat("\n")
        coef.only <- c(result[ , 1])
        names(coef.only) <- c("Correlation Coefficient:")
        print(coef.only)
        rownames(result) <- "Correlation Coefficient:"
        return.result <- coef.only
        
        printc.output <- c("Correlation Coefficient:", coef.only)
        class(printc.output) <- "statement"
        if(printC==TRUE) printC(printc.output)
        # if(printC==TRUE) printC(knitr::kable(format(result[ , 1], drop0trailing=F, nsmall=digits, digits=digits), format="html", 
        #                                     caption=printCaption(caption), align="r"))
      }
      cat("\n")
      
      # simple bivariate scatterplot
      if(plot==TRUE)
      {
        # this for loop lets plot be seen by user in R and then go to PNG driver
        for(k in 1:(1+as.numeric(printC)))
        {
          
        if(printC==TRUE & k==2) 
        {
          imagename <- paste("correlateC.plot.", unclass(Sys.time()), ".png", sep="")
          grDevices::png(filename=imagename, width=3.5, height=3.5, units="in", 
                         type=getPNGtype(), pointsize=8, res=300, antialias="default")
          class(imagename) <- "image"
          printC(imagename)
        }
        # could change the color and size some depending on how many values being plotted
        # some applications could use from jittering
        if(nrow(variable.set) > 500) 
        {
          marker.cex = .8
          marker.col = "#00000020"
          if(missing(jitter)) jitter = TRUE
          message("Note: Because there are many points to plot, correlateC is jittering them. Set jitter=FALSE to prevent this.")
        }
        else
        {
          marker.cex = 1
          marker.col = "#000000"
        }
        
        if(jitter==TRUE) for(j in 1:ncol(variable.set)) variable.set[, j] <- jitter(variable.set[, j], amount=0)
        
        par.ask.restore <- graphics::par("ask")
        if(printC==TRUE & k==2) graphics::par(ask=FALSE) else graphics::par(ask=TRUE)
        
        main = paste("Scatterplot of", variable.names[1], "and", variable.names[2])
        # if(weighted==TRUE) main = paste(main, ",\n weighted by ", w.name, sep="")
        main <- strwrap(main, width=50)
        
        plot(x=variable.set[,1], y=variable.set[,2], xlab=variable.names[1], ylab=variable.names[2],
             cex=marker.cex, col=marker.col, main=main)
        graphics::par(ask=par.ask.restore)
        if(printC==TRUE & k==2) grDevices::dev.off()
        }
      }
    }
    
    ################################# correlation among multiple variables
    if(ncol(variable.set) > 2)
    {
      # restore.warning.option <- getOption("warn")
      #options(warn = -1)
      # wtd.cor generating warning that variables perfectly correlated with themselves
      result <- suppressWarnings(weights::wtd.cor(x=variable.set, weight=w, ...))
      # options(warn = restore.warning.option) 
      
      correlation <- round(result[["correlation"]], digits)
      # print(class(correlation))
      variable.names.string <- paste(variable.names, collapse = ", ")
      caption = paste("Correlation among", variable.names.string)
      if(weighted==TRUE) caption = paste(caption, ", weighted by ", w.name, sep="")
      
      print(knitr::kable(format(correlation, drop0trailing=F, nsmall=digits), format="simple", digits=digits, 
                         caption=caption, align="r"))
      slightpause()
      
      if(printC==TRUE) printC(knitr::kable(format(correlation, drop0trailing=F, nsmall=digits), digits=digits, 
                                           caption=printCaption(caption), format="html", align="r"))
      
      if(stats==FALSE) 
        {
        cat("\n")
        return.result <- correlation
        }
  
      if(stats==TRUE) 
      {
        restore.options.scipen <- options("scipen") 
        options(scipen = 999)
        # headingbox("Standard Errors", marker="~")
        # cat("\n")
        standard.errors <- round(result[["std.err"]], digits)
        se.caption = "Standard Errors of Correlation Coefficients"
        for(i in 1:ncol(standard.errors)) standard.errors[i, i] <- NA
        print(knitr::kable(format(standard.errors, drop0trailing=F, nsmall=digits), format="simple", digits=digits, 
                           caption=se.caption, align="r"))
        slightpause()
        
        return.result <- result
        
        if(printC==TRUE)  printC(knitr::kable(format(standard.errors, drop0trailing=F, nsmall=digits), digits=digits, 
                                              caption=printCaption(se.caption), format="html"))
        
        t.stats <- round(result[["t.value"]], digits)
        for(i in 1:ncol(t.stats)) t.stats[i, i] <- NA
        # print(round(t.stats, digits))
        t.stats.caption = "t-Statistics of Correlation Coefficients"
        # print(format(t.stats, drop0trailing=F, nsmall=digits), quote=F, trim=T, right=TRUE)
        print(knitr::kable(format(t.stats, drop0trailing=F, nsmall=digits), format="simple", digits=digits, 
                           caption=t.stats.caption, align="r"))
        slightpause()
        if(printC==TRUE) printC(knitr::kable(format(t.stats, drop0trailing=F, nsmall=digits), digits=digits, 
                                             caption=printCaption(t.stats.caption), format="html"))
        
        # cat("\n")
        
        p.values <- round(result[["p.value"]], digits)
        for(i in 1:ncol(p.values)) p.values[i, i] <- NA
        p.values.caption = "p-Values of the t-Statistics"
        print(knitr::kable(format(p.values, drop0trailing=F, nsmall=digits), format="simple", digits=digits, 
                           caption=p.values.caption, align="r"))
        slightpause()
        if(printC==TRUE) printC(knitr::kable(format(p.values, drop0trailing=F, nsmall=digits), digits=digits, 
                                             caption=printCaption(p.values.caption), format="html"))
        
        cat("\n")
        options(scipen = restore.options.scipen)
      }
      
      if(plot==TRUE)
      {
        # this for loop lets plot be seen by user in R and then go to PNG driver
        for(k in 1:(1+as.numeric(printC)))
        {
          
        if(printC==TRUE & k==2) 
        {
          imagename <- paste("compmeansC.plot.", unclass(Sys.time()), ".png", sep="")
          grDevices::png(filename=imagename, width=4, height=4, units="in", 
                         type=getPNGtype(), pointsize=8, res=300, antialias="default")
          class(imagename) <- "image"
          printC(imagename)
        }
        # could change the color and size some depending on how many values being plotted
        # some applications could use from jittering
        if(nrow(variable.set) > 500) 
        {
          marker.cex = .8
          marker.col = "#00000020"
          if(missing(jitter)) jitter = TRUE
          message("Note: Because there are many points to plot, correlateC is jittering them. Set jitter=FALSE to prevent this.")
        }
        else
        {
          marker.cex = 1
          marker.col = "#000000"
        }
        
        if(jitter==TRUE) for(j in 1:ncol(variable.set)) variable.set[, j] <- jitter(variable.set[, j], amount=0)
        
        par.ask.restore <- graphics::par("ask")
        if(printC==TRUE & k==2) graphics::par(ask=FALSE) else graphics::par(ask=TRUE)
        
        # main = paste("Scatterplot matrix of", variable.names.string)
        # if(weighted==TRUE) main = paste(main, "weighted by", w.name)
        graphics::pairs(x=variable.set, labels=variable.names, cex=marker.cex, cex.labels=1.6, col=marker.col)
        graphics::par(ask=par.ask.restore)
        if(printC==TRUE & k==2) grDevices::dev.off()
      }
      }
    }
    
    if(printC==T) printC(match.call(expand.dots = FALSE))
    invisible(return.result)
}
