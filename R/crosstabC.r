#' Cross-tabulation analysis, option for weighting observations, makes controlled comparisons, generates plots, performs Chi-Square test, measures strength of association
#'
#' @description This is a workhorse function for analyzing the relationship between two variables measured at the nominal or ordinal-level (factors). Basic output is a cross-tabulation with column percentages and counts. Options include weighting observations, adding control variable for controlled cross-tabulation, several plotting options, conducting Chi-Square test of independence, and measuring strength of association.
#' @param dv Dependent variable, should be in dataset$var form unless dataset specified in optional data argument. Should be a nominal or ordinal-level variable.
#' @param iv Independent variable, should be in dataset$var form unless dataset specified in optional data argument. Should be a nominal or ordinal-level variable.
#' @param w (Optional) Sampling weights of variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param z (Optional) Control variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains dv, iv (and w) variables.
#' @param digits (Optional) Number of decimal places reported in result (defaults to 2).
#' @param compact (Optional) Do you want compact display of cross-tabulation with row subtotals omitted? Default is FALSE.
#' @param dvlabs (Optional) A vector of names for the dependent variable's values (to abbreviate the cross-tabulation's row labels and dv labels on plots)
#' @param ivlabs (Optional) A vector of names for the independent variable's values (to abbreviate the cross-tabulation's column labels and iv labels on plots)
#' @param zlabs (Optional) A vector of names for the control variable's values (to abbreviate the controlled cross-tabulation's column labels and z variable labels on plots)
#' @param printC (Optional) Do you want to print cross-tabulation and plot (if plot is used) to .html file in working directory? (default: FALSE)
#' @param plot (Optional) Do you want a plot of the cross-tabulation? Default is TRUE (and makes a bar plot). Other plot options: 
#' * "line" for a line plot,
#' * "mosaic" for a mosaic plot, 
#' * "bar", TRUE, or T for a bar plot (default plot)
#' * FALSE or F to suppress plot. 
#' @param plot.response (Optional) Set `plot.response="all"` to plot all DV values (for uncontrolled comparisons only).
#' @param chisq (Optional) Do you want to conduct Chi-Square Test? If z argument specific, Chi-Square Test conducted on dv-iv relationship for each value of z.
#' @param lambda (Optional) Do you want Lambda reported? If z argument specified, Lambda reported for dv-iv relationship for each value of z.
#' @param somers (Optional) Do you want Somers' d reported? If z argument specific, Somers' D reported for dv-iv relationship for each value of z.
#' @param cramers (Optional) Do you want Cramer's V reported? If z argument specific, Cramer's V reported for dv-iv relationship for each value of z.
#' @param main (Optional) Main label for plot
#' @param xlab (Optional) x-axislabel for plot
#' @param ylab (Optional) y-axis label for plot
#' @param z.palette (Optional) For bar and line plots with control variables (z). 
#' @param legend.title (Optional) Title for legend shown if plot used with z argument.
#' @return Returns a cross-tabulation
#' @examples  
#'   library(RCPA3)
#'   
#'   \donttest{
#'   crosstabC(dv=nes$death.penalty, iv=nes$partyid3)
#'   
#'   # with optional w, data, chisq, somers arguments
#'   crosstabC(dv=death.penalty, iv=partyid3, w=wt, data=nes, chisq=TRUE, somers=TRUE)
#'   
#'   # example with optional w, data, z, and plot="line" arguments
#'   crosstabC(dv=death.penalty, iv=partyid3, w=wt, data=nes, z=gender, plot="line")
#'   }
#' @export
#' @section Textbook References:
#' * Philip H. Pollock and Barry C. Edwards, _An R Companion to Political Analysis, 3rd Edition_ (Thousand Oaks, CA: Sage Publications, Forthcoming 2022), Chapters 4, 5, 7, 10. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Thousand Oaks, CA: Sage Publications, 2020), pp.85-97, 150-156, 215-231. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [Tutorials & Resources for Making Comparisons](https://www.poliscidata.com/pages/rDemosResources.php?chapter=4), [Graphing Relationships and Describing Patterns](https://www.poliscidata.com/pages/rDemosResources.php?chapter=5), [Making Controlled Comparisons](https://www.poliscidata.com/pages/rDemosResources.php?chapter=7), and [Chi-Square Test and Analysis of Variance](https://www.poliscidata.com/pages/rDemosResources.php?chapter=10), compiled by Barry C. Edwards
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications. 
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom knitr kable
#' @importFrom grDevices png dev.off
#' @importFrom graphics par barplot mosaicplot points lines mtext legend axis box
#' @importFrom stats na.omit xtabs as.formula addmargins chisq.test ftable
#' @md

crosstabC <- function(dv, iv, w, z, data, digits=2, compact=FALSE, dvlabs, ivlabs, zlabs,
                      chisq=FALSE, lambda=FALSE, somers=FALSE, cramers=FALSE, printC=FALSE, 
                      plot=TRUE, plot.response, main, xlab, ylab, z.palette, legend.title) 
{

  if(missing(dv) && missing(iv)) stop("Oops. You need to specify a dependent variable (dv) and indepenent variable (iv). Enter example(crosstabC) to see an example and help(crosstabC) to learn more about this function.")
  
  if(plot!=FALSE)
  {
    old.par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old.par))
  }
  
  dv.name <- deparse(substitute(dv))
  iv.name <- deparse(substitute(iv))
  if(!missing(w)) w.name <- deparse(substitute(w))
  if(!missing(z)) z.name <- deparse(substitute(z))
  check.value(digits, valuetype="numeric")
  
  
  if(!missing(data))
  {
    if(is.matrix(data)) data <- data.frame(data)
    dv <- vector.from.data(substitute(dv), data)
    iv <- vector.from.data(substitute(iv), data)
    if(!missing(w)) w <- vector.from.data(substitute(w), data)
    if(!missing(z)) z <- vector.from.data(substitute(z), data)
  }
  
  check.variable(dv)
  check.variable(iv)

  dv.label <- attr(dv, "label")  
  dv.length <- length(dv)
  dv.class <- class(dv)
  
  iv.label <- attr(iv, "label")  
  iv.length <- length(iv)
  iv.class <- class(iv)
  
  # return(list(ylab, xlab))

  if (missing(w)) { # if no w given, then w=1 always
    w <- rep(1, dv.length)
    weighted = FALSE
  }
  else {
    w.length <- length(w)
    w.label <- attr(w, "label")  
    weighted = TRUE
    if (dv.length != w.length) {
      msg <- paste(dv.name, gettext("and", domain = "R-descr"), w.name, gettext("have different lengths.", domain = "R-descr"))
      stop(msg)
    }
  }
  if (is.numeric(w)) {
    class(w) <- "numeric"
  }
  else {
    check.variable(w, vartype="numeric")
  }
    
  
  if(plot!=FALSE)
  {
    plot.types = c(TRUE, "line", "bar", "mosaic")
    if(plot=="lines") plot <- "line"
    if(plot=="bars") plot <- "bar"
    if(!(plot %in% plot.types))
    {
      cat("There's a problem:", plot, "isn't an available plot type.\n")
      cat("Here are your options:\n")
      cat(as.character(plot.types), sep=", ")
      plot <- FALSE
      cat("\n")
      message("Try another plot type to generate a plot.")
      slightpause()
    }
  }
  
  if(!missing(z)) {
    z.length <- length(z)
    z.label <- attr(z, "label")  
    controlled = TRUE
    if (z.length != dv.length) stop(paste(dv.name, gettext("and", domain = "R-descr"), z.name, gettext("have different lengths.", domain = "R-descr")))
    check.variable(z, limitedvalues = 6)
  }
  else {
    z <- rep(1, dv.length)
    controlled = FALSE
  }

  # if(controlled==TRUE) return(list(z.name, unique(z), z))
  
  k <- grep(FALSE, (is.na(dv) | is.na(iv) | is.na(w) | is.na(z))) # checks for missing values
  dv <- dv[k]
  iv <- iv[k]
  w  <- w[k]
  z  <- z[k]

  # return(list(class(w), w))
  if(is.numeric(dv) & (length(unique(dv)) > 12)) stop(paste(gettext("There's a problem:", domain = "R-descr"), dv.name, gettext("is numeric variable. Try mean comparison analysis instead.", domain = "R-descr")))
  if(is.numeric(iv) & (length(unique(iv)) > 12)) stop(paste(gettext("There's a problem:", domain = "R-descr"), iv.name, gettext("is numeric variable. Collapse values to fit a crosstab.", domain = "R-descr")))


  if(!is.null(levels(dv))) response.options <- levels(dv) else response.options <- unique(stats::na.omit(dv))
  if(!missing(dvlabs)) response.options <- dvlabs
  
  # what response to plot?
  if(plot!=FALSE)
  {
    if(missing(plot.response)) 
    {
      picked.plot.response.note <- TRUE
      plot.response <- response.options[1]
      slightpause()
    }
    else picked.plot.response.note <- FALSE
    # if response isn't one of the x1 values, user should select valid one
    if(!(plot.response %in% response.options) & (plot.response != "all"))
    {
      cat("There's a problem:", plot.response, "isn't one of available response values.\n")
      cat("Here are your options:\n")
      cat(as.character(response.options), sep="\n")
      stop("Please correct this function's plot.response argument or choose another dv.")
    }
  }
  
  if(!missing(dvlabs))
  {
    if(length(dvlabs) != length(response.options)) stop("Oops. The iv labels aren't the right length for this iv's values. There are ", length(dvlabs), " labels for ", length(response.options), " different values. To see how to use the dvlabs argument, try help(crosstabC).")
  }
  if(!missing(ivlabs))
  {
    if(length(ivlabs) != length(unique(stats::na.omit(iv)))) stop("Oops. The iv labels aren't the right length for this iv's values. There are ", length(ivlabs), " labels for ", length(unique(stats::na.omit(iv))), " different values. To see how to use the ivlabs argument, try help(crosstabC).")
  }
  
  ############################### cross-tabs with no z variable
  if(controlled==FALSE) {

    main.heading <- headingbox("Cross-Tabulation Analysis", marker="=")
    if(printC==TRUE) printC(main.heading)
    
    heading <- paste("Cross-Tabulation of ", dv.name, " and ", iv.name, sep="")
    if(weighted==TRUE) heading <- paste(heading, ", weighted by ", w.name, sep="")
    # cat("\n")
    
    xtabs.obj <- stats::xtabs(stats::as.formula(w ~ dv + iv))
    
    crosstab <- stats::addmargins(xtabs.obj)
    # return(crosstab)
    coltotals <- crosstab[nrow(crosstab), ]
    
    if(!missing(ivlabs)) colnames(crosstab) <- c(ivlabs, "Totals")
    
    iv.values <- colnames(crosstab)[-ncol(crosstab)]
    
    complete.crosstab <- matrix(data = NA, nrow = 2*nrow(crosstab), ncol=ncol(crosstab))
    colnames(complete.crosstab) <- colnames(crosstab)
    rownames(complete.crosstab) <- rep("", 2*nrow(crosstab))
    
    if(!missing(dvlabs)) rownames(crosstab) <- c(dvlabs, "Totals")
    
    for(i in 1:nrow(crosstab))
    {
      complete.crosstab[(i*2)-1, ] <- round(crosstab[i, ] / coltotals * 100, digits)
      complete.crosstab[i*2, ]     <- crosstab[i, ] 
      rownames(complete.crosstab)[(i*2)-1] <- paste("%", rownames(crosstab)[i])
      rownames(complete.crosstab)[i*2]     <- paste("__Count___")
    }
    rownames(complete.crosstab)[(2*nrow(crosstab))-1] <- "% Totals"
    colnames(complete.crosstab)[ncol(crosstab)] <- "Totals"
    
    # print(format(grand.crosstab, drop0trailing=F), quote=F, trim=T, right=TRUE)
    if(compact==TRUE) 
      {
      display.rows = c(seq(1, nrow(complete.crosstab)-2, by=2), nrow(complete.crosstab)) 
      rownames(complete.crosstab)[nrow(complete.crosstab)] = "Count"
      }
    else display.rows = 1:nrow(complete.crosstab)
    
    print(knitr::kable(format(complete.crosstab[display.rows, ], drop0trailing=F, nsmall=digits, digits=digits), format="simple", 
                       caption=heading, align="r"))
    cat("\n")
    if(printC==TRUE) printC(knitr::kable(format(complete.crosstab[display.rows, ], drop0trailing=F, nsmall=digits, digits=digits), format="html", 
                          caption=printCaption(heading), align="r"))
    
    return.crosstab <- complete.crosstab[display.rows, ]
    slightpause()
    
    crosstab.observed.frequencies <- xtabs.obj # used for chisq and cramers
    
    # return(list(names(xtp.obj1), xtp.obj1$tab, xtp.obj1$prop.col))
    if(chisq==TRUE)
    {
      chisq.test.heading <- "Chi-Squared Test of Independence"
      chisq.test.null.hypo <- paste("Null hypothesis:", dv.name, "and", iv.name, "are independent.")
      headingbox(chisq.test.heading, marker="-")
      chisq.results <- suppressWarnings(stats::chisq.test(crosstab.observed.frequencies, correct = T))
      # print(chisq.results)
      small.n.warning <- "Note: Chi-Square Test results may be incorrect. At least one expected frequency is less than 5."
      cat(chisq.test.null.hypo, "\n\n")
      chisq.output <- utils::capture.output(chisq.results)
      chisq.output <- chisq.output[-length(chisq.output)]
      if(any(chisq.results$expected<5)) 
      {
        # message(small.n.warning)
        chisq.output <- c(chisq.output, small.n.warning)  
      }
      cat(chisq.output, sep="\n") 
      if(printC==TRUE)
      {
        chisq.test.output <- c(chisq.test.heading, "", chisq.test.null.hypo, chisq.output)
        class(chisq.test.output) <- "statement"
        printC(chisq.test.output)
      }
      cat("\n")
      slightpause()
    }
    
    if(lambda==TRUE)
    {
      cat("\n")
      lambda.heading <- "Lambda Measure of Association"
      headingbox(lambda.heading, marker="-")
      lambda.table <- lambda(xtabs.obj, digits=digits, detailed=TRUE)
      print(knitr::kable(format(lambda.table, drop0trailing=F, nsmall=digits, digits=digits), format="simple", 
                         caption=lambda.heading, align="r"))
      if(printC==TRUE) printC(knitr::kable(format(lambda.table, drop0trailing=F, nsmall=digits, digits=digits), format="html", 
                                           caption=printCaption(lambda.heading), align="r"))
      cat("\n")
      slightpause()
    }
    
    if(somers==TRUE)
    {
      somers.heading <- "Somers' dyx Measure of Association"
      headingbox(somers.heading, marker="-")
      print(round(tablesomersDC(xtabs.obj), digits))
      if(printC==TRUE)
      {
        somers.output <- c(somers.heading, "",
                               utils::capture.output(round(tablesomersDC(xtabs.obj), digits)))
        class(somers.output) <- "statement"
        printC(somers.output)
      }
      slightpause()
      cat("\n")
    }
    if(cramers==TRUE)
    {
      cramers.heading <- "Cramer's V Measure of Association"
      headingbox(cramers.heading, marker="-")
      chisq.stat <- suppressWarnings(stats::chisq.test(crosstab.observed.frequencies, correct = T)$statistic)
      cramers.v <- CramersV(chi=chisq.stat, r=nrow(xtabs.obj), c=ncol(xtabs.obj), n=sum(xtabs.obj))
      names(cramers.v) <- "Cramer's V"
      print(round(cramers.v, digits))
      if(printC==TRUE)
      {
        cramers.output <- c(cramers.heading, "",
                           utils::capture.output(round(cramers.v, digits)))
        class(cramers.output) <- "statement"
        printC(cramers.output)
      }
      slightpause()
      cat("\n")
    }

    if(plot!=FALSE) # common label elements, plots with no z var
    {
      # this for loop lets plot be seen by user in R and then go to PNG driver
      for(k in 1:(1+as.numeric(printC)))
      {
        
      if(printC==TRUE & k==2) 
      {
        imagename <- paste("crosstabC.plot.", unclass(Sys.time()), ".png", sep="")
        grDevices::png(filename=imagename, width=4, height=3, units="in", type="cairo", pointsize=8, res=300, antialias="default")
        class(imagename) <- "image"
        printC(imagename)
      }
      
      if(picked.plot.response.note==TRUE & plot!="mosaic" & k<2)
      {
        message("Note: You did not identify which response value you want to plot.")
        message(paste("Therefore, crosstabC will plot the first response value: \"", response.options[1], "\"", sep=""))
        message("To plot another response value, use this function's plot.response argument.\n")
      }
      if(missing(main)) 
      {
        tempmain <- paste("plot of ", dv.name, " and ", iv.name, sep="")
        if(weighted==TRUE) tempmain <- paste(tempmain, ", weighted by ", w.name, sep="")
      }
      
      # what if x-axis labels are very long?
      if(plot!="mosaic") # bar and line may need extra space for long x-labels
      {
        maxaxislabelsize <- max(nchar(as.character(unique(iv.values))))
        nticklabels <- length(unique(iv.values))
        if((maxaxislabelsize*nticklabels) > 50) # will need extra room
        {
          graphics::par(mar=c(4.1 + sqrt(maxaxislabelsize), 4.1, 4.1, 1.1))
          cex.ticklabels <- 0.75
          las.ticklabels <- 2 # 2
          mtext.lines <- (4.1 + sqrt(maxaxislabelsize)) - 1.5
          if(k<2) message(paste("Adjusting plot configuration to fit labels. Consider modifying column labels with ivlabs argument."))
        }
        else # labels should fit w/o rotation
        {
          graphics::par(mar=c(4.1, 4.1, 3.6, 1.1))
          cex.ticklabels <- 0.9
          las.ticklabels <- 1 # 2
          mtext.lines <- 2.5
        }
      }
      
      plot.response.label <- paste("%", plot.response)
      if(plot.response=="all") plot.response.label <- c(seq(1, nrow(complete.crosstab)-2, by=2))
      if(missing(xlab)) xlab <- paste(iv.name, "values")
      plotting.values <- complete.crosstab[plot.response.label, -ncol(complete.crosstab)]
      
      restore.par.ask <- graphics::par("ask")
      if(printC==TRUE & k==2) graphics::par(ask=FALSE) else graphics::par(ask=TRUE)
      

        if(plot==TRUE || plot=="bar") 
        {
          if(missing(main)) main <- paste("Bar", tempmain)
          if((plot.response=="all") & missing(ylab)) ylab <- "Percentages"
          if(missing(ylab)) ylab <- plot.response.label
          
          if(missing(z.palette) & plot.response=="all") bar.colors <- grDevices::gray.colors(nrow(plotting.values), start=.1, end=.7)
          else if(missing(z.palette)) bar.colors <- "gray80"
          else bar.colors <- grDevices::hcl.colors(1, palette=z.palette)
          
          if(plot.response=="all") barplot.xlim <- c(0, (nrow(plotting.values)+1)*(ncol(plotting.values)+1))
          else barplot.xlim <- c(0, 1.2*length(plotting.values))
          
          main <- strwrap(main, width=50)
          graphics::barplot(plotting.values, names.arg=iv.values, axes=T, ylab=ylab, 
                  ylim=c(0,100), xlim=barplot.xlim, main=main, col=bar.colors,
                  cex.names=cex.ticklabels, las=las.ticklabels, beside=T)
          graphics::mtext(text = xlab, side = 1, line = mtext.lines)
          if(plot.response=="all") graphics::legend("topright", bty="n", fill=bar.colors, legend=rownames(complete.crosstab)[plot.response.label])
        }
        
        if(plot=="line") 
        {
          if(missing(main)) main <- paste("Line", tempmain)
          main <- strwrap(main, width=50)
          if((plot.response=="all") & missing(ylab)) ylab <- "Percentages"
          if(missing(ylab)) ylab <- plot.response.label

          if(missing(z.palette) & plot.response=="all") line.color <- grDevices::gray.colors(nrow(plotting.values), start=.1, end=.7)
          if(missing(z.palette) & plot.response!="all") line.color <- "black"
          if(!missing(z.palette)) line.color <- grDevices::hcl.colors(1, palette=z.palette)
          
          plot(x="", y="", ylim=c(0,100), xlim=c(.5, length(iv.values) + .5 + as.numeric(plot.response=="all")), axes=F, 
               xlab="", ylab=ylab, main=main, pch=16, col=line.color[1], typ="n")
          if(plot.response=="all") for(i in 1:nrow(plotting.values)) graphics::lines(x=1:length(iv.values), y=plotting.values[i, ], lty=i, col=line.color[i], lwd=2)
          else graphics::lines(x=1:length(iv.values), y=plotting.values, lty=1, col=line.color, lwd=2)
          graphics::axis(side=1, at=1:length(iv.values), labels=iv.values, cex.axis=cex.ticklabels, las=las.ticklabels)
          graphics::axis(side=2, at=seq(0,100, by=10), las=2, labels=seq(0,100, by=10))
          graphics::mtext(text = xlab, side = 1, line = mtext.lines)
          graphics::box()
          if(plot.response=="all") graphics::legend("topright", bty="n", lwd=2, lty=1:nrow(plotting.values), col=line.color, legend=rownames(complete.crosstab)[plot.response.label])
          
        }
        
        if(plot=="mosaic") 
        {
          if(missing(main)) main <- paste("Mosaic", tempmain)
          main <- strwrap(main, width=50)
          if(missing(ylab)) ylab <- paste(dv.name, "values")
          if(!missing(ivlabs)) colnames(xtabs.obj) <- ivlabs
          if(!missing(dvlabs)) rownames(xtabs.obj) <- dvlabs
          
          if(missing(z.palette)) panel.colors <- grDevices::gray.colors(nrow(xtabs.obj), start=.1, end=.7)
          else panel.colors <- grDevices::hcl.colors(nrow(xtabs.obj), palette=z.palette)
          graphics::mosaicplot(t(xtabs.obj), color=panel.colors, ylab=ylab, main=main,
                     xlab=xlab)
        }
    
      graphics::par(ask=restore.par.ask)
      if(printC==TRUE & k==2) grDevices::dev.off()
      }
    } # end of plotting command 
    
  } # print single crosstab
  
  
  ############################## controlled cross-tabs
  if(controlled==TRUE) 
  {
    main.heading <- headingbox("Controlled Cross-Tabulation Analysis", marker="=")
    if(printC==TRUE) printC(main.heading)
    
    heading <- paste("Cross-Tabulation of ", dv.name, " and ", iv.name, ", controlling for ", z.name, sep="")
    if(weighted==TRUE) heading <- paste(heading, ", weighted by ", w.name, sep="")
    # cat("\n")
    
    z.xtabs.obj <- stats::xtabs(stats::as.formula(w ~ dv + iv + z))
    if(!missing(ivlabs)) dimnames(z.xtabs.obj)[[2]] <- ivlabs
    
    z.values <- dimnames(z.xtabs.obj)[[3]]
    iv.values <- dimnames(z.xtabs.obj)[[2]]
    dv.values <- dimnames(z.xtabs.obj)[[1]]
    
    if(!missing(dvlabs)) dv.values <- dvlabs
    
    # return(list(iv.values, z.values, dv.values))
    cond.distributions <- stats::ftable(z.xtabs.obj, col.vars=c(3,2))
    somersD.values <- rep(NA, length(z.values))
    cramersV.values <- rep(NA, length(z.values))
    z.lambda.stat <- rep(NA, length(z.values))
    z.chisq.stat <- rep(NA, length(z.values))
    z.chisq.test <- vector("list", length(z.values))
    colnames.combined <- NULL
    combined.crosstab <- stats::addmargins(cond.distributions)
    colnames(combined.crosstab) <- c(rep(iv.values, length(z.values)), "| Totals")
    
    for(i in 1:length(z.values)) 
    {
      # could get controlled cross-table using xtabs w ~ dv + iv + z but won't get 
      
      # 
      thiscolumn <- (length(iv.values)*(i-1)) + 1 
      if(!missing(zlabs)) colnames(combined.crosstab)[thiscolumn] <- paste(zlabs[i], ": ", colnames(combined.crosstab)[thiscolumn], sep="")
       else colnames(combined.crosstab)[thiscolumn] <- paste(z.values[i], ": ", colnames(combined.crosstab)[thiscolumn], sep="")
      
      z.tab <- z.xtabs.obj[,,i]
      # return(list(z.tab, dim(z.tab)))
      somersD.values[i] <- tablesomersDC(z.tab)
      z.lambda.stat[i] <- lambda(z.tab)

      z.chisq <- suppressWarnings(stats::chisq.test(z.tab, correct=T))
      z.chisq.test[[i]] <- utils::capture.output(z.chisq)
      if(any(z.chisq$expected<5)) z.chisq.test[[i]] <- c(z.chisq.test[[i]], "Note: Chi-Square Test results may be incorrect. At least one expected frequency is less than 5.")  
      z.chisq.stat[i] <- z.chisq$statistic

      cramersV.values[i] <- CramersV(z.chisq.stat[i], r=nrow(z.tab), c=ncol(z.tab), n=sum(z.tab))
      # print(round(CramersV(z.chisq.stat[i], r=nrow(z.tab), c=ncol(z.tab), n=sum(z.tab)), digits))
    } # end of each value of z
      
    
    # return(list(combined.crosstab, dim(combined.crosstab), class(combined.crosstab)))
    combined.coltotals <- combined.crosstab[nrow(combined.crosstab), ]
    grand.crosstab <- matrix(data = NA, nrow = 2*nrow(combined.crosstab), ncol=ncol(combined.crosstab))
    
    colnames(grand.crosstab) <- colnames(combined.crosstab)
    rownames(grand.crosstab) <- rep("", 2*nrow(combined.crosstab))
    
    cond.distributions <- combined.crosstab[-nrow(combined.crosstab), -ncol(combined.crosstab)]
    
    for(i in 1:nrow(combined.crosstab))
    {
      grand.crosstab[(i*2)-1, ] <- round(combined.crosstab[i, ] / combined.coltotals * 100, digits)
      grand.crosstab[i*2, ]     <- combined.crosstab[i, ] 
      rownames(grand.crosstab)[(i*2)-1] <- paste("%", dv.values[i])
      rownames(grand.crosstab)[i*2]     <- paste("__Count___")
    }
    rownames(grand.crosstab)[(2*nrow(combined.crosstab))-1] <- "% Totals"
    # colnames(grand.crosstab)[ncol(combined.crosstab)] <- "| Totals"
    
    
    # print(format(grand.crosstab, drop0trailing=F), quote=F, trim=T, right=TRUE)
    if(compact==TRUE) 
    {
      display.rows = c(seq(1, nrow(grand.crosstab)-2, by=2), nrow(grand.crosstab)) 
      rownames(grand.crosstab)[nrow(grand.crosstab)] = "Count"
    }
    else display.rows = 1:nrow(grand.crosstab)
    
    print(knitr::kable(format(grand.crosstab[display.rows, ], drop0trailing=F, nsmall=digits, digits=digits), format="simple", 
                       caption=heading, align="r"))
    cat("\n")
    if(printC==TRUE) printC(knitr::kable(format(grand.crosstab[display.rows, ], drop0trailing=F, nsmall=digits, digits=digits), format="html", 
                       caption=printCaption(heading), align="r"))
    return.crosstab <- grand.crosstab[display.rows, ]
    slightpause()
    
    crosstab.observed.frequencies <- cond.distributions # used for chisq and cramers
    
    if(!missing(zlabs)) z.values <- zlabs # safe after table values calculated
    
    if(chisq==TRUE)
    {
      chisq.test.heading <- "Chi-Squared Tests of Independence"
      headingbox(chisq.test.heading, marker="-")
      # chisq.results <- suppressWarnings(stats::chisq.test(crosstab.observed.frequencies, correct = T))
      
      for(i in 1:length(z.chisq.test))
      {
        chisq.test.null.hypo <- paste("Null hypothesis: ", dv.name, " and ", iv.name, " are independent (for ", z.values[i], ")", sep="")
        chisq.output <- z.chisq.test[[i]]
        chisq.note <- paste("Note: Chi-Square Test conducted within each value of ", z.name, ".", sep="")
        cat(chisq.test.null.hypo, chisq.output, chisq.note, sep="\n")
        cat("\n\n")
        if(printC==TRUE)
        {
          chisq.test.output <- c(chisq.test.heading, "", chisq.test.null.hypo, chisq.output, chisq.note)
          class(chisq.test.output) <- "statement"
          printC(chisq.test.output)
        }
        slightpause()
      }
      
    }
    
    if(lambda==TRUE)
    {
       lambda.heading <- "Lambda Measures of Association"
       headingbox(lambda.heading, marker="-")
       lambda.note <- paste("Note: Lambda is calculated within each value of ", z.name, ".", sep="")
       z.lambda.stat <- round(z.lambda.stat, digits)
       names(z.lambda.stat) <- paste(" Lambda (for ", z.values, ")", sep="")
       cat("\n")
       print(z.lambda.stat)
       cat(lambda.note)
       cat("\n")
       if(printC==TRUE)
       {
         lambda.output <- c(lambda.heading, "",
                            utils::capture.output(z.lambda.stat), "", lambda.note)
         class(lambda.output) <- "statement"
         printC(lambda.output)
       }

       
       # lambda.table <- lambda(cond.distributions, digits=digits, detailed=TRUE)
       # print(knitr::kable(format(lambda.table, drop0trailing=F, nsmall=digits, digits=digits), format="simple", 
       #                    caption=lambda.heading, align="r"))
       # if(printC==TRUE) printC(knitr::kable(format(lambda.table, drop0trailing=F, nsmall=digits, digits=digits), format="html", 
       #                                      caption=printCaption(lambda.heading), align="r"))
       cat("\n")
       slightpause()
    }

    # you could do somers for each table, but not overall
    if(somers==TRUE)
    {
       somers.heading <- "Somers' dyx Measures of Association"
       headingbox(somers.heading, marker="-")
       somers.note <- paste("Note: Somers' dyx is calculated within each value of ", z.name, ".", sep="")
       somersD.values <- round(somersD.values, digits)
       names(somersD.values) <- paste(" Somers' d (for ", z.values, ")", sep="")
       cat("\n")
       print(somersD.values)
       cat(somers.note)
       cat("\n")
       if(printC==TRUE)
       {
         somers.output <- c(somers.heading, "",
                            utils::capture.output(somersD.values), "", somers.note)
         class(somers.output) <- "statement"
         printC(somers.output)
       }
       slightpause()
       cat("\n")
    }
    
    if(cramers==TRUE)
    {
      cramers.heading <- "Cramer's V Measures of Association"
      headingbox(cramers.heading, marker="-")
      cramers.note <- paste("Note: Cramer's V is calculated within each value of ", z.name, ".\n", sep="")
      # chisq.stat <- suppressWarnings(stats::chisq.test(cond.distributions, correct = T)$statistic)
      # cramersV.total <- CramersV(chi=chisq.stat, r=nrow(cond.distributions), c=ncol(cond.distributions), n=sum(cond.distributions))
      # cramersV.values <- c(cramersV.values, cramersV.total)
      # cramersV.partial.names <- paste("Cramer's V (for ", z.values, ")", sep="")
      # names(cramersV.values) <- c(cramersV.partial.names, "Cramer's V (for all)")
      cramersV.values <- round(cramersV.values, digits)
      names(cramersV.values) <- paste("Cramer's V (for ", z.values, ")", sep="")
      cat("\n")
      print(cramersV.values)
      cat(cramers.note)
      if(printC==TRUE)
      {
        cramers.output <- c(cramers.heading, "",
                            utils::capture.output(cramersV.values),
                            cramers.note)
        class(cramers.output) <- "statement"
        printC(cramers.output)
      }
      slightpause()
      cat("\n")
    }
    
    if(plot!=FALSE) # common label elements, plots with z variables
    {
      # this for loop lets plot be seen by user in R and then go to PNG driver
      for(k in 1:(1+as.numeric(printC)))
      {
        
      if(printC==TRUE & k==2) 
      {
        imagename <- paste("crosstabC.plot.", unclass(Sys.time()), ".png", sep="")
        grDevices::png(filename=imagename, width=4, height=3, units="in", type="cairo", pointsize=8, res=300, antialias="default")
        class(imagename) <- "image"
        printC(imagename)
      }
        
      if(picked.plot.response.note==TRUE & plot!="mosaic" & k<2)
      {
        message("Note: You did not identify which response value you want to plot.")
        message(paste("Therefore, crosstabC will plot the first response value: \"", response.options[1], "\"", sep=""))
        message("To plot another response value, use this function's plot.response argument.\n")
      }
      if(missing(main)) 
      {
        tempmain <- paste("plot of ", dv.name, " and ", iv.name, ", controlling for ", z.name, sep="")
        if(weighted==TRUE) tempmain <- paste(tempmain, ", weighted by ", w.name, sep="")
      }
      restore.par.ask <- graphics::par("ask")
      if(printC==TRUE & k==2) graphics::par(ask=FALSE) else graphics::par(ask=TRUE)
      
      restore.par.mar <- graphics::par("mar")
      plot.response.label <- paste("%", plot.response)
      
      if(plot!="mosaic")
      {
        if(missing(ylab)) ylab <- plot.response.label
        
        if(plot=="line" | plot=="points")
        {
          if(missing(xlab)) xlab <- paste(as.character(iv.name), "values")
          n.x.values.plot <- length(iv.values)
          maxaxislabelsize <- max(nchar(iv.values))
        }
        if(plot==T | plot=="bar")
        {
          if(missing(xlab)) xlab <- paste(as.character(z.name), "values")
          n.x.values.plot <- length(z.values)
          maxaxislabelsize <- max(nchar(z.values))
        }
        
        if((maxaxislabelsize*n.x.values.plot) > 50) # will need extra room
        {
          graphics::par(mar=c(4.1 + sqrt(maxaxislabelsize), 4.1, 4.1, 1.1))
          cex.ticklabels <- 0.75
          las.ticklabels <- 2 # 2
          mtext.lines <- (4.1 + sqrt(maxaxislabelsize)) - 1.5
          if(k<2) message(paste("Adjusting plot configuation to fit labels. Consider modifying variable levels."))
        }
        else # labels should fit w/o rotation
        {
          graphics::par(mar=c(4.1, 4.1, 3.6, 1.1))
          cex.ticklabels <- 0.9
          las.ticklabels <- 1 # 2
          mtext.lines <- 2.5
        }
      }
    
        if(plot==TRUE || plot=="bar") 
        {
           if(missing(z.palette)) bar.colors <- grDevices::gray.colors(length(iv.values), start=.1, end=.7)
           else bar.colors <- grDevices::hcl.colors(length(iv.values), palette=z.palette)
           
           if(missing(legend.title)) legend.title <- iv.name
           if(missing(main)) main <- paste("Bar", tempmain)
           main <- strwrap(main, width=50)

           if(missing(xlab)) xlab <- paste(z.name, "values")
           plotting.matrix <- matrix(grand.crosstab[plot.response.label, -ncol(grand.crosstab)], nrow=length(iv.values), byrow = F)
           # print(plotting.matrix)
           graphics::barplot(plotting.matrix, xlim=c(0, (nrow(plotting.matrix)+1)*(ncol(plotting.matrix)+1)),
                   beside=T, names.arg=z.values, space=c(.15, 1),
                   las=las.ticklabels, axes=T, cex.names=cex.ticklabels, legend.text=iv.values, ylab=ylab, xlab="", col=bar.colors,
                   args.legend=list(x="topright", title=legend.title, horiz=T, box.col=bar.colors, bty="n", cex=.85, horiz=FALSE), 
                   ylim=c(0,100), 
                   main=main)
           graphics::mtext(text = xlab, side = 1, line = mtext.lines)
        }
        
        if(plot=="line") 
        {
          if(missing(z.palette)) line.colors <- grDevices::gray.colors(length(z.values), start=.1, end=.7)
          else line.colors <- grDevices::hcl.colors(length(z.values), palette=z.palette)
          if(missing(legend.title)) legend.title <- z.name
          if(missing(main)) main <- paste("Line", tempmain)
          main <- strwrap(main, width=50)
          if(missing(ylab)) ylab <- plot.response
          if(missing(xlab)) xlab <- paste(iv.name, "values")
          plotting.matrix <- matrix(grand.crosstab[plot.response.label, -ncol(grand.crosstab)], nrow=length(z.values), byrow = T)
          plot(x="", y="", ylim=c(0,110), xlim=c(.5, length(iv.values) + .5), axes=F, 
               xlab="", ylab=ylab, main=main)
          for(i in 1:length(z.values))
          {
            graphics::points(x=1:length(iv.values), y=plotting.matrix[i, ], lty=i, col=line.colors[i], pch=15+i)
            graphics::lines(x=1:length(iv.values), y=plotting.matrix[i, ], lty=i, col=line.colors[i], lwd=2)
          }
          graphics::axis(side=1, at=1:length(iv.values), labels=iv.values, cex.axis=cex.ticklabels, las=las.ticklabels)
          graphics::axis(side=2, at=seq(0,100, by=10), las=2, labels=seq(0,100, by=10))
          graphics::legend(x="topright", legend=z.values, horiz=T, lty=1:length(z.values), lwd=2, title=legend.title,
                 col=line.colors, bty="n", cex = .8, pch=seq(16,length.out=length(z.values)))
          graphics::mtext(text = xlab, side = 1, line = mtext.lines)
          graphics::box()
        }
        
        if(plot=="mosaic") 
        {
          if(missing(main)) main <- paste("Mosaic", tempmain)
          main <- strwrap(main, width=50)
          if(missing(ylab)) ylab <- paste(dv.name, "values")
          if(missing(xlab)) xlab <- paste(iv.name, "controlling for", z.name)
          graphics::par(mar=c(4,4,4,1))
          
          if(missing(z.palette)) panel.colors <- grDevices::gray.colors(length(dv.values), start=.1, end=.7)
          else panel.colors <- grDevices::hcl.colors(length(dv.values), palette=z.palette)
        
          keeprows <- seq(from=1, by=2, length.out=length(dv.values))
          plotting.matrix <- grand.crosstab[keeprows, -ncol(grand.crosstab)]
          
          # return(list(rownames(cond.distributions), colnames(cond.distributions)))
          graphics::mosaicplot(t(plotting.matrix), color=panel.colors, 
                               ylab=ylab, main=main,
                               xlab=xlab)
        }
      
      graphics::par(mar=restore.par.mar, ask=restore.par.ask)
      if(printC==TRUE & k==2) grDevices::dev.off()
      }
    } # end of plot statements
    
  } # end statements for controlled=TRUE
  
  if(printC==T) printC(match.call(expand.dots = FALSE))
  invisible(return.crosstab)
  
} # end of crosstabC function




