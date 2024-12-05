#' Mean comparison analysis function, makes controlled comparisons, generates plots, performs ANOVA
#'
#' @description Mean comparison analysis, options for weighted observations and control variable. Also supports several plotting options for basic mean comparisons and controlled mean comparisons. Can conduct single and two-factor analysis of variance (ANOVA) to test differences among multiple means.
#' @param dv Dependent variable, should be in dataset$var form unless dataset specified in optional data argument.
#' @param iv Independent variable, should be in dataset$var form unless dataset specified in optional data argument.
#' @param w (Optional) Sampling weights of variable (optional), must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param z (Optional) Control variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains dv, iv (and w) variables (optional).
#' @param digits (Optional) The number of decimal places reported in result (defaults to 2).
#' @param compact (Optional) Do you want compact version of controlled mean comparison table with N and Std. Dev. values omitted? Default is FALSE. Compact display only available for controlled comparisons.
#' @param ivlabs (Optional) A vector of names for the independent variable's values (to abbreviate the mean comparison table's row labels and iv labels on plots)
#' @param zlabs (Optional) A vector of names for the control variable's values (to abbreviate a controlled mean comparison table's column labels and z variable's labels on plots)
#' @param anova (Optional) Do you want to conduct analysis of variance (ANOVA)? Default is FALSE. 
#' @param printC (Optional) Do you want results printed to .html file in your working directory? Default is FALSE. Set to TRUE to print results.
#' @param plot (Optional) Do you want a plot of the means? Default is TRUE (makes a bar plot). Additional options:
#' * "line" to make a line plot
#' * "bar", TRUE, or T for bar plot (default plot)
#' * "points" to show means as points without connecting lines,
#' * FALSE or F to suppress plot.
#' @param main (Optional) Main label for plot, if missing, default main title generated.
#' @param xlab (Optional) x-axis label for plot, if missing, default label generated using iv name.
#' @param ylab (Optional) y-axis label for plot, if missing, default label generated using dv name.
#' @param ylim (Optional) Range of y-axis values on plot.
#' @param plot.ci (Optional) Do you want vertical 95 percent confidence intervals added to line plot of means? Default is FALSE. Only works when `plot="line"` or `plot="points"`
#' @param z.palette (Optional) For bar and line charts with control variables (z), the name of HCL color palette to use. Default is "LightGrays". See `grDevices::hcl.pals` for palette names and more information. Also see <https://developer.r-project.org/Blog/public/2019/04/01/hcl-based-color-palettes-in-grdevices/> to view color palettes.
#' @param legend.title (Optional) Customize title of legend on plot used for controlling comparisons. 
#' @return Returns a mean comparison table as a matrix of values.
#' @examples 
#'   library(RCPA3)
#'    
#'   compmeansC(dv=nes$ft.rep, iv=nes$partyid7, plot=FALSE)
#'
#'   \dontrun{
#'   # basic usage with a plot
#'   compmeansC(dv=nes$ft.rep, iv=nes$partyid7, w=nes$wt, plot=TRUE)
#'   
#'   # basic usage: data argument used
#'   compmeansC(dv=infant.mortality, iv=region, data=world, plot=FALSE)
#'   
#'   # with weights and z variable
#'   compmeansC(dv=nes$ft.rep, iv=nes$partyid7, w=nes$wt, z=nes$gender, plot="line")
#'   compmeansC(dv=nes$ft.gay, iv=nes$gender, z=nes$partyid3, compact=TRUE, plot=TRUE)
#'   }
#' @export
#' @section RCPA3 Package Tutorial Videos:
#' * [Make Mean Comparisons with RCPA3's compmeansC Function](https://www.youtube.com/watch?v=ZuAsP0ZJuDk) 13:20, shows you how to use the RCPA3 Package's compmeansC function to compare values of an interval-level dependent variable across categories of an independent variable.  
#' * [Plotting Means with RCPA3's compmeansC Function](https://www.youtube.com/watch?v=72ROwE9zGmg) 20:42  
#' * [Making Controlled Mean Comparisons with RCPA3's compmeansC Function](https://www.youtube.com/watch?v=sGiwNUwljLc) 25:46  
#' * [Analysis of Variance with RCPA3's compmeansC Function](https://www.youtube.com/watch?v=ssBhlAHWAa4) 8:58
#' * [Complete Playlist of RCPA3 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook References:
#' * Philip H. Pollock and Barry C. Edwards, _An R Companion to Political Analysis, 3rd Edition_ (Thousand Oaks, CA: Sage Publications, Forthcoming 2022), Chapters 4, 5, 7, 10. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Thousand Oaks, CA: Sage Publications, 2020), pp.85-97, 150-156. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Making Comparisons](https://www.poliscidata.com/pages/rDemosResources.php?chapter=4), [Graphing Relationships and Describing Patterns](https://www.poliscidata.com/pages/rDemosResources.php?chapter=5), [Making Controlled Comparisons](https://www.poliscidata.com/pages/rDemosResources.php?chapter=7), and [Chi-Square Test and Analysis of Variance](https://www.poliscidata.com/pages/rDemosResources.php?chapter=10), compiled by Barry C. Edwards
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications. 
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom car Anova
#' @importFrom survey svydesign svyglm
#' @importFrom knitr kable
#' @importFrom graphics par points lines segments legend
#' @importFrom stats qt pf aov
#' @importFrom grDevices png dev.off
#' @md

compmeansC <- function (dv, iv, w, z, data, digits=2, compact=FALSE, ivlabs, zlabs, 
                        anova=FALSE, printC=FALSE, plot=TRUE, main, xlab, ylab, ylim, 
                        plot.ci=FALSE, z.palette, legend.title) 
{
  if(missing(dv) && missing(iv)) stop("Oops. You need to specify dependent and independent variables (dv and iv). You can enter help(compmeansC) to learn how this function works.")
  
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
 
  # dv <- eval(dv)
  # return(head(dv))
  if(Hmisc::all.is.numeric(dv, extras=c(NA)) | is.logical(dv)) dv = as.numeric(dv)
  check.variable(dv, vartype="numeric")
  check.variable(iv)
  if(is.numeric(iv) & (length(unique(iv)) > 12)) stop(paste(gettext("There's a problem:", domain = "R-descr"), iv.name, gettext("is numeric variable. Collapse values to fit a mean comparison table.", domain = "R-descr")))
  
  dv.label <- attr(dv, "label")  
  dv.length <- length(dv)
  dv.class <- class(dv)
  
  iv.label <- attr(iv, "label")  
  iv.length <- length(iv)
  iv.class <- class(iv)
  
  if (missing(w)) { # if no w given, then w=1 always
    w <- rep(1, dv.length)
    weighted = FALSE
  }
  else {
    w.length <- length(w)
    w.label <- attr(w, "label")  
    weighted = TRUE
    if (dv.length != w.length) stop(paste(dv.name, gettext("and", domain = "R-descr"), w.name, gettext("have different lengths.", domain = "R-descr")))
  }
  check.variable(w, vartype="numeric")
  
  if(plot!=FALSE)
  {
    plot.types = c(TRUE, "line", "bar", "points")
    if(plot=="lines") plot <- "line"
    if(plot=="bars") plot <- "bar"
    if(plot=="point") plot <- "points"
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
    check.variable(z, limitedvalues = 10)
    z.length <- length(z)
    z.label <- attr(z, "label")  
    controlled = TRUE
    if (z.length != dv.length) stop(paste(dv.name, gettext("and", domain = "R-descr"), z.name, gettext("have different lengths.", domain = "R-descr")))
  }
  else {
    z <- rep(1, dv.length)
    controlled = FALSE
  }
  
  if(!missing(ivlabs))
  {
    if(length(ivlabs) != length(unique(stats::na.omit(iv)))) stop("Oops. The iv labels aren't the right length for this iv's values. There are ", length(ivlabs), " labels for ", length(unique(stats::na.omit(iv))), " different values. To see how to use the ivlabs argument, try help(compmeansC).")
    iv.values <- ivlabs
  }
  else 
  {
    if(!is.null(levels(iv))) iv.values <- levels(iv)  else iv.values <- levels(as.factor(iv))
  }
  
  
  if(!missing(zlabs))
  {
    if(length(zlabs) != length(unique(stats::na.omit(z)))) stop("Oops. The z variable's labels aren't the right length for this z variable's values. There are ", length(zlabs), " labels for ", length(unique(stats::na.omit(z))), " different values. To see how to use the zlabs argument, try help(compmeansC).")
  }
  
  # if(controlled==TRUE) return(list(z.name, unique(z), z))
  k <- grep(FALSE, (is.na(dv) | is.na(iv) | is.na(w) | is.na(z))) # checks for missing values
  dv <- dv[k]
  iv <- iv[k]
  w  <- w[k]
  z  <- z[k]
  
      addplotmeans <- function(resultstable, plot.ci, legend.title, z.palette, controlled)
      {
        # working on plotting multiple means when there's a z variable.
        # the means will be in columns 1, 4, 7, 10...
        n.groups <- ncol(resultstable) / 3
        legend.values <- legend.lty <- legend.pch <- NULL
        # color.value <- grDevices::gray.colors(n.groups, end=.70)
        if(missing(z.palette)) lines.colors <- grDevices::gray.colors(n.groups, start=.1, end=.7)
        else lines.colors <- grDevices::hcl.colors(n.groups, palette=z.palette)
        for(i in 1:n.groups)
        {
          this.column = 3*i - 2
          pch.value = (i + 14) %% 20  # recycle pch after 20 
          # color.value = i
          lty.value = i %% 6 # recycle lty after 6
          this.group = sub("\\|", "", colnames(resultstable)[this.column])
          legend.values <- c(legend.values, sub("Mean", "", this.group))
          if(plot=="line") legend.lty <- c(legend.lty, lty.value)
          legend.pch <- c(legend.pch, pch.value)
          these.means <- resultstable[-nrow(resultstable), this.column]
          graphics::points(x=1:n.x.values.plot, y=these.means, pch=pch.value, col=lines.colors[i])
          if(plot=="line") graphics::lines(x=1:n.x.values.plot, y=these.means, col=lines.colors[i], lty=lty.value, lwd=2)
          se.means <- resultstable[-nrow(resultstable), this.column+2] / sqrt(resultstable[-nrow(resultstable), this.column+1])
          me.means <- se.means * stats::qt(.975, resultstable[-nrow(resultstable), this.column+1] - 1)
          if(plot.ci==TRUE) graphics::segments(x0=1:n.x.values.plot, x1=1:n.x.values.plot, y0=these.means-me.means, y1=these.means+me.means, col=lines.colors[i], lwd=1.5)
        }
        if(plot=="points") legend.lty <- -1
        if(controlled==TRUE & missing(legend.title)) legend.title <- z.name
        if(controlled==TRUE) graphics::legend("topleft", legend=legend.values, title=legend.title, lty=legend.lty, col=lines.colors, pch=legend.pch, bty = "n", lwd=1.5)
      }
  
      compmeans.table <- function(dv, iv, w, digits, iv.values)
      {
        resultstable <- matrix(data=NA, nrow=length(iv.values)+1, ncol=3)
        rownames(resultstable) <- c(iv.values, "Total")
        xwsum <- tapply(X=dv * w, INDEX=iv, FUN=sum)
        wsum <- tapply(X=w, INDEX=iv, FUN=sum)
        xmean <- xwsum/wsum        
        wsum[is.na(wsum)] <- 0
        b <- split(data.frame(dv, w), f=iv, drop=FALSE)
        wsd <- sapply(b, function(.df) wtd.sd(.df$dv, .df$w))
        xmean[length(xmean) + 1] <- wtd.mean(dv, w)
        wsum[length(wsum) + 1] <- sum(w)
        wsd[length(wsd) + 1] <- wtd.sd(dv, w)
        xmean <- format(xmean, drop0trailing=F, nsmall=digits, digits=digits)
        if(all(wsum[!is.na(wsum)]%%1==0)) n.drop.decimals <- TRUE else n.drop.decimals <- FALSE
        wsum <- format(wsum, drop0trailing=n.drop.decimals, nsmall=digits, digits=digits)
        wsd <- format(wsd, drop0trailing=F, nsmall=digits, digits=digits)
        calculatedvalues <- cbind(xmean, wsum, wsd)
        # print(calculatedvalues)
        # rownames(resultstable)[nrow(resultstable)] <- "Total"
        if(nrow(calculatedvalues) == nrow(resultstable)) resultstable <- calculatedvalues
         else 
           {
             for(k in 1:nrow(calculatedvalues)-1) 
               {
               this.iv <- rownames(calculatedvalues)[k]
               resultstable[this.iv,] <- calculatedvalues[k, ]
               }
             resultstable["Total",] <- calculatedvalues[nrow(calculatedvalues), ]
           }
        rownames(resultstable) <- c(iv.values, "Total")
        colnames(resultstable) <- c("Mean", "N", "St. Dev.")
        return(resultstable)
      }
      
      
  
  if(controlled==FALSE) {
    
    main.heading <- headingbox("Mean Comparison Analysis", marker="=")  
    if(printC==TRUE) printC(main.heading)

    heading <- paste("Mean Values of", dv.name , "by", iv.name)
    if(weighted==TRUE) heading <- paste(heading, ", Weighted by ", w.name, sep="")
    # if(!missing(main)) heading <- main
    
    resultstable <- compmeans.table(dv, iv, w, digits, iv.values)
    if(!missing(ivlabs)) rownames(resultstable) <- c(ivlabs, "Total")
    
    print(knitr::kable(format(resultstable, drop0trailing=F, nsmall=digits, digits=digits), format="simple", caption=heading, align="r"))
    if(printC==TRUE) printC(knitr::kable(format(resultstable, drop0trailing=F, nsmall=digits, digits=digits), format="html", align="r",
                                         caption=printCaption(heading)))
    cat("\n")
    
    
    if(anova==TRUE)
    {
      headingbox("Analysis of Variance (ANOVA)", marker="-")
      if(weighted==FALSE) 
        {
        aov.model <- stats::aov(dv ~ iv)
        aov.results.table <- round(anova(aov.model), digits)
        heading <- paste("Analysis of Variance (Response: ", dv.name, ")", sep="")
        print(knitr::kable(format(aov.results.table, drop0trailing=F, nsmall=digits, digits=digits), format="simple", caption=heading, align="r"))
        if(printC==TRUE) printC(knitr::kable(format(aov.results.table, drop0trailing=F, nsmall=digits, digits=digits), format="html", align="r",
                                             caption=printCaption(heading)))
        cat("\n")
        }
      if(weighted==TRUE) 
        {
        heading <- paste("Analysis of Variance (Response: ", dv.name, ")", sep="")
        # cat("Response:", dv.name, "\n")
        var.df <- data.frame(dv, iv, w)
        svy.design <- survey::svydesign(ids=~0, weights=~w, data=var.df)
        svyglm.model <- survey::svyglm(dv ~ iv, design=svy.design, na.action="na.omit")
        SSE_between = (svyglm.model$null.deviance - svyglm.model$deviance)
        num_df = (svyglm.model$df.null - svyglm.model$df.residual)
        MSE_between = SSE_between / num_df
        SSE_within = (svyglm.model$deviance) 
        denom_df = (svyglm.model$df.residual)
        MSE_within = SSE_within / denom_df
        f_stat <- round(MSE_between / MSE_within, digits)
        p_value <- round(stats::pf(f_stat, df1=num_df, df2=denom_df, lower.tail=FALSE), digits)
        anova.table <- data.frame(c(num_df, denom_df), round(c(SSE_between, SSE_within), digits), 
                                    round(c(MSE_between, MSE_within), digits),
                                    c(f_stat,""), c(p_value,""))
        rownames(anova.table) <- c(iv.name, "Residuals")
        colnames(anova.table) <- c("Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")
        print(knitr::kable(format(anova.table, drop0trailing=F, nsmall=digits, digits=digits), format="simple", caption=heading, align="r"))
        if(printC==TRUE) printC(knitr::kable(format(anova.table, drop0trailing=F, nsmall=digits, digits=digits), format="html", align="r",
                                             caption=printCaption(heading)))
        svy.design <- NULL
        cat("\n")
        }
    }
    
    if(plot!=FALSE) 
    {
      # this for loop lets plot be seen by user in R and then go to PNG driver
      for(k in 1:(1+as.numeric(printC)))
      {
        
      if(printC==TRUE & k==2) 
      {
        imagename <- paste("compmeansC.plot.", unclass(Sys.time()), ".png", sep="")
        grDevices::png(filename=imagename, width=4, height=3, units="in", 
                       type=getPNGtype(), pointsize=8, res=300, antialias="default")
        class(imagename) <- "image"
        printC(imagename)
      }
      
      if(missing(main)) 
      {
        main <- paste("Plot of Means of", dv.name, "by", iv.name)
        if(weighted==TRUE) main <- paste(main, ", weighted by ", w.name, sep="")
      }
      if(missing(xlab)) xlab <- paste(as.character(iv.name), "values")
      if(missing(ylab)) ylab <- paste("Means of", dv.name)
      main <- strwrap(main, width=50)

      restore.par.ask <- graphics::par("ask")
      if(printC==TRUE & k==2) graphics::par(ask=FALSE) else graphics::par(ask=TRUE)
      restore.par.mar <- graphics::par("mar")
      n.x.values.plot <- nrow(resultstable) - 1
      
      storage.mode(resultstable) <- "numeric"
      # bcs results table formatted N for display
      
      maxaxislabelsize <- max(nchar(rownames(resultstable)[1:n.x.values.plot]))
      if((maxaxislabelsize*n.x.values.plot) > 50) # will need extra room
      {
        graphics::par(mar=c(4.1 + sqrt(maxaxislabelsize + 2), 4.1, 4.1, 1.1))
        cex.ticklabels <- 0.75
        las.ticklabels <- 2 # 2
        mtext.lines <- (4.1 + sqrt(maxaxislabelsize)) - 1.5
        if(k<2) message(paste("One or more text labels for bars is long. Consider abbreviating with ivlabs argument."))
      }
      else # labels should fit w/o rotation
      {
        graphics::par(mar=c(4.1, 4.1, 3.6, 1.1))
        cex.ticklabels <- 0.9
        las.ticklabels <- 1 # 2
        mtext.lines <- 2.5
      }
      

      if(plot=="line" | plot=="points")
      {
        if(missing(ylim)) ylim <- range(dv, na.rm=T)
        plot(x="",y="", xlim=c(.5,n.x.values.plot+.5), 
             ylim=ylim,
             xlab="", ylab=ylab, axes=F, main=main)
        axis(side=1, at = 1:n.x.values.plot, labels=rownames(resultstable)[1:n.x.values.plot],
             cex.axis=cex.ticklabels, las=las.ticklabels)
        axis(side=2, las=2)
        mtext(text = xlab, side = 1, line = mtext.lines)
        box()
        # return(list(resultstable, plot.ci, controlled=FALSE))
        addplotmeans(resultstable, plot.ci, controlled=FALSE)
      }
      if(plot==TRUE | plot=="bar")
      {
        if(missing(ylim)) ylim <- NULL
        if(missing(z.palette)) bar.colors <- "gray80"
        else bar.colors <- grDevices::hcl.colors(1, palette=z.palette)
        barplot(resultstable[-nrow(resultstable), 1], 
                names.arg=rownames(resultstable)[1:n.x.values.plot],
                xlab="", ylab=ylab, las=las.ticklabels, axes=F, 
                main=main, space=.1, ylim=ylim, xpd=F,
                cex.names=cex.ticklabels, col=bar.colors)
        # axis(side=1, at = 1:n.x.values.plot, labels=)
        axis(side=2, las=2)
        mtext(text = xlab, side = 1, line = mtext.lines)
        if(!is.null(ylim)) box()
      }
      graphics::par(ask=restore.par.ask, mar=restore.par.mar)
      if(printC==TRUE & k==2) grDevices::dev.off()
      } 
    }
    
  } # end of statements for single mean comparison table
  
  
  if(controlled==TRUE) {
    
    if(!is.null(levels(z))) z.values <- levels(z)  else z.values <- unique(z)
    
    for(i in 1:length(z.values)) 
    {
      z.obs <- (z == z.values[i])
      # print(list(z.values[i], dv[z.obs], iv[z.obs], w[z.obs], digits))
      z.resultstable <- compmeans.table(dv[z.obs], iv[z.obs], w[z.obs], digits, iv.values)
      
      if(i==1) resultstable <- z.resultstable
      if(i>1)  resultstable <- cbind(resultstable, z.resultstable)
    } # end of for loop printing each controlled crosstab
    
    # class(resultstable) <- c("meanscomp", "matrix")
    main.heading <- headingbox("Controlled Mean Comparison Analysis", marker="=")  
    if(printC==TRUE) printC(main.heading)
    
    heading <- paste("Comparison of ", dv.name , " Means by ", iv.name, ", Controlling for ", z.name, sep="")
    if(weighted==TRUE) heading <- paste(heading, ", Weighted by ", w.name, sep="")
    # if(!missing(main)) heading <- main
    
    # with z var, need to modify some colnames for clarity
    for(i in 1:length(z.values))
    {
      meancol <- 1 + 3*(i-1)
      if(compact==TRUE) col.sep = "" else col.sep = "| " 
      if(missing(zlabs)) colnames(resultstable)[meancol] <- paste(col.sep, z.values[i], " Mean", sep="")
       else colnames(resultstable)[meancol] <- paste(col.sep, zlabs[i], " Mean", sep="")
    }
    if(compact==TRUE) show.cols = seq(1, ncol(resultstable), by=3) else show.cols = 1:ncol(resultstable) 
    
    if(!missing(ivlabs)) rownames(resultstable) <- c(ivlabs, "Total")
    
    print(knitr::kable(format(resultstable[ , show.cols], drop0trailing=F, nsmall=digits, digits=digits), format="simple", caption=heading, align="r")) # prints controlled mean comp table.
    if(printC==TRUE) printC(knitr::kable(format(resultstable[ , show.cols], drop0trailing=F, nsmall=digits, digits=digits), format="html", align="r",
                                         caption=printCaption(heading)))
    cat("\n")
    

    if(anova==TRUE) # with z variable, this becomes two-factor anova
    {
      headingbox("Analysis of Variance (ANOVA)", marker="-")
      if(weighted==FALSE) 
      {
        aov.model <- stats::aov(dv ~ iv + z)
        anova.results.table <- car::Anova(aov.model) # works with z and no weights
        heading <- paste("ANOVA Table, Type II Tests (Response: ", dv.name, ")", sep="")
        print(knitr::kable(format(anova.results.table, drop0trailing=F, nsmall=digits, digits=digits), format="simple", caption=heading, align="r"))
        if(printC==TRUE) printC(knitr::kable(format(anova.results.table, drop0trailing=F, nsmall=digits, digits=digits), format="html", align="r",
                                             caption=printCaption(heading)))
        cat("\n")
        slightpause()
      }
      if(weighted==TRUE)
      {
        cat("Analysis of Variance Results Table\n\n")
        cat("Response:", dv.name, "\n")
        var.df <- data.frame(dv, iv, z, w)
        # return(head(var.df))
        svy.design <- survey::svydesign(ids=~0, weights=~w, data=var.df)
        # return(svy.design)
        svyglm.model <- survey::svyglm(dv ~ iv + z, design=svy.design, na.action="na.omit")
        anova.results.table <- car::Anova(svyglm.model, test="F", method="Wald")
        heading <- paste("ANOVA Table, Type II Tests (Response: ", dv.name, ")", sep="")
        print(knitr::kable(format(anova.results.table, drop0trailing=F, nsmall=digits, digits=digits), format="simple", caption=heading, align="r"))
        if(printC==TRUE) printC(knitr::kable(format(anova.results.table, drop0trailing=F, nsmall=digits, digits=digits), format="html", align="r",
                                             caption=printCaption(heading)))
        cat("\n")
        slightpause()
      }
    }

    if(plot!=FALSE) 
    {
      # this for loop lets plot be seen by user in R and then go to PNG driver
      for(k in 1:(1+as.numeric(printC)))
      {
      if(printC==TRUE & k==2) 
      {
        imagename <- paste("compmeansC.plot.", unclass(Sys.time()), ".png", sep="")
        grDevices::png(filename=imagename, width=4, height=3, units="in", 
                       type=getPNGtype(), pointsize=8, res=300, antialias="default")
        class(imagename) <- "image"
        printC(imagename)
      }
      
      restore.par.ask <- graphics::par("ask")
      if(printC==TRUE & k==2) graphics::par(ask=FALSE) else graphics::par(ask=TRUE)
      if(missing(main)) 
      {
        main <- paste("Comparison of ", dv.name , " means by ", iv.name, ", controlling for ", z.name, sep="")
        if(weighted==TRUE) main <- paste(main, ", weighted by ", w.name, sep="")
      }
      if(missing(ylab)) ylab <- paste("Means of", dv.name)
      main <- strwrap(main, width=50)
      restore.par.mar <- graphics::par("mar")
      storage.mode(resultstable) <- "numeric"
      if(!missing(zlabs)) z.values <- zlabs
      
      # the extra room calculation is different for controlled line and bar plots
      
      if(plot=="line" | plot=="points")
      {
        if(missing(xlab)) xlab <- paste(as.character(iv.name), "values")
        n.x.values.plot <- nrow(resultstable) - 1
        maxaxislabelsize <- max(nchar(rownames(resultstable)[1:n.x.values.plot]))
      }
      if(plot==T | plot=="bar")
      {
        if(missing(xlab)) xlab <- paste(as.character(z.name), "values")
        n.x.values.plot <- length(z.values)
        maxaxislabelsize <- max(nchar(z.values))
      }
      
      if((maxaxislabelsize*n.x.values.plot) > 50) # will need extra room
      {
        graphics::par(mar=c(4.1 + sqrt(maxaxislabelsize), 4.1, 5.1, 1.1))
        cex.ticklabels <- 0.75
        las.ticklabels <- 2 # 2
        mtext.lines <- (4.1 + sqrt(maxaxislabelsize)) - 1.5
      }
      else # labels should fit w/o rotation
      {
        graphics::par(mar=c(4.1, 4.1, 3.6, 1.1))
        cex.ticklabels <- 0.9
        las.ticklabels <- 1 # 2
        mtext.lines <- 2.5
      }
      
      if(plot=="line" | plot=="points")
      {
        if(missing(ylim)) ylim <- range(dv, na.rm=T)*c(1, 1.1)
        plot(x="",y="", xlim=c(.5, n.x.values.plot+.5), ylim=ylim,
             xlab="", ylab=ylab, axes=F, main=main)
        axis(side=1, at = 1:n.x.values.plot, labels=rownames(resultstable)[1:n.x.values.plot],
             cex.axis=cex.ticklabels, las=las.ticklabels)
        axis(side=2, las=2)
        mtext(text = xlab, side = 1, line = mtext.lines)
        box()
        addplotmeans(resultstable, plot.ci, legend.title, z.palette, controlled)
      }
      if(plot==T | plot=="bar")
      {
        if(missing(ylim)) ylim <- NULL
        if(missing(z.palette)) bar.colors <- grDevices::gray.colors(nrow(resultstable) - 1, start=.1, end=.7)
         else bar.colors <- grDevices::hcl.colors(nrow(resultstable) - 1, palette=z.palette)
        plotvalues <- resultstable[-nrow(resultstable), seq(1, ncol(resultstable), by=3)]
        if(missing(legend.title)) legend.title <- iv.name
        barplot(plotvalues, names.arg=as.character(z.values), beside=T, xlim=c(0, (length(z.values)+1)*(nrow(resultstable))),
                xlab="", ylab=ylab, las=las.ticklabels, axes=F, cex.names=cex.ticklabels, 
                main=main, ylim=ylim,
                legend.text=rownames(resultstable)[-nrow(resultstable)],
                col=bar.colors, xpd=F,
                args.legend=list("topright", bty="n", cex=.75, horiz=FALSE, title=legend.title), 
                space=c(.2, 1.25))
        mtext(text = xlab, side = 1, line = mtext.lines)
        axis(side=2, las=2)
        if(!is.null(ylim)) box()
      }
      
      graphics::par(ask=restore.par.ask, mar=restore.par.mar)
      if(printC==TRUE & k==2) grDevices::dev.off()
      }
    }
    
    
  } # end of controlled mean comparison statements

  if(printC==T) printC(match.call(expand.dots = FALSE))
  invisible(resultstable)
  
}
