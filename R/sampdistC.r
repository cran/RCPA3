#' Plots probability and cumulative density functions (PDFs and CDFs) of sample statistics
#'
#' @description Visualize expected sampling distributions for sample statistics. You can plot the probability and cumulative density functions for statistics based on either the normal distribution or a t-distribution. 
#' The `sampdistC` function also generates the confidence interval (default 95%) for a sample statistic which is useful for obtaining the CI of a summary statistic (when you're not estimating it from the dataset yourself).
#' @param stat A numeric statistic, the point estimate of a parameter based on a sample of observations, like a sample mean or a sample proportion.
#' @param se The standard error of the statistic, must be a positive number.
#' @param t.df (Optional) If critical values for sampling distribution should be based on t-distribution (generally true when statistic is a mean), set t.df to the number of degrees of freedom (typically n-1).
#' @param plot.cdf (Optional) Do you want to plot the cumulative density function? Default = FALSE (for probability density function).
#' @param ci (Optional) Specify desired confidence level for confidence interval as a percentage. Set ci=FALSE to suppress CI table (default: 95)
#' @param digits (Optional) Number of digits after decimal to display in CI table (default: 3)
#' @param printC (Optional) Do you want to sampling distribution plot to .html file in working directory? (default: FALSE)
#' @return None (makes a plot)
#' @examples  
#'   library(RCPA3)
#' 
#'   \donttest{
#'   # based on normal distributions
#'   sampdistC(stat=10, se=1)
#'   sampdistC(stat=10, se=1, plot.cdf=TRUE)
#'   
#'   # based on t-distribution with 15 degrees of freedom
#'   sampdistC(stat=8, se=2, t.df=15)
#'   sampdistC(stat=8, se=2, t.df=15, plot.cdf=TRUE)
#'   }
#' @export
#' @section Textbook References:
#' * Philip H. Pollock and Barry C. Edwards, _An R Companion to Political Analysis, 3rd Edition_ (Thousand Oaks, CA: Sage Publications, Forthcoming 2022), Chapter 8. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Thousand Oaks, CA: Sage Publications, 2020), Chapter 6. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Foundations of Inference](https://www.poliscidata.com/pages/rDemosResources.php?chapter=8), Compiled by Barry C. Edwards
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications.
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom graphics par mtext polygon segments text arrows abline axis legend box
#' @importFrom grDevices png dev.off
#' @importFrom stats qnorm qt pnorm dnorm dt pt
#' @md

sampdistC = function(stat, se, t.df, plot.cdf=FALSE, ci=95, digits=3, printC=FALSE) 
  {
  
  if (missing(stat) && missing(se)) stop("Oops. To show an expected sampling distribution, you need to complete this function's stat and se arguments. To see how to use this function, try example(sampdistC) or enter help(sampdistC)")
  old.par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old.par))
  
  check.value(stat, valuetype="numeric")
  check.value(se, valuetype="numeric")
  if(!missing(t.df)) check.value(t.df, valuetype="numeric")
  if(ci!=FALSE) check.value(ci, valuetype="numeric")
  
  # heading just a reminder of what we're doing 
  mainheading <- headingbox("Expected Distribution of Sample Statistics", marker="=")
  if(printC==TRUE) printC(mainheading)
   
  # this for loop lets plot be seen by user in R and then go to PNG driver
  for(k in 1:(1+as.numeric(printC)))
  {
    
  if(printC==TRUE & k==2) 
  {
    imagename <- paste("sampdistC.plot.", unclass(Sys.time()), ".png", sep="")
    grDevices::png(filename=imagename, width=4, height=3, units="in", 
                   type=getPNGtype(), pointsize=8, res=300, antialias="default")
    class(imagename) <- "imageonly"
    printC(imagename)
  }
  
     restore.par.ask <- graphics::par("ask")
     if(printC==TRUE & k==2) graphics::par(ask=FALSE) else graphics::par(ask=TRUE)
     
     if(ci!=FALSE) half.alpha <- (100-ci)/200 else half.alpha <- .025 

     # already checked t.df to be a number
     if(missing(t.df))
     {
       lower <- stat + stats::qnorm(.025)*se
       upper <- stat + stats::qnorm(.975)*se
       lower99 <- stat + stats::qnorm(.005)*se
       upper99 <- stat + stats::qnorm(.995)*se
       lower.otherlevel <- stat + stats::qnorm(half.alpha)*se
       upper.otherlevel <- stat + stats::qnorm(1-half.alpha)*se
     }
     else 
     {
       lower <- stat + stats::qt(.025, df=t.df)*se
       upper <- stat + stats::qt(.975, df=t.df)*se
       lower99 <- stat + stats::qt(.005, df=t.df)*se
       upper99 <- stat + stats::qt(.995, df=t.df)*se
       lower.otherlevel <- stat + stats::qt(half.alpha, df=t.df)*se
       upper.otherlevel <- stat + stats::qt(1-half.alpha, df=t.df)*se
     }
     
     # have already generated upper and lower bounds for 95% CI
     title <- paste(ci, "% Confidence Interval of a Sample Statistic", sep="")
     if(ci==95) ci.tab <- data.frame(lower, stat, upper)
     else if(ci==99) ci.tab <- data.frame(lower99, stat, upper99)
     else ci.tab <- data.frame(lower.otherlevel, stat, upper.otherlevel)
     names(ci.tab) = c("Lower Bound", "Point Estimate","Upper Bound")
     if(ci!=FALSE & k==1) print(knitr::kable(format(ci.tab, drop0trailing=F, nsmall=digits, digits=digits), format="simple", caption=title, align="r"))
     if(printC==TRUE & k==1 & ci!=FALSE) printC(knitr::kable(format(ci.tab, drop0trailing=F, nsmall=digits, digits=digits), 
                                          format="html", align="r", caption=printCaption(title)))
     cat("\n")
     slightpause()
     
     plot.over.x <- seq(lower-2*(stat-lower), upper+2*(upper-stat), by=(upper-lower)/100)
     # length(plot.over.x)
     
     if(missing(t.df)) 
     {
       if(plot.cdf==TRUE) densities.y <- stats::pnorm(q=plot.over.x, mean=stat, sd = se, lower.tail = T)
       # trying out CDF
       if(plot.cdf==FALSE) densities.y <- stats::dnorm(x=plot.over.x, mean=stat, sd = se)
     }
     else 
     {
       plot.over.t.stats <- (plot.over.x - stat) / se
       if(plot.cdf==FALSE) densities.y <- stats::dt(x=plot.over.t.stats, df=t.df)
       if(plot.cdf==TRUE)  densities.y <- stats::pt(q=plot.over.t.stats, df=t.df, lower.tail = T)
     }
       
     graphics::par(mar=c(8,4,3,1))
     
     plot(x="",y="",xlim=c(lower-1.25*(stat-lower), upper+1.25*(upper-stat)), ylim=c(0,max(densities.y)), 
          axes=F, xlab="", ylab="", main="")
     
     if (missing(t.df)) dist.title <- paste("Normal Distribution of Sample Statistics (stat=", stat, ", se=", se, ")", sep="")
     else dist.title <- paste("t-Distribution of Sample Statistics (stat=", stat, ", se=", se, ", df=", t.df, ")", sep="")
     
     graphics::mtext(text = dist.title, side = 3, line=1.25, font=2, cex=1.1)
     
     
     if(plot.cdf==F)
     {
       graphics::polygon(x=c(min(plot.over.x), plot.over.x, max(plot.over.x)), y=c(0, densities.y, 0), col="gray90")

       # identifying the area +/- 1 SE
       if (missing(t.df)) 
       {
         density.minus1se <- stats::dnorm((stat-se), mean=stat, sd = se)
         density.plus1se <- stats::dnorm((stat+se), mean=stat, sd = se)
       }
       else 
       {
         density.minus1se <- stats::dt(-1, df=t.df)
         density.plus1se <- stats::dt(+1, df=t.df)
       }
       graphics::segments(x0=stat-se, x1=stat-se, y0=0, y1=max(densities.y), lty=3, col="gray50")
       graphics::segments(x0=stat+se, x1=stat+se, y0=0, y1=max(densities.y), lty=3, col="gray50")
       graphics::polygon(x=c(plot.over.x[plot.over.x<=(stat-se)], (stat-se), (stat-se)), 
               y=c(densities.y[plot.over.x<=(stat-se)], density.minus1se, 0), col="pink")
       graphics::polygon(x=c(plot.over.x[plot.over.x>=(stat+se)], (stat+se), (stat+se)), 
               y=c(densities.y[plot.over.x>=(stat+se)], 0, density.plus1se), col="pink")
       
       # identifying the 5% sig. areas
       if (missing(t.df)) 
       {
         density.lower <- stats::dnorm(lower, mean=stat, sd = se)
         density.upper <- stats::dnorm(upper, mean=stat, sd = se)
       }
       else
       {
         density.lower <- stats::dt(stats::qt(.025, df=t.df), df=t.df)
         density.upper <- stats::dt(stats::qt(.975, df=t.df), df=t.df)
       }
       graphics::segments(x0=lower, x1=lower, y0=0, y1=max(densities.y), lty=3, col="gray50")
       graphics::segments(x0=upper, x1=upper, y0=0, y1=max(densities.y), lty=3, col="gray50")
       graphics::polygon(x=c(plot.over.x[plot.over.x<=lower], lower, lower), 
               y=c(densities.y[plot.over.x<=lower], density.lower, 0), col="red")
       graphics::polygon(x=c(plot.over.x[plot.over.x>=upper], upper, upper), 
               y=c(densities.y[plot.over.x>=upper], 0, density.upper), col="red")
       
       graphics::text(x=lower-.1*(stat-lower), y=.2*max(densities.y), labels = "2.5% of area", pos = 2, cex = .8)
       graphics::text(x=upper+.1*(upper-stat), y=.2*max(densities.y), labels = "2.5% of area", pos = 4, cex = .8)
       
       # identifying the 1% sig. areas
       if (missing(t.df)) 
       {
         density.lower99 <- stats::dnorm(lower99, mean=stat, sd = se)
         density.upper99 <- stats::dnorm(upper99, mean=stat, sd = se)
       }
       else
       {
         density.lower99 <- stats::dt(stats::qt(.005, df=t.df), df=t.df)
         density.upper99 <- stats::dt(stats::qt(.995, df=t.df), df=t.df)
       }
       graphics::segments(x0=lower99, x1=lower99, y0=0, y1=max(densities.y), lty=3, col="gray50")
       graphics::segments(x0=upper99, x1=upper99, y0=0, y1=max(densities.y), lty=3, col="gray50")
       graphics::polygon(x=c(plot.over.x[plot.over.x<=lower99], lower99, lower99), 
               y=c(densities.y[plot.over.x<=lower99], density.lower99, 0), col="darkred")
       graphics::polygon(x=c(plot.over.x[plot.over.x>=upper99], upper99, upper99), 
               y=c(densities.y[plot.over.x>=upper99], 0, density.upper99), col="darkred")
       
       graphics::text(x=lower99-.1*(stat-lower), y=.05*max(densities.y), labels = "0.5% of area", pos = 2, cex = .6)
       graphics::text(x=upper99+.1*(upper-stat), y=.05*max(densities.y), labels = "  0.5% of area", pos = 4, cex = .6)
       
       
       # 16% won't be outside +/- 1 SE if it's a t-dist
       if(missing(t.df)) outside.1se.area <- "16"
       else
       {
         outside.1se.area <- 100 * stats::pt(1, lower.tail=F, df=t.df)
         outside.1se.area <- round(outside.1se.area, 1)
       }
       graphics::text(x=stat-1.4*(se), y=.65*max(densities.y), labels =paste(outside.1se.area, "% of area", sep=""), pos = 2, cex = .9)
       graphics::text(x=stat+1.4*(se), y=.65*max(densities.y), labels =paste(outside.1se.area, "% of area", sep=""), pos = 4, cex = .9)
       # middle part won't be 68% if it's a t-distribution
       if(missing(t.df)) middle.area.percent <- "68"
       else
       {
         middle.area.percent <- 100 - 2*outside.1se.area
         middle.area.percent <- round(middle.area.percent, 1)
       }
       graphics::text(x=stat, y=.15*max(densities.y), labels =paste(middle.area.percent, "% of\narea", sep=""), pos = 3)

       graphics::arrows(x0=lower99, x1=lower99-1.25*se, y0=.09*max(densities.y), y1=.09*max(densities.y), 
              angle=15, lend=1, length=.1)
       graphics::arrows(x0=upper99, x1=upper99+1.25*se, y0=.09*max(densities.y), y1=.09*max(densities.y), 
              angle=15, lend=1, length=.1)
       
       graphics::arrows(x0=lower, x1=lower-2*se, y0=.25*max(densities.y), y1=.25*max(densities.y), 
              angle=15, lend=1, length=.1)
       graphics::arrows(x0=upper, x1=upper+2*se, y0=.25*max(densities.y), y1=.25*max(densities.y), 
              angle=15, lend=1, length=.1)
       graphics::arrows(x0=(stat-se), x1=(stat-se)-3*se, y0=.7*max(densities.y), y1=.7*max(densities.y), 
              angle=15, lend=1, length=.1)
       graphics::arrows(x0=(stat+se), x1=(stat+se)+3*se, y0=.7*max(densities.y), y1=.7*max(densities.y), 
              angle=15, lend=1, length=.1)
       graphics::mtext(text = "Probability Density", side=2, line=1.5)
       
     } # elements just for PDF
     

     if(plot.cdf==T)
     {
       
       graphics::abline(h=c(0.025, 0.975), lty=3, col="red")
       graphics::abline(h=c(0.005,  0.995), lty=3, col="darkred")
       
       if(missing(t.df)) 
       {
         cd.minus1se <- stats::pnorm((stat-se), mean=stat, sd = se, lower.tail = T)
         cd.plus1se <- stats::pnorm((stat+se), mean=stat, sd = se, lower.tail = T)
       }
       else 
       {
         cd.minus1se <- stats::pt(-1, df=t.df, lower.tail = T)
         cd.plus1se <- stats::pt(+1, df=t.df, lower.tail = T)
       }
       graphics::abline(h=c(cd.minus1se, cd.plus1se), lty=3, col="pink")
       
       graphics::polygon(x=c(min(plot.over.x), plot.over.x, max(plot.over.x)), y=c(0, densities.y, 0), 
               col="gray90", border="gray40")
       if (missing(t.df)) 
       {
         cd.lower <- stats::pnorm(lower, mean=stat, sd = se, lower.tail = T)
         cd.upper <- stats::pnorm(upper, mean=stat, sd = se, lower.tail = T)
         cd.lower99 <- stats::pnorm(lower99, mean=stat, sd = se, lower.tail = T)
         cd.upper99 <- stats::pnorm(upper99, mean=stat, sd = se, lower.tail = T)
       }
       else
       {
         cd.lower <- stats::pt(stats::qt(.025, df=t.df), df=t.df, lower.tail = T)
         cd.upper <- stats::pt(stats::qt(.975, df=t.df), df=t.df, lower.tail = T)
         cd.lower99 <- stats::pt(stats::qt(.005, df=t.df), df=t.df, lower.tail = T)
         cd.upper99 <- stats::pt(stats::qt(.995, df=t.df), df=t.df, lower.tail = T)
       }
       graphics::segments(x0=lower,x1=lower,y0=0,y1=cd.lower, lty=3, col="red")
       graphics::segments(x0=upper,x1=upper,y0=0,y1=cd.upper, lty=3, col="red")
       
       graphics::segments(x0=lower99,x1=lower99,y0=0,y1=cd.lower99, lty=3, col="darkred")
       graphics::segments(x0=upper99,x1=upper99,y0=0,y1=cd.upper99, lty=3, col="darkred")
       
       graphics::segments(x0=stat-se,x1=stat-se,y0=0,y1=cd.minus1se, lty=3, col="pink")
       graphics::segments(x0=stat+se,x1=stat+se,y0=0,y1=cd.plus1se, lty=3, col="pink")

       graphics::mtext(text = "Cumulative Density", side=2, line=3)
       graphics::axis(side = 2, line = 0, las=2, at=seq(0,1,by=.1), labels=seq(0,1,by=.1))
       
       graphics::legend(x=lower-1.25*(stat-lower), y=.7, legend=c("+/- 1 SE", "5% levels", "1% levels"),
              lty=3, col=c("pink","red","darkred"), cex=.8, bty="n")
     } # end of elements just for CDF
          
     graphics::axis(side = 1, line = 0)
     graphics::axis(side = 1, at = c(stat-4*se, stat-3*se, stat-2*se, stat-se, stat, stat+se, stat+2*se, stat+3*se, stat+4*se), 
          labels = c(-4, -3, -2, -1, 0, 1, 2, 3, 4), line=4.5)

     graphics::mtext(text = "Expected Sample Statistics (Original Scale)", side = 1, line=2.25)
     
     if (missing(t.df)) xlab <- "Standardized Statistics (Z)" else xlab <- "Standardized Statistics (t)"
     graphics::mtext(text = xlab, side = 1, line=6.5)
    
     graphics::box()
    
     graphics::par(ask=restore.par.ask)
     if(printC==TRUE & k==2) grDevices::dev.off()
  }
     if(printC==T) printC(match.call(expand.dots = FALSE))
     
}





