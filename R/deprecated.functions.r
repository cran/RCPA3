#' @name deprecated.functions
#' @title Deprecated functions (and how to still use them)
#' @description A number of functions we used in our _R Companion to Political Analysis, 2nd Edition_ are not used in _R Companion to Political Analysis, 3rd Edition_.
#' These deprecated functions have been superseded by new and improved functions in the 3rd Edition. This page lists the deprecated functions and provides reference information for anyone used to using these functions.  
#' @export
#' @md


#' @rdname deprecated.functions
#' @description * **AdjR2** calculated adjusted R-Squared statistic. 
#' This function was called after estimating a (weighted) model that did not report adjusted R-Squared statistic. 
#' Adjusted R-Square is now calculated automatically by the \code{\link{regC}} and \code{\link{logregC}} functions.
#' @export
#' @keywords internal
#' @md
AdjR2 = function()
{
  message("The AdjR2 function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("Adjusted R-Square is calculated automatically by regC and logregC functions.")
}


#' @rdname deprecated.functions
#' @description * **CI95** calculated the 95 percent confidence interval of a sample statistic (returning the upper and lower boundaries).  
#' The \code{\link{CIprop}} and \code{\link{CImean}} functions generate confidence intervals for sample proportions and sample means (default confidence level=95).
#' To find confidence interval of difference of sample proportions, use \code{\link{testpropsC}}.
#' To find confidence interval of difference of sample means, use \code{\link{testmeansC}}. 
#' @export
#' @keywords internal
#' @md
CI95 = function() 
{
  message("The CI95 function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("Please use CImean and/or CIprop functions instead with level=95.")
}


#' @rdname deprecated.functions
#' @description * **CI99** calculated the 99 percent confidence interval of a sample statistic (returning the upper and lower boundaries).  
#' The \code{\link{CIprop}} and \code{\link{CImean}} functions generate confidence intervals for sample proportions and sample means (use argument level=99).
#' To find confidence interval of difference of sample proportions, use \code{\link{testpropsC}} with argument ci.level=99.
#' To find confidence interval of difference of sample means, use \code{\link{testmeansC}} with argument ci.level=99. 
#' @export
#' @keywords internal
#' @md
#' 
CI99 = function()
{
  message("The CI99 function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("Please use CImean and/or CIprop functions instead with level=99 argument.")
}


#' @rdname deprecated.functions
#' @description * **colPercents** calculated column percentages tables for multi-dimensional controlled cross-tabulations, based on function by John Fox. Was used by the xtabC function.
#' Column percentages are calculated automatically by the \code{\link{crosstabC}} function.
#' @export
#' @keywords internal
#' @md
colPercents = function() 
{
  message("The colPercents function is not used in R Companion to Political Analysis, 3rd Edition.")
}


#' @rdname deprecated.functions
#' @return Returned a list of statistics
#' @description * **compADPQ** calculated a statistic for measure of association in cross-tabulation. This function was called by tablesomersDC function (also deprecated).
#' The Somers' d measure of association is calculated by \code{\link{crosstabC}} function with somers=T argument.
#' @export
#' @keywords internal
#' @md
compADPQ = function()
{
  message("The compADPQ function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("This deprecated function has been fully integrated into crosstabC with somers=T argument.")
}


#' @rdname deprecated.functions
#' @description * **compmeans** was used for mean comparison analysis. The compmeans function was imported from descr package,
#' See \code{\link[descr]{compmeans}} documentation for details.
#' For mean comparison analysis, we now feature \code{\link{compmeansC}} which extends the functionality of compmeans to controlled comparisons, graphings, and analysis of variance.
#' @export
#' @keywords internal
#' @md
compmeans <- function() 
{
  message("The compmeans function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("This function has been superceded by the compmeansC function.")
  message("To continue to use the compmeans function from 2nd Edition, use poliscidata package.")
}



#' @rdname deprecated.functions
#' @description * **crosstab** function was imported from the descr package.
#' See \code{\link[descr]{crosstab}} documentation for details.
#' @export
#' @keywords internal
#' @md
crosstab <- function() 
{
  message("The crosstab function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("This function has been superceded by the crosstabC function.")
  message("To continue to use crosstab function from 2nd Edition, use poliscidata package.")
}


#' @rdname deprecated.functions
#' @description * **csv.get** was used to import files in comma separated value (csv) format. This function was imported from the Hmisc package.  
#' See \code{\link[Hmisc]{csv.get}} documentation for details.
#' @export
#' @keywords internal
#' @md
csv.get <- function() 
{
  message("The csv.get function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("This function has been superceded by the getC function.")
  message("To continue to use csv.get function from 2nd Edition, use poliscidata package.")
}


#' @rdname deprecated.functions
#' @description * **cut2** was used to recode numeric variables into ordered factors. This function was imported from the Hmisc package.  
#' See \code{\link[Hmisc]{cut2}} documentation for details.
#' @export
#' @keywords internal
#' @md
cut2 <- function() 
{
  message("The cut2 function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("This function has been superceded by the transformC function with type='cut' argument.")
  message("To continue to use cut2 function from 2nd Edition, use poliscidata package.")
}


#' @rdname deprecated.functions
#' @description * **describe** was used to generate descriptive statistics. This function is imported from the Hmisc package.  
#' See \code{\link[Hmisc]{describe}} documentation for details.
#' @export
#' @keywords internal
#' @md
describe <- function() 
{
  message("The describe function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("This function has been superceded by the describeC function with type='cut' argument.")
  message("To continue to use describe function from 2nd Edition, use poliscidata package.")
}


#' @rdname deprecated.functions
#' @description * **ddply** was used to generate summary statistics prior to graphing. The function was imported from the plyr package.  
#' See plyr package's ddply function documentation for details.
#' @export
#' @keywords internal
#' @md
ddply <- function() 
{
  message("The ddply function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("To continue to use ddply function from 2nd Edition, use poliscidata package.")
}


#' @rdname deprecated.functions
#' @description * **freq** was used for frequency distribution analysis. This function is imported from the descr package. 
#' See \code{\link[descr]{freq}} documentation for details.
#' @export
#' @keywords internal
#' @md
freq <- function() 
{
  message("The freq function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("This function has been superceded by the freqC function with type='cut' argument.")
  message("To continue to use freq function from 2nd Edition, use poliscidata package.")
}


#' @rdname deprecated.functions
#' @description * **imeansC** produced a controlled mean comparison table. Rows defined by values of independent variable and columns defines by values of the control variable.  
#' @export
#' @keywords internal
#' @md
imeansC = function()
{
  message("The imeansC function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("Thi function has been superceded by the compmeansC function with z argument.")
}


#' @rdname deprecated.functions
#' @description * **iplotC** generated an interaction plot used brkdn.plot function from plotrix package and svyby function from survey package. Warnings suppressed so the function creates plot without generating intermediate results used to create plot.
#' @export
#' @keywords internal
#' @md
iplotC = function()
{
  message("The iplotC function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("This function has been superceded by the compmeansC function.")
  message("For means plots with multiple lines, use compmeansC function with z and plot=TRUE arguments.")
}






#' @rdname deprecated.functions
#' @description * **plotmeans** was used to plot mean values to visualize mean comparisons. This function was imported from the gplots package.  
#' See gplots package's plotmeans function documentation for details.
#' @export
#' @keywords internal
#' @md
plotmeans <- function() 
{
  message("The plotmeans function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("This function has been superceded by the compmeansC function with plot=T argument.")
  message("For means plots with multiple lines, use compmeansC function with z argument and plot=T.")
  message("To continue to use plotmeans function in 2nd Edition, use poliscidata package.")
}



#' @rdname deprecated.functions
#' @description * **plotmeansC** plotted the mean values of a dependent variable at different values of nominal or ordinal independent variable. Made use of survey package and gplot package's plotmeans function.
#' @export
#' @keywords internal
#' @md
plotmeansC = function()
{
  message("The plotmeansC function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("This function has been superceded by the compmeansC function with plot=T argument. For means plots with multiple lines, use compmeansC function with z argument and plot=T.")
}


#' @rdname deprecated.functions
#' @description * **prop.testC** was used to test differences of proportions. Deprecated function to test difference of proportions. 
#' @export
#' @keywords internal
#' @md
prop.testC = function()
{
  message("The prop.testC function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("Use the testpropsC function instead.")
}


#' @rdname deprecated.functions
#' @description * **SetTextContrastColor** determined whether black or white better contrasted with a given color. Used by the \code{\link{Colors}} function.  
#' @export
#' @keywords internal
#' @md
#'
SetTextContrastColor = function()
{
  message("The SetTextContrastColor function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("This function has been incorporated into the Colors function.")
}


#' @rdname deprecated.functions
#' @description * **somersD** calculated the Somers' D measure of association statistic.  This function made use of the svytable function from the survey package.
#' @export
#' @keywords internal
#' @md
somersD = function() 
{
  message("The somersD function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("This function has been superceded by the crosstabC function with somers=TRUE argument.")
}


#' @rdname deprecated.functions
#' @description * **spss.get** was used to import datasets in SPSS format. The spss.get function is imported from the Hmisc package.  
#' See \code{\link[Hmisc]{spss.get}} documentation for details.
#' @export
#' @keywords internal
#' @md
spss.get <- function() 
{
  message("The spss.get function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("This function has been superceded by the getC function.")
  message("To continue to use the spss.get function from 2nd Edition, use poliscidata package.")
}


#' @rdname deprecated.functions
#' @description * **stata.get** was used to import Stata-format datasets. The stata.get function is imported from the Hmisc package.  
#' See \code{\link[Hmisc]{stata.get}} documentation for details.
#' @export
#' @keywords internal
#' @md
stata.get <- function() 
{
  message("The stata.get function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("This function has been superceded by the getC function.")
  message("To continue to use the stata.get function from 2nd Edition, use poliscidata package.")
}


#' @rdname deprecated.functions
#' @description * **svyboxplot** was used to make a box plot with option for weighted observations. The svyboxplot function is imported from the survey package.  
#' See \code{\link[survey]{svyhist}} documentation for details.
#' @export
#' @keywords internal
#' @md
svyboxplot <- function()
{
  message("The svyboxplot function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("Please use boxplotC function, specifying weights with w argument.")
}


#' @rdname deprecated.functions
#' @description * **svychisq** conducted a Chi-Square test with weighted observations. This function reports chi-squared test statistic based on weighted dataset
#' @export
#' @keywords internal
#' @md
svychisqC = function()
{
  message("The svychisqC function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("Please use crosstabC function, specifying weights with w argument and with chisq=TRUE argument.")
}


#' @rdname deprecated.functions
#' @description * **svychisq** was used to implement a Chi-Square test with weighted observations. The svychisq function is imported from the survey package.  
#' See \code{\link[survey]{svychisq}} documentation for details.
#' @export
#' @keywords internal
#' @md
svychisq = function()
{
  message("The svychisq function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("Please use crosstabC function, specifying weights with w argument and with chisq=TRUE argument.")
}


#' @rdname deprecated.functions
#' @description * **svyglm** was used to estimate logistic regression model with weighted observations. The svyglm function is imported from the survey package.  
#' See \code{\link[survey]{svyglm}} documentation for details.
#' @export
#' @keywords internal
#' @md
svyglm = function()
{
  message("The svyglm function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("Please use logregC function, specifying weights with w argument.")
}


#' @rdname deprecated.functions
#' @description * **svymean** was used to calculate sample mean for weighted observations. The svymean function is imported from the survey package.  
#' See \code{\link[survey]{surveysummary}} documentation for details.
#' @export
#' @keywords internal
#' @md
svymean <- function()
{
  message("The svymean function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("Please use wtd.mean or describeC functions, specifying weights with w argument.")
}


#' @rdname deprecated.functions
#' @description * **svytable** was used to generate cross-tabulations with weighted observations. The svytable function is imported from the survey package.  
#' See survey package documentation for details.
#' @export
#' @keywords internal
#' @md
svytable <- function()
{
  message("The svytable function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("Please use crosstabC function, specifying weights with w argument.")
}


#' @rdname deprecated.functions
#' @description * **wtd.boxplot** generated a box plot with option for weighted observations. Deprecated function to generate box plot 
#' @export
#' @keywords internal
#' @md
wtd.boxplot = function()
{
  message("The wtd.boxplot function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("Use the boxplotC function instead.")
}


#' @rdname deprecated.functions
#' @description * **wtd.chi.sq** was used for a Chi-Square test with weighted observations. The wtd.chi.sq function is imported from the weights package.  
#' See \code{\link[weights]{wtd.chi.sq}} documentation for details.
#' @export
#' @keywords internal
#' @md
wtd.chi.sq <- function()
{
  message("The wtd.chi.sq function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("Use the crosstabC function instead, with w argument for weights variable and chisq=TRUE.")
}


#' @rdname deprecated.functions
#' @description * **wtd.t.test** was used to conduct t-tests on sample means with weighted observations. This function is imported from the weights package.  
#' See \code{\link[weights]{wtd.t.test}} documentation for details.
#' @export
#' @keywords internal
#' @md
wtd.t.test <- function()
{
  message("The wtd.t.test function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("Use the testmeansC function instead, with w argument for weights variable.")
}


#' @rdname deprecated.functions
#' @description * **wtd.ttestC** was used to test difference of means using t-test. Deprecated function to test difference of means. 
#' @export
#' @keywords internal
#' @md
wtd.ttestC = function()
{
  message("The wtd.ttestC function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("Use the compmeansC function instead.")
}


#' @rdname deprecated.functions
#' @description * **xtabC** did controlled cross tabulation analysis with optional weights.
#' @export
#' @keywords internal
#' @md
xtabC = function()
{
  message("The xtabC function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("This function has been superceded by the crosstabC function. Use crosstabC with z argument for controlled cross-tabulations.")
}


#' @rdname deprecated.functions
#' @description * **xtp** generated cross-tabulation of two categorical variables and produced a mosaic plot. Made use of the crosstab function in the descr package.
#' @export
#' @keywords internal
#' @md
xtp = function() 
{
  message("The xtp function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("Please use crosstabC function instead.")
}



#' @rdname deprecated.functions
#' @description * **xtp.chi2** did cross tabulation analysis with Chi-Square test. This function made use of the crosstab function from the descr package
#' @export
#' @keywords internal
#' @md
xtp.chi2 = function() 
{
  message("The xtp.chi2 function is not used in R Companion to Political Analysis, 3rd Edition.")
  message("Please use crosstabC function with argument chisq=TRUE instead.")
}
