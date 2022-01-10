#' Compares full v. reduced logistic regression models with Chi-Squared test
#' 
#' @description Compares full and reduced logistic regression models with Chi-Square Test to assess whether additional variables of full model are statistically significant.
#' @param reduced The reduced logistic regression model as an object. This is model with fewer independent variables. 
#' @param full The full logistic regression model as an object. This is model with more independent variables.
#' @param digits (Optional) The number of digits to display after decimal point, default is 3.
#' @return The chi-squared statistic, df, and p-value as a vector of numbers to test null hypothesis that full model no better than reduced model.
#' @examples 
#'    library(RCPA3)
#'    
#'    \donttest{
#'    model_full <- logregC(battleground2020 ~ vep16.turnout + adv.or.more, data=states)
#'    model_reduced <- logregC(battleground2020 ~ vep16.turnout, data=states)
#'    
#'    pchisqC(reduced=model_reduced, full=model_full)
#'    }
#' @export
#' @section Textbook References:
#' * Philip H. Pollock and Barry C. Edwards, _An R Companion to Political Analysis, 3rd Edition_ (Thousand Oaks, CA: Sage Publications, Forthcoming 2022), Chapter 14. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Thousand Oaks, CA: Sage Publications, 2020), Chapter 9. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @keywords internal
#' @importFrom methods is
#' @importFrom stats pchisq
#' @md



pchisqC = function(reduced, full, digits=3)
{
  if(missing(reduced) && missing(full))  stop("Opps. You need to specify the models to be compared using reduced and full arguments. To see how to use this function, try example(pchisqC) or help(pchisqC).")
  
  # check if user supplied glm objects 
  if(methods::is(reduced, "glm")!=TRUE) message("Warning: Reduced model is not a glm object.")
  if(methods::is(full, "glm")!=TRUE) message("Warning: Full model is not a glm object.")
  chisquared_stat = reduced$deviance - full$deviance
  n_diff_warning = "Warning: These models have different numbers of observations."
  if(reduced$df.null != full$df.null) { message(n_diff_warning) }
  df = length(full$coefficients) - length(reduced$coefficients)
  pvalue = 1 - (stats::pchisq(chisquared_stat, df))
  # if (pvalue==0) { pvalue = "<.001"}
  results    <- round(c(Chi2 = chisquared_stat, df = df, p = pvalue), digits)
  names(results) = c("Chi-Squared","    DF","    P-Value")
  return(results)
}