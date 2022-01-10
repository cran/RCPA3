#' @name pass.thru.functions
#' @title Imported Functions Made Available to Package Users
#' @description A number of functions we used in our _R Companion to Political Analysis, 2nd Edition_ are not used in _R Companion to Political Analysis, 3rd Edition_.
#' For backwards compatibility, we have some functions that simply pass through arguments to a function in another package (allowing user to call them without loading additional packages).   
#' @param ... See source package help files for details on function usage.
#'




#' @rdname pass.thru.functions
#' @description The \code{\link[car]{scatterplot}} function is imported from the car package.  
#' See \code{\link[car]{scatterplot}} documentation for details.
#' @return NULL
#' @export
#' @importFrom car scatterplot
#' @keywords internal
#' @md
scatterplot <- function(...) { car::scatterplot(...) }


#' @rdname pass.thru.functions
#' @description The \code{\link[survey]{svyby}} function is imported from the survey package.  
#' See \code{\link[survey]{svyby}} documentation for details.
#' @return A svyby class object, see \code{\link[survey]{svyby}} documentation for details.
#' @export 
#' @importFrom survey svyby
#' @keywords internal
#' @md
svyby <- function(...) { survey::svyby(...) }



#' @rdname pass.thru.functions
#' @description The \code{\link[survey]{svydesign}} function is imported from the survey package.  
#' See \code{\link[survey]{svydesign}} documentation for details.
#' @return A survey.design class object, see \code{\link[survey]{svydesign}} documentation for details.
#' @export
#' @importFrom survey svydesign
#' @keywords internal
#' @md
svydesign <- function(...) { survey::svydesign(...) }



#' @rdname pass.thru.functions
#' @description The \code{\link[survey]{svyplot}} function is imported from the survey package.  
#' See \code{\link[survey]{svyplot}} documentation for details.
#' @export
#' @importFrom survey svyplot
#' @keywords internal
#' @md
svyplot <- function(...) { survey::svyplot(...) }




