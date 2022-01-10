#' Sorts dataset observations by user-defined criteria to return case-level information
#'
#' @description Returns case-level information in order specified by user. You can sort by additional criteria to break ties. Useful for learning about units of analysis and selecting cases for qualitative research designs.
#' @param id A variable in the dataset (data) that identfies individual cases, typically the name of states, countries, etc.
#' @param by Variable the cases should be sorted by.
#' @param data (Optional) Dataset to be sorted.
#' @param thenby (Optional) Criteria for sorting cases after sorting with the "by" variable. Useful if many cases tied on first criteria.
#' @param descending (Optional) Should the cases be sorted in descending order?  By default, set to TRUE. When sorting ordered factors, check that the levels higher numerically correspond to the sort order you have in mind. 
#' @param limit (Optional) The number of rows to report. If there are many observations to be sorted, you may want to limit output to 5, 10, etc. rows. 
#' @param confirm (Optional) If function is going to return long table of results (more than 20 rows), you'll be asked for confirmation (use `confirm=F` to bypass).
#' @param printC (Optional) Do you want to print table of sorted observations to to .html file in working directory? (default: FALSE)
#' @return A data frame of sorted observations.
#' @examples 
#'    library(RCPA3)
#'    
#'    # basic usage
#'    sortC(id=state, by=abortlaws, data=states)
#'    
#'    # options to limit results and sort in ascending order
#'    sortC(id=country, by=gini.index, descending=FALSE, limit=10, data=world)
#'    
#'    # sort by and thenby 
#'    sortC(id=country, by=vdem.4cat, thenby=gini.index, descending=c(FALSE, FALSE), 
#'          data=world, confirm=FALSE)
#'    sortC(id=country, by=vdem.4cat, thenby=gini.index, descending=c(FALSE, TRUE), 
#'          data=world, confirm=FALSE)
#' @export
#' @section Textbook References:
#' * Philip H. Pollock and Barry C. Edwards, _An R Companion to Political Analysis, 3rd Edition_ (Thousand Oaks, CA: Sage Publications, Forthcoming 2022), Chapters 2, 6. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Thousand Oaks, CA: Sage Publications, 2020), pp. 122-123. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Descriptive Statistics](https://www.poliscidata.com/pages/rDemosResources.php?chapter=2), compiled by Barry C. Edwards
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications.  
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom Hmisc all.is.numeric
#' @importFrom knitr kable
#' @md

sortC = function(id, by, data, thenby, descending=TRUE, limit, confirm=TRUE, printC=FALSE)
  {
  if(missing(id) && (missing(by))) stop("Oops. You need to identify what you're sorting and specify what you're sorting it by using the id and by arguments. Try example(sortC) or help(sortC) for assistance with this function.")

  id.name <- deparse(substitute(id))
  by.name <- deparse(substitute(by))
  if(!missing(thenby)) thenby.name <- deparse(substitute(thenby))
  
  if(id.name=="states" | id.name=="gss" | id.name=="nes" | id.name=="world")
  {
    message("It looks like you're using old sortC syntax. The order of arguments has changed.\nTry example(sortC) or help(sortC) for assistance with this function.")
  }
  
  if(!missing(data))
  {
    if(is.matrix(data)) data <- data.frame(data)
    id  <- vector.from.data(substitute(id), data)
    by <- vector.from.data(substitute(by), data)
    if(!missing(thenby)) thenby <- vector.from.data(substitute(thenby), data)
  }
  
  check.variable(id)
  check.variable(by)
  if(!missing(thenby)) 
  {
    check.variable(thenby) 
    if(length(descending)==1) descending <- c(descending, TRUE)    
  }
  
  if(!missing(thenby)) thisorder = order(by, thenby, na.last=NA, decreasing=descending)
  if(missing(thenby)) thisorder = order(by, na.last=NA, decreasing=descending)
    # thisorder is a numeric vector
    # if by is factor with levels, the obs sorted by level
    
    if(!missing(data)) 
    {
      if(missing(thenby)) obj1	<- data[, c(id.name, by.name)]
      if(!missing(thenby)) obj1	<- data[, c(id.name, by.name, thenby.name)]
    }
    if(missing(data)) 
    {
      if(missing(thenby)) 
        {
        obj1 <- data.frame(id, by)
        colnames(obj1) <- c(id.name, by.name)      
        }
      if(!missing(thenby)) 
      {
        obj1 <- data.frame(id, by, thenby)
        colnames(obj1) <- c(id.name, by.name, thenby.name)      
      }
    }

    obj1sort <- data.frame(obj1[thisorder, ])
    rownames(obj1sort) <- NULL
    # return(attributes(obj1sort))
    
    main.heading <- headingbox("Sorted Case-Level Information", width=75, marker="=")
    if(printC==TRUE) printC(main.heading)
    
    if(missing(limit)) limit <- nrow(obj1sort)
    
    if(confirm!=FALSE)
    {
      if(limit > 20) 
      {
        select.limit.prompt <- paste("The sorted table has", limit, "rows. Do you want to see them all?\nIf you want to (further) limit the number of rows, enter a number now, enter q to cancel, or any other key to continue.")
        newlimit <- ask(select.limit.prompt)
        if(Hmisc::all.is.numeric(newlimit)) limit <- as.numeric(newlimit)
        # return(limit)
        if(newlimit=="q" | newlimit=="Q") stop("You have stopped the getC function.")
      }
      check.value(limit, valuetype="numeric")
    }
    
    if(missing(thenby))
    {
      if(descending==TRUE) order.type <- "descending" else order.type <- "ascending"
      table.caption <- paste(id.name, "values sorted by", order.type, "values of", by.name)
    }
    if(!missing(thenby))
    {
      table.caption <- paste(id.name, "values sorted by")
      if(descending[1]==TRUE) by.type <- "descending" else by.type <- "ascending"
      table.caption <- paste(table.caption, by.type, "values of", by.name)
      if(descending[2]==TRUE) thenby.type <- "descending" else thenby.type <- "ascending"
      table.caption <- paste(table.caption, "and then by", thenby.type, "values of", thenby.name)
      
    }
    
    print(knitr::kable(obj1sort[1:limit,], format="simple", caption = table.caption))
    if(printC==TRUE) printC(knitr::kable(obj1sort[1:limit,], format="html", 
                                         caption = printCaption(table.caption)))
    cat("\n")
    
    if(printC==T) printC(match.call(expand.dots = FALSE))
    invisible(obj1sort[1:limit,])
}




