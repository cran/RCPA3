#' Imports common dataset file types into R environment
#' 
#' @description If you don't complete file argument, you will be prompted to select file. Supports dataset file format like Stata, SPSS, Rdata, and csv files. It previews imported data and asks you to confirm before returning a data frame. You must assign the returned data frame to an object to work with it. If `getC` doesn't support a file type, it may suggest other functions and packages for importing that type of file.  
#' @param file (Optional) Path to file you want to get and load in your R session; if you do not specify file you will be prompted to select one.
#' @param confirm (Optional) Do you want to confirm getting file before function results returned? (default: TRUE)
#' @param ... (Optional) Additional arguments passed to loading function
#' @return Dataset specified in file argument as a data frame. You must assign this returned data frame to an object to work with it.
#' @examples 
#'    library(RCPA3)
#'    
#'    # basic call will prompt user to choose file
#'    \dontrun{ 
#'    getC()
#'    }
#' @export
#' @section RCPA3 Package Tutorial Videos:
#' * [Importing Data with RCPA3 Package's getC Function](https://youtu.be/t3UhWDGqkSc), 18:14  
#' * [Complete Playlist of RCPA3 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook References:
#' * Philip H. Pollock and Barry C. Edwards, _An R Companion to Political Analysis, 3rd Edition_ (Thousand Oaks, CA: Sage Publications, Forthcoming 2022), Chapter 15. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Thousand Oaks, CA: Sage Publications, 2020), pp. 321-327. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Doing Your Own Political Analysis](https://www.poliscidata.com/pages/rDemosResources.php?chapter=15), Compiled by Barry C. Edwards
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications. 
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom haven read_por read_xpt
#' @importFrom Hmisc stata.get
#' @importFrom utils head
#' @md




getC <- function(file, confirm=TRUE, ...)
{
  main.heading <- headingbox("Importing Data into R Session", marker="=")
  
 if(missing(file))
 {
   select.file.prompt <- paste("You did not specify the file to get. Would you like to choose it now?\nEnter y to continue or enter any other key to cancel.")
   if(tolower(ask(select.file.prompt))=="y")
   {
     file <- file.choose()
     cat("\n")
   }
   else stop("You have stopped the getC function.")
 }

  if(missing(file) | length(file)==0)  stop("Oops. You need to specify the file to get using the file argument. To see how to use this function, try example(getC) or help(getC).")
  
  file.name <- deparse(substitute(file))
  file.type <- tolower(tools::file_ext(file))
  
 cat("File selected:", file.name, "\n")
 cat("File type:    ", file.type, "\n")
 
 verifyReturn <- function(dataframe, file.name)
   {
     dataframe <- data.frame(dataframe)
     if(confirm==FALSE) return(dataframe)
     message(paste("Preview of this file as a data frame:\n"))
     slightpause()
     colpreview <- min(ncol(dataframe), 3)
     print(utils::head(dataframe[, 1:colpreview]))
     cat("\nThis dataset has ", nrow(dataframe), "rows and", ncol(dataframe), "columns\n")
     # dataframe.name <- deparse(substitute(dataframe))
     slightpause()
     message("\nBefore getC returns this dataset, please verify you're not going to overwrite something.\n")
     verify.prompt <- paste("Enter y to continue or enter any other key to cancel.")
     if(tolower(ask(verify.prompt))=="y") 
     {
       cat("File returned as R data frame object. Assign getC output to object to save it.\n\n")
       return(dataframe)
     }
     else stop("You have stopped the getC function.")
   }
 
 # supported file types: csv, tab, RDS, Rdata, DIF, dta, sav, por, xpt
 if(file.type=="csv") return(verifyReturn(utils::read.csv(file=file, ...), file.name))
 if(file.type=="dif") return(verifyReturn(utils::read.DIF(file=file, ...), file.name)) 
 if(file.type=="dta") return(verifyReturn(Hmisc::stata.get(file=file, ...), file.name))
 if(file.type=="por") return(verifyReturn(haven::read_por(file=file, ...), file.name)) 
 if(file.type=="rdata" | file.type=="rda") return(verifyReturn(base::load(file=file, ...), file.name)) 
 if(file.type=="rds") return(verifyReturn(base::readRDS(file=file, ...), file.name)) 
 if(file.type=="sav") return(verifyReturn(Hmisc::spss.get(file=file, ...), file.name)) 
 if(file.type=="zsav") return(verifyReturn(haven::read_sav(file=file, ...), file.name)) 
 if(file.type=="tab") return(verifyReturn(utils::read.delim(file=file, ...), file.name)) 
 if(file.type=="xpt") return(verifyReturn(haven::read_xpt(file=file, ...), file.name)) 
 

 # file types with suggestions
 # also with foreign package: arff, dbf, epiinfo, octave
 # sas needs additional arguments passed to it, the header info is stored separately
 if(file.type=="arff") stop("Sorry, ARFF-format files not supported by getC, try using foreign::read.arff")
 if(file.type=="dbf") stop("Sorry, DBF-format files not supported by getC, try using foreign::read.dbf")
 if(file.type=="epiinfo" | file.type=="rec") stop("Sorry, Epi-format files not supported by getC, try using foreign::read.epiinfo")
 if(file.type=="mat") stop("Sorry, Matlab-format files not supported by getC, try using R.matlab package.")
 if(file.type=="mtp") stop("Sorry, Minitab-format files not supported by getC, try using foreign::read.mtp")
 if(file.type=="octave") stop("Sorry, Octave-format files not supported by getC, try using foreign::read.octave")
 if(file.type=="sas" | file.type=="sas7bdat" | file.type=="sas7bcat" | file.type=="ssd") stop("Sorry, SAS-format files not supported by getC, try haven::read_sas.\n")
 if(file.type=="systat") stop("Sorry, Systat-format files not supported by getC, try using foreign::read.systat")
 if(file.type=="xls" | file.type=="xlsx") stop(paste("Sorry, Excel-format files not supported by getC.\nSuggestions: Save your Excel file in .csv format and getC that file, or use XLConnect, readxl, or xlsx packages to read Excel-format file.\n"))
 if(file.type=="xport") stop("Sorry, SAS-format files not supported by getC, try using foreign::read.xport")

 # needs a catch-call message
 # if known/advised file types return data.frames function only gets to end if file type unknown
 cat("\nSorry \"", file.type, "\" is not a recognized data file type. See help(getC) for supported file types.\n\n", sep="")
  
}
