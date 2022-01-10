#' Welcomes new users to package with basic information, option to reset user's working environment
#'
#' @description Welcomes users to RCPA3 package for _An R Companion to Political Analysis, 3rd Edition_ and provides basic information about using Companion functions and datasets.
#' @param reset (Optional) Do you want to remove objects from your workspace and restore default graphical parameters? Default is FALSE. Removing workspace objects and restoring default graphical parameters can help undo some unintended side-effects of past work.  
#' @return No value returned
#' @examples  
#'   library(RCPA3)
#'   
#'   # Welcome message from RCPA3 package.
#'   \dontrun{
#'   welcome()
#'   }
#' @export
#' @section Textbook Reference:
#' * Philip H. Pollock and Barry C. Edwards, _An R Companion to Political Analysis, 3rd Edition_ (Thousand Oaks, CA: Sage Publications, Forthcoming 2022), Chapter 1. 
#' @section Online Resources:
#' * [R Tutorials & Resources for Getting Started with R](https://www.poliscidata.com/pages/rDemosResources.php?chapter='I'), Compiled by Barry C. Edwards 
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications. 
#' @importFrom graphics par
#' @importFrom grDevices dev.new dev.off
#' @md


welcome <- function(reset=FALSE)
  {

  slowConsolePrint(slow=.006, "    __      __          _                                    \n")
  slowConsolePrint(slow=.006, "    \\ \\    / / ___     | |     __      ___    _ __     ___   \n")
  slowConsolePrint(slow=.006, "     \\ \\/\\/ / / -_)    | |    / _|    / _ \\  | '  \\   / -_)  \n")
  slowConsolePrint(slow=.006, "      \\_/\\_/  \\___|   _|_|_   \\__|_   \\___/  |_|_|_|  \\___|  \n")
  # slowConsolePrint(slow=.006, "    _|\"\"\"\"\"|_|\"\"\"\"\"|_|\"\"\"\"\"|_|\"\"\"\"\"|_|\"\"\"\"\"|_|\"\"\"\"\"|_|\"\"\"\"\"| \n")
  # slowConsolePrint(slow=.006, "    H`-0-0-I\"`-0-0-K\"7-0-0-M\"`-0-0-'\"`-0-0-'\"`-0-0-'\"`-0-0-A \n")

    cat("\n")
    
    slightpause()
    slowConsolePrint("The RCPA3 package bundles datasets and functions featured in ")
    slowConsolePrint("An R Companion to Political Analysis, 3rd Edition, ")
    slowConsolePrint("by Philip H. Pollock III and Barry C. Edwards.\n\n")
    slightpause()

    slowConsolePrint(paste("Your current working directory is:", getwd(),"\n"))
    slowConsolePrint("Use the setwd() function to change your working directory.\n\n")
    slightpause()
    slowConsolePrint("This package contains four datasets (debate, nes, states, and world) and many functions.\n")
    slowConsolePrint("To see a list of all objects in this package, enter ls(\"package:rcpa3\")\n\n")
    
    slowConsolePrint("You can enter welcome(reset=TRUE) to clear workspace objects and restore default graphical parameters. ")
    slowConsolePrint("For help with this function, or any other R function, type ? followed by the function's name, or help(function_name)\n\n")
    
      # slowConsolePrint("These are the functions and objects in the poliscidata package:\n")
      # slightpause()
      # packagecontents <- utils::capture.output(print(ls("package:poliscidata")))
      # for(i in 1:length(packagecontents)) quickConsolePrint(paste(packagecontents[i], "\n"))
      # slightpause()
      
    slowConsolePrint("If you want to play Widget Factory, just type widgetFactory() then press enter.\n\n")
    message("We hope you enjoying using the rcpa3 package!\n")
    slightpause()
    
    if(reset==TRUE)
    {
      # restore par to default settings
      grDevices::dev.new()
      op <- par(no.readonly = TRUE)
      grDevices::dev.off()
      graphics::par(op)
      # remove all objects from global environment
      rm(list=ls(all.names=TRUE, envir=globalenv()), envir=globalenv()) # try to clean-up working environment to get started
    }
    
}
