#' Prints table of results to a .html file in local working directory
#'
#' @description Prints table or summary of results to a .html file in local working directory. Converting Console format tables to .html tables helps users quickly create publication- and presentation-ready tables. The .html file's name is displayed as Console message. 
#' Current date added to Table.Output.html file name to keep output organized. You can print output directly from Companion functions using `printC=TRUE` argument (where available). 
#' @param objx A table or data frame. The table must be html-ready, not all Console output is organized in tables. If objx is not a html-ready table, `printC` will write it as preformatted text to the .html file in the working directory.
#' @param file (Optional) The path/file name for .html output. If not specified, function will output to .html file in your working directory.
#' @return No return to R. The formatted objx is outputted to a .html file in working directory. 
#' @examples 
#'    library(RCPA3)
#'    
#'    example.table <- freqC(x=world$vdem.4cat, plot=FALSE)
#'    # running printC will generate a .html file in your working directory
#' \donttest{
#'    printC(example.table, file=tempfile(fileext = ".html"))
#' }
#' @export
#' @section Textbook Reference:
#' * Philip H. Pollock and Barry C. Edwards, _An R Companion to Political Analysis, 3rd Edition_ (Thousand Oaks, CA: Sage Publications, Forthcoming 2022), Chapter 1. 
#' @importFrom knitr kables
#' @importFrom utils capture.output
#' @md


printC <- function(objx, file)
        { 
          if(missing(objx))  stop("Opps. You need to specify what you want the printC function to print using the objx argument. To see how to use this function, try example(printC) or help(printC).")
          
          if(missing(file)) 
            {
            outputfile <- paste("Table.Output.", format(Sys.Date(), "%b%d%y"), ".html", sep="")
            location <- paste("in", getwd())
            }          
          else 
            {
              outputfile <- file
              location <- ""
            }
          
  
          # check if output file exists, if not, create one with basic css guide for formatting
          if(!file.exists(outputfile)) cat("<head><style> body { margin-left: 5%; margin-right: 5%; font-family: verdana; } h2 { font-size: 22px; } h3 { font-size: 16px; } h4 { font-size: 14px; line-height: 1.1; } p { font-size: 12px; } tr:nth-child(even) { background: #F7F7F7 } </style></head>\n\n", file = outputfile, append = TRUE)            

          if(class(objx) == "knitr_kable")
          {
            objx <- gsub("<table>", "<table style=\"border-spacing: 0px; border-collapse: collapse; min-width: 400px;\">", objx)
            objx <- gsub("<th style=\"text-align:left;\">", "<th style=\"text-align: left; font-size: 14px; background-color: #EFEFEF; border-bottom: 1.5px dotted black; padding: 4px;\">", objx)
            objx <- gsub("<th style=\"text-align:right;\">", "<th style=\"text-align: right; font-size: 14px; background-color: #EFEFEF; border-bottom: 1.5px dotted black; padding: 4px;\">", objx)
            objx <- gsub("<td style=\"text-align:right;\">", "<td style=\"text-align: right; font-size: 14px; padding: 2px;\">", objx)
            objx <- gsub("<td style=\"text-align:left;\">", "<td style=\"text-align: left; font-size: 14px; padding: 2px;\">", objx)
            objx <- gsub("<caption><h3 style=\"color: #404040;\"><nobr>", "<caption><h3 style=\"color: #404040;\">", objx)
            objx <- gsub("</nobr><BR></h3></caption>", "<BR></h3></caption>", objx)
            objx <- gsub("</nobr><BR><nobr>", " ", objx)
            #  onMouseOver=\"this.style.backgroundColor='lightyellow'\" onMouseOut=\"this.style.backgroundColor='#FFF'\"            
            cat("<div style=\"border: 1px solid black; border-radius: 4px; padding-right: 6px; padding-left: 6px; padding-top: 0px; display: inline-block; margin: 10px;\">\n", file = outputfile, append = TRUE)
            write(knitr::kables(list(objx), format="html"), file=outputfile, append=TRUE)
            cat("</div><BR>\n", file = outputfile, append = TRUE)
            message(paste("Table appended to", outputfile, location))
          }
          else if(class(objx) == "character")
          {
            objx[1] <- paste("<strong>", objx[1], "</strong>", sep="")
            # gray tables: background-color:whitesmoke;border:1px solid lightgray;
            # yellow tables: background-color:#FFFFCC;border:1px solid gold;
            # blue tables: azure and lightskyblue
            cat("<div style=\"background-color: white; margin: 10px; padding:0px 10px 0px 10px; display: inline-block; border: 1px solid gray; border-radius: 4px;\">\n<PRE style=\"font-size: 14px;\">", 
                file = outputfile, append = TRUE)
            blob <- paste(objx)
            write(blob, file=outputfile, append=TRUE)
            cat("</PRE></div><BR>\n", file = outputfile, append = TRUE)
            message(paste("Output appended to", outputfile, location))
          }
          else if(class(objx) == "statement")
          {
            objx[1] <- paste("<strong>", objx[1], "</strong>", sep="")
            # gray tables: background-color:whitesmoke;border:1px solid lightgray;
            # yellow tables: background-color:#FFFFCC;border:1px solid gold;
            # blue tables: azure and lightskyblue
            cat("<div style=\"background-color: #F5F5F5; margin: 10px; padding:0px 10px 0px 10px; display: inline-block; border: 1px solid black; border-radius: 4px;\"><PRE style=\"font-size: 14px; font-family: verdana;\">", 
                file = outputfile, append = TRUE)
            blob <- paste(objx)
            write(blob, file=outputfile, append=TRUE)
            cat("</PRE></div><BR>\n", file = outputfile, append = TRUE)
            message(paste("Statement appended to", outputfile, location))
          }
          else if(class(objx) == "banner.heading")
          {
            banner.color <- sample(grDevices::hcl.colors(50, palette="Pastel"), size=1)
            cat("<hr><div style=\"background-color:", banner.color, "; border-radius: 5px; width: 100%; text-align: center; margin: 2px;\"><h2>", objx[2], "</h2></div>\n\n", file = outputfile, append = TRUE)
            # this div closed after notes printed...
            cat("<div style=\"background-color: white; min-height: 280px; position: relative; width: 100%; margin: 0px;\">", file = outputfile, append = TRUE)
          }
          else if(class(objx) == "call")
          {
            # <div style=\"background-color: orange; padding: 2px; width: 100%; font-size: 12px; text-align: left;\">
            # "</div>", 
            # this closes div that contains everything under banner
            cat("</div><BR CLEAR=ALL>", file = outputfile, append = TRUE)
            citation <- utils::capture.output(citation("RCPA3"))
            objx <- utils::capture.output(objx)
            cat("<p>----------------<BR><i>Notes:</i> Output generated ", format(Sys.time(), "%a %b %d %X %Y")," with R command RCPA3::", objx, ". </p>", file = outputfile, append = TRUE, sep="")
            # ", citation[2:7], ".
          }
          else if(class(objx) == "image")
          {
            # this system should work so long as there's not more than one plot per call
            cat("<div style=\"float: right; text-align: center; position: absolute; top: 0px; right: 0px;\">", file = outputfile, append = TRUE, sep="")
            cat("<A HREF=\"", objx, "\"><img  src=\"", objx, "\" border=0 alt=\"R plot\" width=\"300\"><p>Click to enlarge</p></A></div>", file = outputfile, append = TRUE, sep="")
            message(paste("Image file added to", outputfile, location))
          }
          else if(class(objx) == "imageonly")
          {
            cat("<div style=\"text-align: center; width: 100%;\">", file = outputfile, append = TRUE, sep="")
            cat("<img  src=\"", objx, "\" border=0 alt=\"R plot\">", file = outputfile, append = TRUE, sep="")
            cat("</div>", file = outputfile, append = TRUE, sep="")
            message(paste("Image file added to", outputfile, location))
          }
          else 
          {
            objx <- utils::capture.output(objx)
            cat("<div style=\"background-color: white; margin: 10px; padding:0px 10px 0px 10px; display: inline-block; border: 1px solid gray; border-radius: 4px;\">\n<PRE style=\"font-size: 14px;\">", 
                file = outputfile, append = TRUE)
            blob <- paste(objx)
            write(blob, file=outputfile, append=TRUE)
            cat("</PRE></div><BR>\n", file = outputfile, append = TRUE)
            message(paste("Output appended to", outputfile, location))
          }

        

        }



