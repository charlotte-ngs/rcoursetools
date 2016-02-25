###
###
###
###   Purpose:   Tools related to diagrams
###   started:   2016/01/20 (pvr)
###
### ######################################## ###

#' Create an empty diagram
#'
#' @description
#' \code{create_diagram} uses a templated for a given format
#' that is expected to be stored in inst/extdata/templates
#'
#' @param psFormat   Format of diagram to be created
#' @export create_diagram
create_diagram <- function(psFormat = "odg"){
  ### # get default settings for diagrams
  lDiagramDefaults <- lGetDiagramDefaults()
  ### # determine full path and filename for diagram templates
  sDiagramTemplate <- file.path(lDiagramDefaults$sTemplatePath, psFormat, paste("template", psFormat, sep = "."))
  ### # template file must exist, otherwise we stop here
  stopifnot(file.exists(sDiagramTemplate))
  ### # depending on platform start open the template file differently
  if (.Platform$OS.type == "windows"){
    file.show(sDiagramTemplate)
  } else {
    file.edit(sDiagramTemplate)
  }
  ### # return nothing
  invisible()
}

#' Get a list with default settings related to diagrams
lGetDiagramDefaults <- function(){
  return(list(sTemplatePath = system.file(file.path("inst","extdata","templates"), package = "rcoursetools")
              ))
}

#' Insert a plot into a document using a given width scale
#'
#' @description
#' \code{genericScaledPlot} is a work-around due to the issue with setkeys{Gin}
#' which, we have to use because of the background picture in the header and which
#' has to be specified before each plot because otherwise, the plot is not visible.
#'
#' @details
#' The function \code{genericScaledPlot} can also be used to just produce the
#' LaTeX statement that sets the Gin key. This can be done when leaving
#' pfPlotMethod NULL.
#'
#' @param pData               data to be plotted
#' @param pnPaperWidthScale   scale factor for graphics width
#' @param pfPlotMethod        function that should produce the plot
#' @export genericScaledPlot
genericScaledPlot <- function(pData = NULL, pnPaperWidthScale, pfPlotMethod = NULL, ...){
  cat("\\setkeys{Gin}{width=", pnPaperWidthScale, "\\paperwidth}\n", sep = "")
  if (!is.null(pfPlotMethod))
    pfPlotMethod(pData, ...)
}
