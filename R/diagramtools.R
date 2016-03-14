###
###
###
###   Purpose:   Tools related to diagrams
###   started:   2016/01/20 (pvr)
###
### ######################################## ###

#' Create an empty odg graphic
#'
#' @description
#' \code{create_odg_graphic} uses the empty template skeleton.odg which
#' is stored in the rmarkdown template directory of package \code{rcoursetools}.
#' When calling  \code{create_odg_graphic} without any arguments, then the
#' template is opened using soffice draw. When a filename for the odg graphic
#' which should be created, is specified the template is renamed to the given
#' name.
#'
#' @param psGraphicName   Format of diagram to be created
#' @export create_odg_graphic
create_odg_graphic <- function(psGraphicName = NULL){
  ### # use package template dir used by rmarkdown::draft
  sTemplatePath <- system.file(package = "rcoursetools", "rmarkdown", "templates", "odg_figure", "skeleton")
  ### # determine full path and filename for diagram templates
  sGraphicTemplate <- file.path(sTemplatePath, paste("skeleton", psFormat, sep = "."))
  ### # template file must exist, otherwise we stop here
  stopifnot(file.exists(sGraphicTemplate))
  ### # in case a filename for the graphic is specified, we rename the templated
  ### #  to the given name
  if (!is.null(psGraphicName)) {
    file.copy(from = sGraphicTemplate, to = psGraphicName)
    sGraphicFile <-  psGraphicName
  } else {
    sGraphicFile <- sGraphicTemplate
  }
  ### # depending on platform start open the template file differently
  if (.Platform$OS.type == "windows"){
    file.show(sGraphicFile)
  } else {
    sSofficeCmd <- paste("soffice --draw", sGraphicFile)
    system(sSofficeCmd)
  }
  ### # return nothing
  invisible()
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


#' Open an existing odg graphics file
#'
#' @description
#' Existing graphic files in odg format can be opened using
#' \code{open_odg_graphic}. Depending on the OS platform
#' different commands are used to open the graphic file which
#' is specified by the parameter psOdgGraphicName
#'
#' @param psOdgGraphicName   Name of the odg graphic file
#' @export open_odg_graphic
open_odg_graphic <- function(psOdgGraphicName){
  ### # depending on the platform type, use a different command
  ### #  to open the file
  if (.Platform$OS.type == "windows"){
    file.show(psOdgGraphicName)
  } else {
    sSofficeCmd <- paste("soffice --draw", psOdgGraphicName)
    system(sSofficeCmd)
  }
  ### # return nothing
  invisible()

}
