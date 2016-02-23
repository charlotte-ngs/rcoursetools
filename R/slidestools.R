###
###
###
###   Purpose:   Tools related to slides
###   started:   2016/02/22 (pvr)
###
### ##################################### ###

#' Create a deck of slides
#'
#' @description
#' This is a wrapper function to \code{rmarkdown::draft} that
#' creates a source document for a deck of beamer slides. The
#' template is taken from this packages template directory
#'
#' @details
#' The setup here is done analogeously to \code{devtools::use_vignette},
#' except that here, we are using different templates and we want to
#' create pdf_ouput and not html_vignette. The template to be used
#' can be specified as a parameter, by default "beamer_slides" is
#' used as a template.
#'
#' @param psSlidesName       Name of Source-File for Slides
#' @param psCourseDir        Course directory
#' @param psSlidesTemplate   template for slides which should be used
#' @param psSlidesFormat     extension of final slides file name
#' @export create_slides
create_slides <- function(psSlidesName, psCourseDir = ".",
                          psSlidesTemplate = "beamer_slides",
                          psSlidesFormat   = "Rmd"){
  ### # doing a setup similar to devtools::use_vignette
  pkg <- devtools::as.package(psCourseDir)
  devtools:::add_desc_package(pkg, "Suggests", "knitr")
  devtools:::add_desc_package(pkg, "Suggests", "rmarkdown")
  devtools:::add_desc_package(pkg, "VignetteBuilder", "knitr")
  dir.create(file.path(pkg$path, "vignettes"), showWarnings = FALSE)
  ### # depending on psSlidesFormat, put different extensions to final file
  sSlidesPath <- file.path(pkg$path, "vignettes", paste(psSlidesName, "Rmd", sep = "."))
  ### # use rmarkdown draft function to copy over the draft file
  rmarkdown::draft(file = sSlidesPath,
                   template = psSlidesTemplate,
                   package = "rcoursetools",
                   create_dir = FALSE,
                   edit = FALSE)
  sFinalSlideName <- sSlidesPath
  if (psSlidesFormat == "Rnw"){
    sFinalSlideName <- gsub("Rmd$", "Rnw", sSlidesPath)
    file.rename(from = sSlidesPath, to = sFinalSlideName)
  }

  message("Slides draft created in: ", sFinalSlideName)
}


## ---- Clean up output from slide compilation -------------------------------------- ##
#' Clean up output files from slide compilation
#'
cleanup_slidesdir <- function(psSlidesDir = ".", psFormatToKeep = c("rnw$", "cls$", "rpres$"), psNamesToKeep = c("ETH-BG")) {
  ### # get list of all files in slides directory
  vAllFiles <- list.files(path = psSlidesDir)
  ### # find index  of files that match a file extension pattern
  vKeepFileIdx <- unlist(sapply(psFormatToKeep,
                                function(psPat) grep(pattern = psPat, vAllFiles, ignore.case = TRUE),
                                USE.NAMES = FALSE))
  ### # find files that match a fixed name
  vKeepNameIdx <- unlist(sapply(psNamesToKeep,
                                function(psNamePat) grep(psNamePat, vAllFiles, fixed = TRUE),
                                USE.NAMES = FALSE))
  vKeep <- union(vKeepFileIdx, vKeepNameIdx)
  ### # delete all files except those to be kept
  sapply(vAllFiles[-c(vKeep)], function(x) unlink(file.path(psSlidesDir, x), recursive = TRUE))

}
