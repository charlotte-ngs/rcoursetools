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
#' create pdf_ouput and not html_vignette.
#'
#' @param psSlidesName   Name of Source-File for Slides
#' @param psCourseDir    Course directory
#' @export create_slides
create_slides <- function(psSlidesName, psCourseDir = ".", psSlidesSubdir = NULL){
  ### # doing a setup similar to devtools::use_vignette
  pkg <- devtools::as.package(psCourseDir)
  devtools:::add_desc_package(pkg, "Suggests", "knitr")
  devtools:::add_desc_package(pkg, "Suggests", "rmarkdown")
  devtools:::add_desc_package(pkg, "VignetteBuilder", "knitr")
  dir.create(file.path(pkg$path, "vignettes"), showWarnings = FALSE)
  sSlidesPath <- file.path(pkg$path, "vignettes", paste0(psSlidesName, ".Rmd"))
  ### # use rmarkdown draft function to copy over the draft file
  rmarkdown::draft(file = sSlidesPath,
                   template = "beamer_slides",
                   package = "rcoursetools",
                   create_dir = FALSE,
                   edit = FALSE)
  message("Slides draft created in: ", sSlidesPath)
}
