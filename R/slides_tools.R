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
#' @param psSlidesName   Name of Source-File for Slides
#' @param psCourseDir    Course directory
#' @export create_slides
create_slides <- function(psSlidesName, psCourseDir = ".", psSlidesSubdir = NULL){
  ### # use rmarkdown draft function to copy over the draft file
  rmarkdown::draft(file = psSlidesName, template = "beamer_slides", package = "rcoursetools")

}
