###
###
###
###   Purpose:   Functions related to the whole course
###   started:   2016/02/02 (pvr)
###
### ################################################### ###

#' Create a new course
#'
#' The function \code{create_course} sets up the skeleton
#' infrastructure for a course
#' @param pscoursename   name of the course
#' @param pscoursedir    root directory where course is created
#' @param pbOverWrite    in case resulting course package directory, exists, delete it
#' @export create_course
create_course <- function(psCourseName, psCourseDir = ".", pbOverWrite = FALSE){
  ### # if pbOverWrite is specified, then delete any existing course directories
  ### #  be careful with this
  sCoursePackDir <- file.path(psCourseDir, psCourseName)
  if (dir.exists(sCoursePackDir) && pbOverWrite)
    unlink(sCoursePackDir, recursive = TRUE)
  ### # in case pscoursedir does not exist, create it
  if(!dir.exists(psCourseDir))
    dir.create(psCourseDir, recursive = TRUE)
  ### # use devtools to create a package
  devtools::create(path = sCoursePackDir)
  ### # return invisible
  invisible(TRUE)
}
