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
create_course <- function(psCourseName, psCourseDir = "."){
  ### # in case pscoursedir does not exist, create it
  if(!dir.exists(psCourseDir))
    dir.create(psCourseDir, recursive = TRUE)
  ### # use devtools to create a package
  devtools::create(path = file.path(psCourseDir, psCourseName))
}
