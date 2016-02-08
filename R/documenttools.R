###
###
###
###   Purpose:   Collection of tools for documents
###   started:   2016/02/03 (pvr)
###
### ################################################ ###

#' Create a course website
create_course_website <- function(psCourseName, psCourseDir = ".", psWsTemplate = NULL){
  ### # specify the course package directory
  sCoursePackDir <- file.path(psCourseDir, psCourseName)
  ### # check that course package exists
  stopifnot(dir.exists(sCoursePackDir))
  ### # if template is not specified use a template from rmarkdown
  if (is.null(psWsTemplate)) {
    devtools::use_vignette(name = "index", pkg = sCoursePackDir)
  } else {
    sWsTemplate <- psWsTemplate
    file.copy(from = sWsTemplate, to = file.path(sCoursePackDir, "vignettes", "index.Rmd"), recursive = TRUE)
  }
  invisible(TRUE)
}
