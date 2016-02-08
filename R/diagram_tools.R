###
###
###
###   Purpose:   Tools related to diagrams
###   started:   2016/01/20 (pvr)
###
### ######################################## ###

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
