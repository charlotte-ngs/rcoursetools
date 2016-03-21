###
###
###
###   Purpose:   Collection of tools to work with templates
###   started:   2016/03/21 (pvr)
###
### ######################################################### ###

#' Create skeleton directories and files for a new template
#'
#' @description
#' The function \code{rmarkdown::draft()} uses a defined
#' directory structure under "rmarkdown/templates" to
#' instantiate new documents. This function \code{create_template()}
#' creates a new skeleton infrastructure for a new template.
#'
#' @details
#' In principle, we are doing meta-templating here. We use a
#' generic template and create a more specific template based
#' on that. The generic template is stored as template of
#' this package "rcoursetools". From this the template
#' infrastructure is copied to the target package from where
#' it can be used as ordinary template.
#'
#' @param  psTemplateName   name of the template to be created
#' @param  psPkgDir         Package directory
#' @export create_template
create_template <- function(psTemplateName, psPkgDir = ".") {
  ### # define template source
  sTemplateSrc <- system.file("rmarkdown", "templates", "generic_document", package = "rcoursetools")
  ### # template destination path, depending on package for which
  ### #  template should be used
  pkg <- devtools::as.package(psPkgDir)
  sTemplatePath <- file.path(pkg$path, "inst", "rmarkdown", "templates", psTemplateName)
  ### # in case template path does not exist, create it
  if (!dir.exists(sTemplatePath))
    dir.create(sTemplatePath, recursive = TRUE)
  ### # start with template yaml
  sTemplateYamlSrc <- file.path(sTemplateSrc, "template.yaml")
  if (!file.exists(sTemplateYamlSrc))
    stop("No template.yaml found in: ", sTemplateSrc)
  file.copy(from = sTemplateYamlSrc, to = sTemplatePath)
  ### # skeleton
  sSkeletonSrc <- file.path(sTemplateSrc, "skeleton")
  sSkeletonFiles <- list.files(sSkeletonSrc)
  sSkeletonDir <- file.path(sTemplatePath, "skeleton")
  ### # check whether target dir exists
  if (!dir.exists(sSkeletonDir))
    dir.create(path = sSkeletonDir, recursive = TRUE)
  for (f in sSkeletonFiles){
    if (file.exists(file.path(sSkeletonDir, f)))
      stop("File: ", f, " already exists.")
    file.copy(from = file.path(sSkeletonSrc, f), to = sSkeletonDir, overwrite = FALSE, recursive = TRUE)
  }
}
