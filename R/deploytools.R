###
###
###
###   Purpose:   Deployment of material
###   started:   2016/02/04 (pvr)
###
### #################################### ###
#' Deploy a website from a source branch to a publication branch
#'
#' @description
#' \code{deploy_website} deploys a website that was created in some
#' branch directory into some other branch from where it is published
#' on the web via github pages. The need for this function comes from
#' the fact that github project pages are published from the gh-pages
#' branch and not from the master branch. But still we would like to
#' develop the website on the master branch which gives us the possibility
#' to test everyting without the need for immediate publication.
#'
#' @details
#' The flag \code{pbKnitrKnit} indicates whether the original document should
#' be converted from Rmd to md format. NB github pages only allow .md documents
#' for publication. The flag \code{pbOverWrite} specifies whether any existing
#' result files should be overwritten or not. The same value is used for option
#' \code{recursive} of the used function \code{file.copy}.
#'
#' @param   psFrom        source file which is to be deployed, the source directory must exist, o/w, the function stops with an error
#' @param   psToDir       directory where the file should be deployed to, if it does not exist, the directory will be created
#' @param   pbKnitrKnit   should the original file be converted from Rmd to md, default = FALSE
#' @param   pbOverWrite   should existing result files be overwritten, default = TRUE, this also influences option recursive of file.copy
deploy_website <- function(psFrom = "vignettes/index.Rmd", psToDir = NULL, pbKnitrKnit = FALSE, pbOverWrite = TRUE){
  ### # check that psFrom exists
  stopifnot(file.exists(psFrom))
  ### # psToDir must be specified
  stopifnot(!is.null(psToDir))
  ### # if to dir does not exist, then create it
  if (!dir.exists(psToDir))
    dir.create(psToDir, recursive = TRUE)
  ### # if pbKnitrKnit then convert from Rmd to md, otherwise leave the format
  if (pbKnitrKnit){
    ### # translate rmd to md
    knitr::knit(input = psFrom, output = file.path(psToDir, "index.md"))
  } else {
    sFromFile <- basename(psFrom)
    file.copy(from = psFrom, to = file.path(psToDir, sFromFile), overwrite = pbOverWrite, recursive = pbOverWrite)
  }
  invisible(TRUE)
}


#' build and deploy website
#'
#' First any existing output of vignettes in pkg/inst/doc is removed
#' using \code{devtools::clean_vignettes}. Then vignettes are built
#' using \code{devtools::build_vignettes}. In the last step, the result
#' is deployed to a given target directory using \code{deploy_website}.
#'
#' @param    pkg   package directory where vignette sources are stored
#' @param    psFrom   directory where vignette source is stored
#' @param    psToDir  target directory to where vigette is deployed to
build_deploy_website <- function(pkg = ".", psFrom = file.path(pkg, "vignettes", "index.Rmd"), psToDir = ".") {
  ### # clean existing vignettes first
  devtools::clean_vignettes(pkg = pkg)
  ### # build vignettes
  devtools::build_vignettes(pkg = pkg)
  ### # deploy the website to where we want them to be
  sVigHtmlFn <- sub_ext(basename(psFrom), "html")
  deploy_website(psFrom = file.path(pkg, "inst", "doc", sVigHtmlFn), psToDir = psToDir)
}

