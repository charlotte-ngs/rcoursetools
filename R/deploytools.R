###
###
###
###   Purpose:   Deployment of material
###   started:   2016/02/04 (pvr)
###
### #################################### ###
#' Deploy a website from a source branch to a publication branch
#'
#' \code{deploy_website} deploys a website that was created in some
#' branch directory into some other branch from where it is published
#' on the web via github pages. The need for this function comes from
#' the fact that github project pages are published from the gh-pages
#' branch and not from the master branch. But still we would like to
#' develop the website on the master branch which gives us the possibility
#' to test everyting without the need for immediate publication.
#'
#' @param   psFrom   source file which is to be deployed, the source directory must exist, o/w, the function stops with an error
#' @param   psToDir  directory where the file should be deployed to, if it does not exist, the directory will be created
deploy_website <- function(psFrom = "vignettes/index.Rmd", psToDir = NULL){
  ### # check that psFrom exists
  stopifnot(file.exists(psFrom))
  ### # psToDir must be specified
  stopifnot(!is.null(psToDir))
  ### # if to dir does not exist, then create it
  if (!dir.exists(psToDir))
    dir.create(psToDir, recursive = TRUE)
  ### # translate rmd to md
  knitr::knit(input = psFrom, output = file.path(psToDir, "index.md"))
  invisible(TRUE)
}
