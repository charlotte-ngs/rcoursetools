#' Tangle vignette documents
#'
#' \code{purl_vignette(input)} tangles document input in directory vignettes
#'
#' @param input   input file to be tangled
purl_vignette <- function(psInput, psVigsDir = "vignettes") {
  # if input is just a filename without path, we add the subdir
  sPurlVignFn <- file.path(psVigsDir,psInput)
  # check file existance
  if (!file.exists(sPurlVignFn)) stop(paste(" *** ERROR: Cannot find file:", sPurlVignFn))
  # tangle to vignette-file
  knitr::purl(input = sPurlVignFn)
  invisible()
}
