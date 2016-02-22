###
###
###
###   Purpose:   Wrapper to knitr functions which either have a problem
###              on Mac Os X or which are not exported
###   started:   2016/02/08 (pvr)
###
### ############################################################### ###


#' substitute extension of file x with ext
#'
#' x is assumed to be a vector of characters which stand for
#' filenames. The extension of the file name is the suffix that
#' occurs after the last dot (.) in every component of x. The
#' substitution is done in two steps. First the existing extension
#' of x is removed using function \code{sans_ext}. Then the new
#' extension is added with a dot (.) as separator.
#'
#' @param   x     vector with original file names
#' @param   ext   new extension to be added
#' @return  file names with new extensions
sub_ext <- function (x, ext)
{
  i = grep("\\.([[:alnum:]]+)$", x)
  x[i] = sans_ext(x[i])
  paste(x, ext, sep = ".")
}

#' remove file extension from x
#'
#' Extension of a file name is defined as the suffix that
#' occurs after the last dot (.). If argument \code{compression}
#' is set to true, then any endings that match "[.](gz|bz2|xz)$"
#' are removed first.
#'
#' @param    x             file name from which ending should be removed
#' @param    compression   flag indicating whether file x is compressed
#' @return   filename without extension
sans_ext <- function (x, compression = FALSE)
{
  if (compression)
    x <- sub("[.](gz|bz2|xz)$", "", x)
  sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}

