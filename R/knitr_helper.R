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

#' run pandoc on an input document
pandoc <- function (input,
                    format,
                    config = getOption("config.pandoc"),
                    ext = NA,
                    encoding = getOption("encoding"))
{

  if (Sys.which("pandoc") == "")
    stop("Please install pandoc first: http://johnmacfarlane.net/pandoc/")
  cfg = if (is.null(config))
    sub_ext(input[1L], "pandoc")
  else config
  con = file(input[1L], encoding = encoding)
  tryCatch(txt <- pandoc_cfg(readLines(con, warn = FALSE)),
           finally = close(con))
  if (file.exists(cfg))
    txt = c(txt, "", readLines(cfg, warn = FALSE))
  con = textConnection(txt)
  on.exit(close(con))
  cfg = read.dcf(con)
  nms = colnames(cfg)
  if (length(nms) && "format" %in% nms) {
    warning("the \"format\" field in the configuration must be renamed to \"t\"")
    colnames(cfg)[nms == "format"] = "t"
  }
  if (missing(format))
    format = pandoc_fmt(cfg)
  if (encoding != "UTF-8") {
    input_utf8 = character(length(input))
    on.exit(unlink(input_utf8), add = TRUE)
    for (i in seq_along(input)) {
      input_utf8[i] = sub_ext(input[i], "utf8md")
      encode_utf8(input[i], encoding, input_utf8[i])
    }
    input = input_utf8
  }
  mapply(pandoc_one, input, format, ext, MoreArgs = list(cfg = cfg),
         USE.NAMES = FALSE)
}


pandoc_one <- function (input, format, ext, cfg)
{
  cmn = NULL
  if (nrow(cfg) == 0L)
    cfg = character(0)
  else if (nrow(cfg) == 1L) {
    if ("t" %in% colnames(cfg)) {
      cfg = if (cfg[1L, "t"] == format)
        drop(cfg)
      else NA
    }
    else {
      cmn = drop(cfg)
      cfg = NA
    }
  }
  else {
    if (!("t" %in% colnames(cfg)))
      stop("for a config file with multiple output formats, there must be a field named \"t\"")
    if (sum(idx <- is.na(cfg[, "t"])) > 1L)
      stop("at most one \"t\" field can be NA")
    if (sum(idx) == 1L)
      cmn = cfg[idx, ]
    cfg = cfg[!idx, , drop = FALSE]
    cfg = cfg[cfg[, "t"] == format, ]
    if (!is.null(dim(cfg))) {
      if (nrow(cfg) > 1)
        stop("the output format is not unique in config")
      cfg = character(0)
    }
  }
  out = unname(if (!is.na(cfg["o"]))
    cfg["o"]
    else {
      if (!is.na(cfg["output"]))
        cfg["output"]
      else {
        sub_ext(input, if (is.na(ext))
          pandoc_ext(format)
          else ext)
      }
    })
  cfg = cfg[setdiff(names(cfg), c("o", "output", "t"))]
  cmd = paste("pandoc", pandoc_arg(cfg), pandoc_arg(cmn), "-f markdown",
              "-t", format, "-o", out, paste(shQuote(input), collapse = " "))
  message("executing ", cmd)
  if (system(cmd) == 0L)
    out
  else stop("conversion failed")
}


#' Try to find path to pandoc
#'
#' Especially on Mac Os X system paths are no longer propagated to applications,
#' hence we try to do some work in order to find a pandoc executable
getPandocPath <- function(){
  sRstudioVersion <- RStudio.Version()$version
  #/Applications/RStudio/0.99.491/RStudio.app/Contents/MacOS/pandoc/pandoc
}
