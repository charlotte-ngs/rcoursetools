#' Convert documents from source format into target format used in the document
#'
#' \code{convertLibOToPdf} assumes that graphics or diagrams are produced by
#' LibreOffice Draw. Source files are converted on the fly to pdf which are
#' then included in the source R markdown document
#'
#' @param psLibOFile    name of the libre office graphics file
#' @param psLibODir     source directory of Libre Office files
#' @param psFigOutDir   output directory where figure pdfs are expected to be
#' @export convertLibOToPdf
convertLibOToPdf <- function(psLibOFile, psLibODir = "odg", psFigOutDir = "."){
  sOdgDir <- psLibODir
  sOdgDirWin <- gsub("/", "\\", sOdgDir, fixed = TRUE)
  sConvCmdStem <- ifelse(.Platform$OS.type == "windows",
                         '"C:/Program Files (x86)/LibreOffice 5/program/soffice" --headless --convert-to pdf',
                         "soffice --headless --convert-to pdf")
  sFigFile <- ifelse(.Platform$OS.type == "windows",
                     paste(sOdgDirWin, psLibOFile, sep = "\\"),
                     file.path(sOdgDir, psLibOFile))
  sConvCommand <- paste(sConvCmdStem, sFigFile)
  system(command = sConvCommand)
  sPdfFile <- gsub("odg$", "pdf", psLibOFile)
  sFigOutFile <- file.path(psFigOutDir, sPdfFile)
  file.rename(from = sPdfFile, sFigOutFile)
}


## ---- Insert a Odg draw figure -------------------------------------------
#' Inserts an odg draw figure into a rmarkdown text
#'
#' \code{insertOdgFigure} takes the name of a file containing a figure
#' in odg format, converts the content of that file into pdf using
#' function \code{convertLibOToPdf} and outputs the string in markdown
#' format to include the figure
#'
#' @param  psOdgFileStem  stem of odg figure file
#' @param  psOdgDir       directory where odg figure file is stored
#' @param  psFigOutDir    directory where output should be placed
#' @export insertOdgFigureAsPdf
insertOdgFigureAsPdf <- function(psOdgFileStem, psOdgDir = "odg", psFigOutDir = ".") {
  ### # check that psOdgFileName exists
  sOdgFilename <- paste(psOdgFileStem, "odg", sep = ".")
  sOdgFile <- file.path(psOdgDir, sOdgFilename)
  if (!file.exists(sOdgFile))
    stop("Cannot find Odg figure file: ", sOdgFile)
  ### # convert odg file to pdf
  convertLibOToPdf(psLibOFile = sOdgFilename, psLibODir = psOdgDir, psFigOutDir = psFigOutDir)
  ### # check that generated pdf file exists
  sPdfFilename <- paste(psOdgFileStem, "pdf", sep = ".")
  sPdfFile <- file.path(psFigOutDir,sPdfFilename)
  if (!file.exists(sPdfFile))
    stop("Cannot find pdf figure file: ", sPdfFile)
  ### # output the command to include the figure
  cat("![", psOdgFileStem, "](", sPdfFile, ")\n", sep = "")
}
