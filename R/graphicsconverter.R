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


## ---- Insert a Odg draw graphic -------------------------------------------
#' Inserts an odg draw graphic into a rmarkdown text
#'
#' @description
#' \code{insertOdgAsPdf} takes the name of a file containing a graphic
#' in odg format, converts the content of that file into pdf using
#' function \code{convertLibOToPdf} and outputs the string in markdown
#' format to include the figure. Pdf-formatted graphics are only re-generated
#' if the pdf-file does not exist or, if the flag pbMustGenerate is TRUE.
#'
#' @param  psOdgFileStem    stem of odg figure file
#' @param  psOdgDir         directory where odg figure file is stored
#' @param  pbMustGenerate   flag to indicate whether pdf-graphics must be regenerated
#' @param  psFigOutDir      directory where output should be placed
#' @export insertOdgAsPdf
insertOdgAsPdf <- function(psOdgFileStem, psOdgDir = "odg",
                                 psFigOutDir = ".",
                                 pbMustGenerate = FALSE,
                                 pnPaperWidthScale = NULL) {
  ### # check wether pdf file already exists, if so, do nothing
  sPdfFilename <- paste(psOdgFileStem, "pdf", sep = ".")
  sPdfFile <- file.path(psFigOutDir,sPdfFilename)
  if (!file.exists(sPdfFile) | pbMustGenerate){
    ### # if pdf files cannot be found, regenerate them, check that psOdgFileName exists
    sOdgFilename <- paste(psOdgFileStem, "odg", sep = ".")
    sOdgFile <- file.path(psOdgDir, sOdgFilename)
    if (!file.exists(sOdgFile))
      stop("Cannot find Odg figure file: ", sOdgFile)
    ### # convert odg file to pdf
    convertLibOToPdf(psLibOFile = sOdgFilename, psLibODir = psOdgDir, psFigOutDir = psFigOutDir)

  }
  ### # at this point the pdf file must exist, either from previous conversion
  ### #  or from converting it right now
  if (!file.exists(sPdfFile))
    stop("Cannot find pdf figure file: ", sPdfFile)
  ### # in case a width scale was specified, use it
  if (!is.null(pnPaperWidthScale))
    genericScaledPlot(pnPaperWidthScale = pnPaperWidthScale)
  ### # output the command to include the figure
  cat("![", psOdgFileStem, "](", sPdfFile, ")\n", sep = "")
}
