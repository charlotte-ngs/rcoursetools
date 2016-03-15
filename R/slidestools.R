###
###
###
###   Purpose:   Tools related to slides
###   started:   2016/02/22 (pvr)
###
### ##################################### ###

#' Create a deck of slides
#'
#' @description
#' This is a wrapper function to \code{rmarkdown::draft} that
#' creates a source document for a deck of beamer slides. The
#' template is taken from the template directory of this packages
#'
#' @details
#' The setup here is done analogeously to \code{devtools::use_vignette},
#' except that here, we are using different templates and we want to
#' create pdf_ouput and not html_vignette. The template to be used
#' can be specified as a parameter, by default "beamer_slides" is
#' used as a template. We explicitly allow and encourage source files of
#' slides to be organized in subdirectories of the vignettes directory,
#' knowing that this breaks vignette building by the devtools infrastructure.
#' There are several reasons for the subdirectories, first the function
#' \code{rmarkdown::draft} gives an error when a file or directory that
#' should be copied from the templated directory already exists in the
#' target directory where it should be copied to. Secondly, we want to
#' have the possibilities to use different documentclasses for different
#' slide documents. Thirdly, putting all slide sources of a complete
#' course in a single directory clutters the whole directory. Hence
#' organizing the material in subdirectories helps us to keep a better
#' overview over the whole course material.
#'
#' @param psSlidesName       name of slides source file
#' @param psCourseDir        course directory
#' @param psSlidesSubdir     subdirectory for current slides
#' @param psSlidesTemplate   template for slides which should be used
#' @param psSlidesFormat     extension of final slides file name
#' @export create_slides
create_slides <- function(psSlidesName, psCourseDir = ".",
                          psSlidesSubdir = NULL,
                          psSlidesTemplate = "beamer_slides",
                          psSlidesFormat   = "Rmd"){
  ### # doing a setup similar to devtools::use_vignette
  pkg <- devtools::as.package(psCourseDir)
  devtools:::add_desc_package(pkg, "Suggests", "knitr")
  devtools:::add_desc_package(pkg, "Suggests", "rmarkdown")
  devtools:::add_desc_package(pkg, "VignetteBuilder", "knitr")
  ### # setup the directory structure where we want to put the slides
  sVigDir <- file.path(pkg$path, "vignettes")
  if (!is.null(psSlidesSubdir)){
    sSlidesDir <- file.path(sVigDir, psSlidesSubdir)
  } else {
    sSlidesDir <- sVigDir
  }
  ### # check whether slides directory exists
  if (!dir.exists(sSlidesDir))
    dir.create(sSlidesDir, recursive = TRUE, showWarnings = FALSE)
  ### # depending on psSlidesFormat, put different extensions to final file
  sSlidesPath <- file.path(sSlidesDir, paste(psSlidesName, "Rmd", sep = "."))
  ### # use rmarkdown draft function to copy over the draft file
  rmarkdown::draft(file = sSlidesPath,
                   template = psSlidesTemplate,
                   package = "rcoursetools",
                   create_dir = FALSE,
                   edit = FALSE)
  sFinalSlideName <- sSlidesPath
  ### # allow for legacy Rnw based templates
  if (psSlidesFormat == "Rnw"){
    sFinalSlideName <- gsub("Rmd$", "Rnw", sSlidesPath)
    file.rename(from = sSlidesPath, to = sFinalSlideName)
  }

  message("Slides draft created in: ", sFinalSlideName)
}


## ---- Clean up output from slide compilation -------------------------------------- ##
#' Clean up output files from slide compilation
#'
#' @description
#' Cleaning up files and directories that are produced while compiling different
#' types of rmarkdown sources. If certain files should be ignored by the cleanup
#' function, then their extension can be specified as a component of the vector
#' passed as function parameter psFormatToKeep. If directories should be kept,
#' then their name can be specified as a component of the parameter vector
#' psDirsToKeep.
#'
#' @details
#' As a security feature, a question to the user is asked whether or not to delete
#' the output files.
#'
#' @param psSlidesDir      root directory of document source files
#' @param psFormatToKeep   file extensions of source files to be ignored by cleanup
#' @param psDirsToKeep     explicit names of directories to be ignored by cleanup
#' @export cleanup_slidesdir
cleanup_slidesdir <- function(psSlidesDir = "vignettes",
                              psFormatToKeep = c("rmd$", "rnw$", "cls$", "rpres$"),
                              psDirsToKeep = c("ETH-BG","odg","png","tex")) {
  ### # get list of all files in slides directory
  vAllFiles <- list.files(path = psSlidesDir, full.names = TRUE)
  ### # find index  of files that match a file extension pattern
  vKeepFileIdx <- unlist(sapply(psFormatToKeep,
                                function(psPat) grep(pattern = psPat, vAllFiles, ignore.case = TRUE),
                                USE.NAMES = FALSE))
  ### # find files that match a fixed name
  vKeepDirIdx <- unlist(sapply(psDirsToKeep,
                                function(psNamePat) grep(psNamePat, vAllFiles, fixed = TRUE),
                                USE.NAMES = FALSE))
  vKeepDirIdx <- vKeepDirIdx[dir.exists(vAllFiles[vKeepDirIdx])]
  vKeep <- union(vKeepFileIdx, vKeepDirIdx)
  ### # ask user whether to delete files
  cat(" * Do you want to delete the following list of files:\n")
  print(vAllFiles[-c(vKeep)])
  sAnswerUserQuestion <- readline(prompt = "Please answer [y/N]: ")
  if (identical(sAnswerUserQuestion, "y")) {
    ### # delete all files except those to be kept
    sapply(vAllFiles[-c(vKeep)], function(x) unlink(x, recursive = TRUE))
    print(vAllFiles[-c(vKeep)])
    cat(" * ... deleted\n")
  } else {
    cat(" * No files deleted\n")
  }

}


## ---- Create notes for a given deck of slides -------------------------------------- ##
#' Create Notes document for a given deck of slides
#'
#' @description
#' For a given Rmd source file containing a deck of
#' slides, a new Rmd files is created. The new file
#' contains all the slides from the source file with
#' an extra slides inserted between each existing
#' slide. The newly inserted slides all have the title
#' Notes. These new slides can be used for notes
#' used during the presentation. The file with the
#' notes has the same name as the slide source file
#' with the string "_Notes" added before the extension.
#'
#' @details
#' Because not in all Rmd source files slide titles
#' are indicated with the same number of hashes (#),
#' we give the pattern that denotes a slide title as
#' additional function parameter psSlideTitlePattern.
#'
#' @param psSlideSrcFile        Rmd source file with slides
#' @param psSlideTitlePattern   pattern denoting a slide title
#' @param pbEditNotesFile       Flag indicating whether to open the notes file for edit
#' @export create_slides_notes
create_slides_notes <- function(psSlideSrcFile, psSlideTitlePattern = "###", pbEditNotesFile = TRUE){
  ### # define the text for a notes slide
  sNotesSlide <- paste(psSlideTitlePattern, " Notes\n\n\n")
  ### # check whether ending was given
  if (length(grep("Rmd$", psSlideSrcFile)) > 0) {
    sSlideSrc <- psSlideSrcFile
  } else {
    sSlideSrc <- paste(psSlideSrcFile, "Rmd", sep = ".")
  }

  ### # check whether source slide file exists
  stopifnot(file.exists(sSlideSrc))
  ### # read slides file and determine titles
  conSlideFile <- file(description = sSlideSrc)
  vecSlideText <- readLines(con = conSlideFile)
  vecSlideTitles <- grep(pattern = paste0("^", psSlideTitlePattern), vecSlideText)
  vecSlideText[vecSlideTitles]
  ### # add notes slide before each title
  vecSlideText[vecSlideTitles] <- sapply(vecSlideText[vecSlideTitles],
                                         function(x) paste0(sNotesSlide, x),
                                         USE.NAMES = FALSE)
  ### # add one more notes slide
  vecSlideText <- c(vecSlideText, paste0("\n\n", sNotesSlide))
  ### # write notes to file
  sNoteFile <- gsub(".Rmd$", "_Notes.Rmd", sSlideSrc)
  conNoteFile <- file(description = sNoteFile)
  writeLines(vecSlideText, con = conNoteFile)
  ### # close connections
  close(conSlideFile)
  close(conNoteFile)
  ### # edit the notes file
  if (pbEditNotesFile) file.edit(sNoteFile)

  invisible(TRUE)
}


#' Clean up notes source files
#'
#' @description
#' This function \code{cleanup_source_notes()} is to be used with caution,
#' because it will delete any Rmd source file of slide notes. It is created
#' for convenience in the testing phase with notes. Once slide notes with
#' meaningful content are created, it should not be used anymore.
#'
#' @param psNotesPattern   pattern of notes file to be deleted
#' @param psSubDir         subdirectory where notes file is
#' @param psNotesSrcFile   explicit name of notes file
#' @export cleanup_source_notes
cleanup_source_notes <- function(psNotesPattern = "_Notes.Rmd$", psSubDir = "vignettes", psNotesSrcFile = NULL){
  ### # in case a single notes source file is specified, then only delete that one
  if (!is.null(psNotesSrcFile)) {
    cat(" * Do you want to delete the following file: ", psNotesSrcFile, "\n")
    sAnswerUserQuestion <- readline(prompt = "Please answer [y/N]: ")
    if (identical(sAnswerUserQuestion, "y")) {
      file.remove(psNotesSrcFile)
    } else {
      cat(" * No files deleted\n")
    }
  } else {
    ### # files matching pattern
    sDelFiles <- list.files(path = psSubDir, pattern = psNotesPattern, full.names = TRUE)
    cat(" * Do you want to delete the following files:\n")
    print(sDelFiles)
    sAnswerUserQuestion <- readline(prompt = "Please answer [y/N]: ")
    if (identical(sAnswerUserQuestion, "y")) {
      file.remove(sDelFiles)
    } else {
      cat(" * No files deleted\n")
    }
  }
  invisible(TRUE)
}
