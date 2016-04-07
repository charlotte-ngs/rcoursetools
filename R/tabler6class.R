###
###
###
###   Purpose:   Collection of reference classes representing tables
###   started:   2016/04/06 (pvr)
###
### ################################################################# ###

#' R6 class representing a generic table
#'
#' @description
#' Within the object of R6 class \code{RCGenericTable}
#' a table is represented by a header and a body. Tables can only be
#' modified by adding new rows. The two methods \code{to_knitr_kable}
#' and \code{to_pander_pandoc} write tables in markdown format using
#' functions \code{knitr::kable()} and \code{pander::pandoc.table()}
#'
#' @details
#' An object of R6 class \code{R6GenericTable} assumes
#' that a table simply consists of a header and a body. The body
#' is just a character vector where each component represents one
#' column header. The body is a list which each element contains the
#' data for one column. The R6 class components are represented
#' by the fields \code{sTableHeader} and \code{lTableBody}. The
#' initialisation method creates a table with an empty body. The table
#' header field has a getter and a setter methods. The method \code{addRow}
#' adds a new row at the end of the table.
#'
#' @field sTableHeader  vector of table headers
#' @field lTableBody    list of table body
#' @export R6GenericTable
R6GenericTable <- R6::R6Class(classname = "R6GenericTable",
                              public = list(
                                initialize = function(){
                                  'Initialisation of table with empty body.'
                                  private$sTableHeader <- NULL
                                  private$lTableBody <- list()
                                },
                                setTableHeader = function(psTableHeader){
                                  'Setter for table header'
                                  private$sTableHeader <- psTableHeader
                                },
                                getTableHeader = function(){
                                  'Getter for table header'
                                  return(private$sTableHeader)
                                },
                                addRow = function(plTableRow){
                                  'Adding a row represented by a list to the
                                  body of the table. The list representing
                                  the row must have the same names as the
                                  existing table body, otherwise, the row is
                                  not added to the table body.
                                  '
                                  if (length(private$lTableBody) == 0L){
                                    private$lTableBody <- plTableRow
                                  } else {
                                    sTableBodyNames <- names(private$lTableBody)
                                    sTableRowNames <- names(plTableRow)
                                    if (any(sTableBodyNames != sTableRowNames)){
                                      cat(" * Error cannot add current row to table due to name missmatches")
                                      cat(" * Table names:\n")
                                      print(sTableBodyNames)
                                      cat(" * Table row names:\n")
                                      print(sTableRowNames)
                                    } else {
                                      ### # use mapply to merge fields of table body and row to be added
                                      private$lTableBody <<- mapply(c,
                                                                    private$lTableBody,
                                                                    plTableRow,
                                                                    SIMPLIFY = FALSE)
                                    }
                                  }
                                },
                                to_data_frame  = function(){
                                  dfTable <- as.data.frame(private$lTableBody, stringsAsFactors = FALSE)
                                  if (is.null(private$sTableHeader))
                                    private$sTableHeader <- names(private$lTableBody)
                                  ### # in case length of sTableHeader and number of columns match
                                  ### #  use them as column names
                                  if (identical(length(private$sTableHeader), ncol(dfTable)))
                                    colnames(dfTable) <- private$sTableHeader
                                  return(dfTable)
                                },
                                to_knitr_kable = function(){
                                  'Output current table in markdown format using function
                                  knitr::kable(). In case the length of the specified table
                                  header is consistent with the number of columns, then
                                  the table header is added as columnnames of the data.frame
                                  representation of the table body.
                                  '
                                  ### # convert table body to data.frame
                                  dfTable <- self$to_data_frame()
                                  ### # use knitr::kable to print the output
                                  knitr::kable(dfTable)
                                },
                                to_pander_pandoc = function(psStyle = "rmarkdown",
                                                            psJustify = NULL,
                                                            pnSplitCells = 30){
                                  'Output current table in markdown format using the function
                                  pander::pandoc.table(). This method accepts two parameters
                                  psStyle and psJustify which are passed to
                                  to pander::pandoc.table().
                                  '
                                  ### # convert table body to data.frame
                                  dfTable <- self$to_data_frame()
                                  ### # in case psJustify is specified, use it, otherwise use default
                                  if (!is.null(psJustify) & identical(length(psJustify), ncol(dfTable))){
                                    pander::pandoc.table(dfTable,
                                                         style = psStyle,
                                                         justify = psJustify,
                                                         split.cells = pnSplitCells)
                                  } else {
                                    pander::pandoc.table(dfTable,
                                                         style = psStyle,
                                                         split.cells = pnSplitCells)
                                  }
                                }
                              ),
                              private = list(
                                sTableHeader = "character",
                                lTableBody   = "list"
                              )
                            )


#' ================================================================================== '#

#' Reference class representing document status information
#'
#' @description
#' Concrete class for document status table
#'
#' @field   sStatusFile  name of the status file
#' @export  R6DocuStatus
R6DocuStatus <- R6::R6Class(classname = "R6DocuStatus",
                            inherit = R6GenericTable,
                            public = list(
                              initialize = function(){
                                'Initialisation of a document status table'
                                private$sTableHeaderOutput <- c("Version",
                                                          "Datum",
                                                          "Wer",
                                                          "Änderung")
                                private$lTableBody <- list()
                              },
                              setStatusFile = function(psStatusFile){
                                'Setter method for status file'
                                private$sStatusFile <- psStatusFile
                              },
                              getStatusFile = function(){
                                'Getter method for status file'
                                return(private$sStatusFile)
                              },
                              writeStatusToFile = function(psFile = NULL){
                                'Write current status info to file which is
                                 either given by private$sStatusFile or
                                 by the method argument psFile. If both are
                                 NULL, then an error is generated.'
                                # writing current status to file
                                dfTable <- self$to_data_frame()
                                # check whether status file is given
                                #  in object or by method argument
                                if (is.null(private$sStatusFile)){
                                  if (is.null(psFile))
                                    stop("No file to write current status to, specified")
                                  write.csv(dfTable, file = psFile,
                                            quote = FALSE, row.names = FALSE)
                                } else {
                                  write.csv(dfTable, file = private$sStatusFile,
                                            quote = FALSE, row.names = FALSE)
                                }
                              },
                              readStatusFile = function(psFile = NULL){
                                'read status info file which is either given
                                 in the object or supplied as an argument to
                                 this method. If the file cannot be found then
                                 an error is generated.'
                                if (is.null(private$sStatusFile)){
                                  if (is.null(psFile))
                                    stop("No file to write current status to, specified")
                                  sFile <- psFile
                                } else {
                                  sFile <- private$sStatusFile
                                }
                                ### # check whether file is found
                                if (!file.exists(sFile))
                                  stop("Status file: ", sFile, " cannot be found")
                                dfTable <- read.csv(file = sFile, stringsAsFactors = FALSE)
                                ### # convert data-frame to list
                                private$lTableBody <- as.list(dfTable)
                                private$sTableHeader <- names(private$lTableBody)
                              }
                            ),
                            private = list(
                              sStatusFile = "character",
                              sTableHeaderOutput = "character"
                            ))
