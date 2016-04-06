###
###
###
###   Purpose:   Collection of reference classes representing tables
###   started:   2016/04/06 (pvr)
###
### ################################################################# ###

#' Reference class representing a generic table
#'
#' @description
#' Within the reference object of reference class \code{RCGenericTable}
#' a table is represented by a header and a body. Tables can only be
#' modified by adding new rows. The two methods \code{to_knitr_kable}
#' and \code{to_pander_pandoc} write tables in markdown format using
#' functions \code{knitr::kable()} and \code{pander::pandoc.table()}
#'
#' @details
#' A reference object of reference class \code{RCGenericTable} assumes
#' that a table simply consists of a header and a body. The body
#' is just a character vector where each component represents one
#' column header. The body is a list which each element contains the
#' data for one column. The reference class components are represented
#' by the fields \code{sTableHeader} and \code{lTableBody}. The
#' initialisation method creates a table with an empty body. The table
#' header field has a getter and a setter methods. The method \code{addRow}
#' adds a new row at the end of the table.
#'
#' @field sTableHeader  vector of table headers
#' @field lTableBody    list of table body
#' @exportClass RCGenericTable
#' @export RCGenericTable
RCGenericTable <- setRefClass(Class = "RCGenericTable",
                              fields = list(sTableHeader = "character",
                                            lTableBody   = "list"),
                              methods = list(
                                initialize = function(){
                                  'Initialisation of table with empty body.'
                                  lTableBody <<- list()
                                },
                                setTableHeader = function(psTableHeader){
                                  'Setter for table header'
                                  sTableHeader <<- psTableHeader
                                },
                                getTableHeader = function(){
                                  'Getter for table header'
                                  return(sTableHeader)
                                },
                                addRow = function(plTableRow){
                                  'Adding a row represented by a list to the
                                  body of the table. The list representing
                                  the row must have the same names as the
                                  existing table body, otherwise, the row is
                                  not added to the table body.
                                  '
                                  if (length(lTableBody) == 0L){
                                    lTableBody <<- plTableRow
                                  } else {
                                    sTableBodyNames <- names(lTableBody)
                                    sTableRowNames <- names(plTableRow)
                                    if (any(sTableBodyNames != sTableRowNames)){
                                      cat(" * Error cannot add current row to table due to name missmatches")
                                      cat(" * Table names:\n")
                                      print(sTableBodyNames)
                                      cat(" * Table row names:\n")
                                      print(sTableRowNames)
                                    } else {
                                      ### # use mapply to merge fields of table body and row to be added
                                      lTableBody <<- mapply(c, lTableBody, plTableRow, SIMPLIFY = FALSE)
                                    }
                                  }
                                },
                                to_knitr_kable = function(){
                                  'Output current table in markdown format using function
                                  knitr::kable(). In case the length of the specified table
                                  header is consistent with the number of columns, then
                                  the table header is added as columnnames of the data.frame
                                  representation of the table body.
                                  '
                                  ### # convert table body to data.frame
                                  dfTable <- as.data.frame(lTableBody, stringsAsFactors = FALSE)
                                  ### # in case length of sTableHeader and number of columns match
                                  ### #  use them as column names
                                  if (identical(length(sTableHeader), ncol(dfTable)))
                                    colnames(dfTable) <- sTableHeader
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
                                  dfTable <- as.data.frame(lTableBody, stringsAsFactors = FALSE)
                                  ### # in case length of sTableHeader and number of columns match
                                  ### #  use them as column names
                                  if (identical(length(sTableHeader), ncol(dfTable)))
                                    colnames(dfTable) <- sTableHeader
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
                                }))

#' ================================================================================== '#

#' Reference class representing document status information
#'
# RCDocuStatus <- setRefClass(Class = "RCDocuStatus",
#                             contains = "RCGenericTable",
#                             fields = list(),
#                             methods = list())
