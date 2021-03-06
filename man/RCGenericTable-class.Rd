% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tablerefclass.R
\docType{class}
\name{RCGenericTable-class}
\alias{RCGenericTable}
\alias{RCGenericTable-class}
\title{Reference class representing a generic table}
\description{
Within the reference object of reference class \code{RCGenericTable}
a table is represented by a header and a body. Tables can only be
modified by adding new rows. The two methods \code{to_knitr_kable}
and \code{to_pander_pandoc} write tables in markdown format using
functions \code{knitr::kable()} and \code{pander::pandoc.table()}
}
\details{
A reference object of reference class \code{RCGenericTable} assumes
that a table simply consists of a header and a body. The body
is just a character vector where each component represents one
column header. The body is a list which each element contains the
data for one column. The reference class components are represented
by the fields \code{sTableHeader} and \code{lTableBody}. The
initialisation method creates a table with an empty body. The table
header field has a getter and a setter methods. The method \code{addRow}
adds a new row at the end of the table.
}
\section{Fields}{

\describe{
\item{\code{sTableHeader}}{vector of table headers}

\item{\code{lTableBody}}{list of table body}
}}
\section{Methods}{

\describe{
\item{\code{addRow(plTableRow)}}{Adding a row represented by a list to the
body of the table. The list representing
the row must have the same names as the
existing table body, otherwise, the row is
not added to the table body.}

\item{\code{getTableHeader()}}{Getter for table header}

\item{\code{initialize()}}{Initialisation of table with empty body.}

\item{\code{setTableHeader(psTableHeader)}}{Setter for table header}

\item{\code{to_knitr_kable()}}{Output current table in markdown format using function
knitr::kable(). In case the length of the specified table
header is consistent with the number of columns, then
the table header is added as columnnames of the data.frame
representation of the table body.}

\item{\code{to_pander_pandoc(psStyle = "rmarkdown", psJustify = NULL,
  pnSplitCells = 30)}}{Output current table in markdown format using the function
pander::pandoc.table(). This method accepts two parameters
psStyle and psJustify which are passed to
to pander::pandoc.table().}
}}

