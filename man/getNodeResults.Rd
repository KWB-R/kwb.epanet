% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readEpanetOutput.R
\name{getNodeResults}
\alias{getNodeResults}
\title{Get Node Results}
\usage{
getNodeResults(outdat, nodes, vars = c("d", "h", "p", "wq"))
}
\arguments{
\item{outdat}{output data read from EPANET output file, as returned by
\code{\link{readEpanetOutputFile}}}

\item{nodes}{names of \code{nodes} to be included in the returned data frame. You may use
\code{\link{getNamesOfJunctions}, \link{getNamesOfReservoirs}, 
  \link{getNamesOfTanks}} in order to get the names of available \code{nodes}}

\item{vars}{acronyms of variables to be included in the returned data frame.
"d" = demand, "h" = head, "p" = pressure, "wq" = water quality}
}
\description{
Get Node Results
}
