% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/writeCurves.R
\name{writeEfficiencyCurves}
\alias{writeEfficiencyCurves}
\title{Write Efficiency Curves}
\usage{
writeEfficiencyCurves(
  epanetConfig,
  Eff,
  curveNamePrefix = "Eff",
  deleteOldCurves = FALSE,
  dbg = TRUE
)
}
\arguments{
\item{epanetConfig}{EPANET configuration, as retrieved by \code{readEpanetInputFile}}

\item{Eff}{data.frame(ID, X_Value, Y_Value)}

\item{curveNamePrefix}{prefix to be used in the curve name (default: "Eff")}

\item{deleteOldCurves}{if TRUE all curves in epantetConfig$CURVES will be
deleted before adding new ones (default: FALSE)}

\item{dbg}{show debug messages? (default: TRUE)}
}
\value{
Modified EPANET configuration
}
\description{
Write Efficiency Curves
}
