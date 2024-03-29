% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tutorialFunctions.R
\name{runOptimisationStrategy}
\alias{runOptimisationStrategy}
\title{Run Optimisation Strategy}
\usage{
runOptimisationStrategy(
  configuration,
  newCurvesData,
  optimisationStrategy,
  operationSchemes = wellFieldOperationSchemes(getNamesOfPumps(configuration)),
  showLivePlot = FALSE
)
}
\arguments{
\item{configuration}{EPANET \code{configuration}, representing an EPANET
input \code{file}, as returned by \code{\link{readEpanetInputFile}}}

\item{newCurvesData}{list with elements \emph{Pump} (data frame with columns
"ID", "X_VALUE" and "Y_VALUE") of pump curves, \emph{GlobalPumpEfficiency} (data frame with columns
"ID", "X_VALUE" and "Y_VALUE") and sublist \emph{PumpNamePrefix} (with elements
\emph{PumpCurves} = "TDH" and  \emph{GlobalPumpEfficiency} = "Eff" )}

\item{optimisationStrategy}{list with elements \emph{name} (name of optimisation strategy),
\emph{shortName} (short name of optimisation strategy) and \emph{pumpsToReplace} (vector with
pump-ids to be replaced, if none: "")}

\item{operationSchemes}{possible wellfield operation schemes. Default:
wellFieldOperationSchemes(getNamesOfPumps(\code{configuration}))}

\item{showLivePlot}{(default: FALSE)}
}
\value{
list with elements \emph{energyTotal} and \emph{energyPerPump}
}
\description{
Run Optimisation Strategy
}
