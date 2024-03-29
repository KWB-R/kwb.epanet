% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/configureReport.R
\name{reportVariable}
\alias{reportVariable}
\title{Report Variable Definition}
\usage{
reportVariable(
  name = "Elevation",
  yes = TRUE,
  below = NA,
  above = NA,
  precision = 2
)
}
\arguments{
\item{name}{\code{name} of variable. Node variables that can be reported on include:
"Elevation", "Demand", "Head", "Pressure", "Quality".
Link variables include: "Length", "Diameter", "Flow", "Velocity",
"Headloss", "LinkQuality", "LinkStatus", "Setting" (Roughness for pipes,
speed for pumps,,  pressure/flow setting for valves), "Reaction" (reaction
rate), "FFactor" (friction factor)}

\item{yes}{shall the variable be importet on?}

\item{below}{if set to a non-NA value (default: NA) only values \code{below} the given value
will be reported on}

\item{above}{if set to a non-NA value (default: NA) only values \code{above} the given value
will be reported on}

\item{precision}{the variable will be reported on with the given \code{precision} (number of
decimal places). Default: 2}
}
\description{
Report variable definition for argument \emph{variables} of
\code{\link{setReportOptions}}
}
