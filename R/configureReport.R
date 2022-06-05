# setReportOptions -------------------------------------------------------------

#' Set REPORT Option in EPANET configuration
#' 
#' @param configuration EPANET \code{configuration}, representing an EPANET
#'   input \code{file}, as returned by \code{\link{readEpanetInputFile}}
#' @param pagesize \code{pagesize} sets the number of lines written per page of
#'   the output report. The default is 0, meaning that no line limit per page is
#'   in effect.
#' @param file \code{file} supplies the name of a \code{file} to which the
#'   output report will be written. If the \code{file} name contains spaces then
#'   it must be surrounded by double quotes. If not supplied then the Report
#'   \code{file}, as specified in the second parameter of the ENopen (or
#'   ENepanet) function will be used.
#' @param status \code{status} determines whether hydraulic \code{status}
#'   \code{messages} are written to the Report \code{file}. If YES is selected
#'   the \code{messages} will identify those network components that change
#'   \code{status} during each time step of the simulation. If FULL is selected,
#'   then convergence information will also be included from each trial of each
#'   hydraulic analysis. This level of detail is only useful for de-bugging
#'   networks that become hydraulically unbalanced. The default is NO.
#' @param summary \code{summary} determines whether a \code{summary} table of
#'   number of network components and key analysis options is generated. The
#'   default is YES.
#' @param messages \code{messages} determines whether error and warning
#'   \code{messages} generated during a hydraulic/water quality analysis are
#'   written to the Report \code{file}. The default is YES.
#' @param energy \code{energy} determines if a table reporting average
#'   \code{energy} usage and cost for each pump is provided. The default is NO.
#' @param nodes \code{nodes} identifies which \code{nodes} will be reported on.
#'   You can either list individual node ID labels or use the keywords NONE or
#'   ALL. Additional NODES lines can be used to continue the list. The default
#'   is NONE.
#' @param links \code{links} identifies which \code{links} will be reported on.
#'   You can either list individual link ID labels or use the keywords NONE or
#'   ALL. Additional LINKS lines can be used to continue the list. The default
#'   is NONE.
#' @param variables list of report \code{variables} as defined by
#'   \code{\link{reportVariable}}, e.g. list(reportVariable(name = "VELOCITY",
#'   above = 3.0, precision = 4), reportVariable(name = "F-FACTOR", precision =
#'   4)). Default: \code{\link{defaultReportVariables}}
#' @export
setReportOptions <- function(
  configuration, 
  pagesize = 0, 
  file = "",    
  status = "NO",
  summary = "YES",
  messages = "YES",
  energy = "NO",
  nodes = "NONE",
  links = "NONE",
  variables = defaultReportVariables()
)
{
  section <- configuration$REPORT
  
  section <- setTimeParameterIfGiven(section, "PAGE", pagesize)
  
  if (file != "") {
    section <- setTimeParameterIfGiven(section, "FILE", file)
  }
  
  section <- setTimeParameterIfGiven(section, "STATUS", toupper(status))
  section <- setTimeParameterIfGiven(section, "SUMMARY", toupper(summary))
  section <- setTimeParameterIfGiven(section, "MESSAGES", toupper(messages))
  section <- setTimeParameterIfGiven(section, "ENERGY", toupper(energy))
  section <- setTimeParameterIfGiven(section, "NODES", nodes)
  section <- setTimeParameterIfGiven(section, "LINKS", links)
  
  for (variable in variables) {
    section <- setReportVariable(section, variable)
  }
  
  configuration$REPORT <- section
  
  configuration
}

# setTimeParameterIfGiven ------------------------------------------------------
setTimeParameterIfGiven <- function(section, name, value)
{
    name <- toupper(name)
    sectionRows.old <- section[toupper(section[[1]]) != name, 
        ]
    sectionRows.new <- data.frame(name, value, stringsAsFactors = FALSE)
    names(sectionRows.new) <- names(sectionRows.old)
    rbind(sectionRows.old, sectionRows.new)
}

# setReportVariable ------------------------------------------------------------
setReportVariable <- function(section, variable)
{
  affected <- section[[1]] == variable$name
  sectionRows.old <- section[!affected, ]
  sectionRows <- section[affected, ]
  sectionRows <- removeConcernedRows(sectionRows, "^(yes|no)")
  
  if (!is.null(variable$below) || !is.null(variable$above)) {
    sectionRows <- removeConcernedRows(sectionRows, "^(above|below)")
  }
  
  if (!is.null(variable$precision)) {
    sectionRows <- removeConcernedRows(sectionRows, "^precision")
  }
  
  newRows <- data.frame(name = variable$name, value = yesno(variable$yes))
  newRows <- addIfNotNull(newRows, variable$name, "BELOW", variable$below)
  newRows <- addIfNotNull(newRows, variable$name, "ABOVE", variable$above)
  newRows <- addIfNotNull(newRows, variable$name, "PRECISION", variable$precision)
  
  names(newRows) <- names(sectionRows)
  rbind(sectionRows.old, sectionRows, newRows)
}

# removeConcernedRows ----------------------------------------------------------
removeConcernedRows <- function(rows, pattern)
{
  indices <- grep(pattern, rows[[2]], ignore.case = TRUE)
  
  if (length(indices) > 0) {
    rows <- rows[-indices, ]
  }
  
  rows
}

# addIfNotNull -----------------------------------------------------------------
addIfNotNull <- function(rows, variableName, propertyName, value)
{
  if (!is.null(value)) {
    rows <- rbind(
      rows, 
      data.frame(
        name = variableName, 
        value = paste(propertyName, value), 
        stringsAsFactors = FALSE
      )
    )
  }
  
  rows
}

# defaultReportVariables -------------------------------------------------------

#' Default Report Variables
#' 
#' @return list of report variable definitions as returned by
#'   \code{\link{reportVariable}}
#' @export
defaultReportVariables <- function() 
{
  list(
    reportVariable(name = "Demand"),
    reportVariable(name = "Head"),
    reportVariable(name = "Pressure"),
    reportVariable(name = "Quality"),
    reportVariable(name = "Flow"),
    reportVariable(name = "Velocity"),
    reportVariable(name = "Headloss")
  )
}

# reportVariable ---------------------------------------------------------------

#' Report Variable Definition
#' 
#' Report variable definition for argument \emph{variables} of
#'   \code{\link{setReportOptions}}
#' 
#' @param name \code{name} of variable. Node variables that can be reported on include:
#'   "Elevation", "Demand", "Head", "Pressure", "Quality".
#'   Link variables include: "Length", "Diameter", "Flow", "Velocity",
#'   "Headloss", "LinkQuality", "LinkStatus", "Setting" (Roughness for pipes,
#'   speed for pumps,,  pressure/flow setting for valves), "Reaction" (reaction
#'   rate), "FFactor" (friction factor)
#' @param yes shall the variable be importet on? 
#' @param below if set to a non-NA value (default: NA) only values \code{below} the given value
#'   will be reported on
#' @param above if set to a non-NA value (default: NA) only values \code{above} the given value
#'   will be reported on  
#' @param precision the variable will be reported on with the given \code{precision} (number of
#'   decimal places). Default: 2
#' @export
reportVariable <- function(
  name = "Elevation", 
  yes = TRUE, 
  below = NA,   
  above = NA, 
  precision = 2
)
{
  nodeVariables <- c("Elevation", "Demand", "Head", "Pressure", "Quality")
  
  linkVariables <- c(
    "Length", "Diameter", "Flow", "Velocity", "Headloss", "LinkQuality", 
    "LinkStatus", "Setting", "Reaction", "FFactor"
  )
  
  variableNames <- c(nodeVariables, linkVariables)
  
  if (! name %in% variableNames) {
    stop("\nname must be one of:\n  ", paste(variableNames, collapse = "\n  "))
  }
  
  variableConfiguration <- list(name = name, yes = yes)
  
  if (! is.na(below)) {
    variableConfiguration$below <- below
  }
  
  if (! is.na(above)) {
    variableConfiguration$above <- above
  }
  
  if (precision != 2) {
    variableConfiguration$precision <- precision
  }
  
  variableConfiguration
}

# yesno ------------------------------------------------------------------------
yesno <- function(yesno)
{
  ifelse(yesno, "YES", "NO")
}
