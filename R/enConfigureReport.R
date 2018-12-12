# setReportOptions ------------------------------------------------------------
setReportOptions <- function # set REPORT option in EPANET configuration
### set REPORT option in EPANET configuration
(
  configuration, 
  ### EPANET configuration, representing an EPANET input file, as returned by 
  ### \code{\link{readEpanetInputFile}}
  pagesize = 0, 
  ### pagesize sets the number of lines written per page of the output report.
  ### The default is 0, meaning that no line limit per page is in effect. 
  file = "",    
  ### file supplies the name of a file to which the output report will be
  ### written. If the file name contains spaces then it must be surrounded by
  ### double quotes. If not supplied then the Report file, as specified in the
  ### second parameter of the ENopen (or ENepanet) function will be used.
  status = "NO",
  ### status determines whether hydraulic status messages are written to the
  ### Report file. If YES is selected the messages will identify those network
  ### components that change status during each time step of the simulation. If
  ### FULL is selected, then convergence information will also be included from
  ### each trial of each hydraulic analysis. This level of detail is only useful
  ### for de-bugging networks that become hydraulically unbalanced. The default
  ### is NO.
  summary = "YES",
  ### summary determines whether a summary table of number of network components
  ### and key analysis options is generated. The default is YES.
  messages = "YES",
  ### messages determines whether error and warning messages generated during a
  ### hydraulic/water quality analysis are written to the Report file. The
  ### default is YES.
  energy = "NO",
  ### energy determines if a table reporting average energy usage and cost for
  ### each pump is provided. The default is NO.
  nodes = "NONE",
  ### nodes identifies which nodes will be reported on. You can either list
  ### individual node ID labels or use the keywords NONE or ALL. Additional
  ### NODES lines can be used to continue the list. The default is NONE.
  links = "NONE",
  ### links identifies which links will be reported on. You can either list
  ### individual link ID labels or use the keywords NONE or ALL. Additional
  ### LINKS lines can be used to continue the list. The default is NONE.
  variables = defaultReportVariables()
  ### list of report variables as defined by \code{\link{reportVariable}}, e.g.
  ### list(reportVariable(name = "VELOCITY", above = 3.0, precision = 4),
  ### reportVariable(name = "F-FACTOR", precision = 4)). Default:
  ### \code{\link{defaultReportVariables}}
)
{
  section <- configuration$REPORT
  
  section <- .setReportProperty(section, "PAGE", pagesize)
  if (file != "") {
    section <- .setReportProperty(section, "FILE", file)
  }
  section <- .setReportProperty(section, "STATUS", toupper(status))
  section <- .setReportProperty(section, "SUMMARY", toupper(summary))
  section <- .setReportProperty(section, "MESSAGES", toupper(messages))
  section <- .setReportProperty(section, "ENERGY", toupper(energy))
  section <- .setReportProperty(section, "NODES", nodes)
  section <- .setReportProperty(section, "LINKS", links)
  
  for (variable in variables) {
    section <- .setReportVariable(section, variable)
  }
  
  configuration$REPORT <- section
  
  configuration
}

# .setReportProperty -----------------------------------------------------------
.setReportProperty <- function
(
  section, name, value
)
{
  # exclude existing lines corresponding to the property named "name" and
  # append new lines
  name <- toupper(name)
  
  sectionRows.old <- section[toupper(section[[1]]) != name, ]
  sectionRows.new <- data.frame(name, value, stringsAsFactors = FALSE)
  names(sectionRows.new) <- names(sectionRows.old)
  
  rbind(sectionRows.old, sectionRows.new)
}

# .setReportVariable -----------------------------------------------------------
.setReportVariable <- function
(
  section, variable
)
{
  # exclude existing lines corresponding to the property named "name" and
  # append new lines
  affected <- section[[1]] == variable$name
  
  sectionRows.old <- section[! affected, ]
  sectionRows <- section[affected, ]
  
  # Remove concerned existing rows
  sectionRows <- .removeConcernedRows(sectionRows, "^(yes|no)")
  if (!is.null(variable$below) || !is.null(variable$above)) {
    sectionRows <- .removeConcernedRows(sectionRows, "^(above|below)")
  }
  if (!is.null(variable$precision)) {
    sectionRows <- .removeConcernedRows(sectionRows, "^precision")
  }
  
  newRows <- data.frame(name = variable$name, value = .yesno(variable$yes))
  newRows <- .addIfNotNull(newRows, variable$name, "BELOW", variable$below)
  newRows <- .addIfNotNull(newRows, variable$name, "ABOVE", variable$above)
  newRows <- .addIfNotNull(newRows, variable$name, "PRECISION", variable$precision)
  
  names(newRows) <- names(sectionRows)
  
  rbind(sectionRows.old, sectionRows, newRows)
}

# .removeConcernedRows ---------------------------------------------------------
.removeConcernedRows <- function(rows, pattern)
{
  indices <- grep(pattern, rows[[2]], ignore.case = TRUE)
  if (length(indices) > 0) {
    rows <- rows[-indices, ]
  }
  rows
}

# .addIfNotNull ----------------------------------------------------------------
.addIfNotNull <- function(rows, variableName, propertyName, value)
{
  if (!is.null(value)) {
    rows <- rbind(
      rows, 
      data.frame(
        name = variableName, 
        value = paste(propertyName, value), stringsAsFactors = FALSE)
    )
  }
  rows  
}

# defaultReportVariables -------------------------------------------------------
defaultReportVariables <- function # defaultReportVariables
### defaultReportVariables
(
) 
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
  ### list of report variable definitions as returned by
  ### \code{\link{reportVariable}}
}

# reportVariable ---------------------------------------------------------------
reportVariable <- function # report variable definition
### report variable definition for argument \emph{variables} of
### \code{\link{setReportOptions}}
(
  name = "Elevation", 
  ### name of variable. Node variables that can be reported on include:
  ### "Elevation", "Demand", "Head", "Pressure", "Quality".
  ### Link variables include: "Length", "Diameter", "Flow", "Velocity",
  ### "Headloss", "LinkQuality", "LinkStatus", "Setting" (Roughness for pipes,
  ### speed for pumps,,  pressure/flow setting for valves), "Reaction" (reaction
  ### rate), "FFactor" (friction factor)
  yes = TRUE, 
  ### shall the variable be importet on? 
  below = NA,   
  ### if set to a non-NA value (default: NA) only values below the given value
  ### will be reported on
  above = NA, 
  ### if set to a non-NA value (default: NA) only values above the given value
  ### will be reported on  
  precision = 2
  ### the variable will be reported on with the given precision (number of
  ### decimal places). Default: 2
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
  
  if (!is.na(below)) {
    variableConfiguration$below <- below
  }
  if (!is.na(above)) {
    variableConfiguration$above <- above
  }
  if (precision != 2) {
    variableConfiguration$precision <- precision
  }
  
  variableConfiguration
}

# .yesno -----------------------------------------------------------------------
.yesno <- function(yesno) {
  ifelse(yesno, "YES", "NO")
}
