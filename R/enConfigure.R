# setTimeParameter ---------------------------------------------------------
setTimeParameter <- function # set EPANET' time-related simulation parameters
### set EPANET' time-related simulation parameters. See EPANET reference for the
### meaning of the parameters
(
  configuration, 
  duration = "",
  hydraulic.timestep = "",
  quality.timestep = "",
  rule.timestep = "", 
  pattern.timestep = "", 
  pattern.start = "",
  report.timestep = "",
  report.start = "",
  start.clocktime = "",
  statistic = ""
)
{
  times <- configuration$TIMES
  
  times <- .setTimeParameterIfGiven(times, "DURATION", duration)
  times <- .setTimeParameterIfGiven(times, "HYDRAULIC TIMESTEP", hydraulic.timestep)
  times <- .setTimeParameterIfGiven(times, "QUALITY TIMESTEP", quality.timestep)
  times <- .setTimeParameterIfGiven(times, "RULE TIMESTEP", rule.timestep)
  times <- .setTimeParameterIfGiven(times, "PATTERN TIMESTEP", pattern.timestep)
  times <- .setTimeParameterIfGiven(times, "PATTERN START", pattern.start)
  times <- .setTimeParameterIfGiven(times, "REPORT TIMESTEP", report.timestep)
  times <- .setTimeParameterIfGiven(times, "REPORT START", report.start)
  times <- .setTimeParameterIfGiven(times, "STATISTIC", statistic)
  
  configuration$TIMES <- times
  
  configuration
}

# .setTimeParameterIfGiven -----------------------------------------------------
.setTimeParameterIfGiven <- function
(
  times, parameterName, parameterValue
)
{
  if (parameterValue != "") {
    
    index <- toupper(times[[1]]) == parameterName
    
    if (length(index) == 0) {
      index <- nrow(times) + 1
    }
    
    times[index, 2] <- parameterValue
  }
  times
}
